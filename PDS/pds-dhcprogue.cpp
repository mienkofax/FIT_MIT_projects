#include <string>
#include <getopt.h>
#include <iostream>
#include <cstring>
#include <netinet/in.h>
#include "Util.h"
#include "DHCPMsg.h"

#include <stdio.h>
#include <time.h>
#include <unistd.h>
#include <ctype.h>
#include <string.h>
#include <ifaddrs.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <signal.h>
#include <stdbool.h>
#include <stdlib.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <net/ethernet.h>
#include <linux/if_packet.h>
#include <time.h>

#include <pcap.h>
#include <vector>

using namespace std;

static const string helpMessage =
	"R(Help message)";

struct TIPv4 {
	int raw[4];

	static TIPv4 fromString(const string &ip, char separator = '.')
	{
		TIPv4 ipv4 = {0};
		size_t separatorCount = 0;
		string value;
		size_t index = 0;

		for (size_t i = 0; i < ip.size(); i++) {
			if (ip.at(i) != separator) {
				value.push_back(ip.at(i));

				if (i != ip.size()-1)
					continue;
			}

			separatorCount++;
			ipv4.raw[index++] = stoi(value, nullptr, 10);
			value.erase();
		}

		return ipv4;
	}

	string toString(const string &separator = "\n") const
	{
		string repr;

		for (auto it : raw)
			repr += to_string(it) + ".";
		repr.pop_back();
		repr += separator;

		return repr;
	}

	bool operator ==(const TIPv4 &ip) const
	{
		for (size_t i = 0; i < 4; i++) {
			if (raw[i] != ip.raw[i])
				return false;
		}

		return true;
	}

	bool operator !=(const TIPv4 &ip) const
	{
		for (size_t i = 0; i < 4; i++) {
			if (raw[i] != ip.raw[i])
				return true;
		}

		return false;
	}

	bool operator <(const TIPv4 &ip) const
	{
		for (size_t i = 0; i < 4; i++) {
			if (raw[i] < ip.raw[i])
				return true;
		}

		return false;
	}

	bool operator >(const TIPv4 &ip) const
	{
		for (size_t i = 0; i < 4; i++) {
			if (raw[i] > ip.raw[i])
				return true;
		}

		return false;
	}
};

struct TPool {
	TIPv4 start;
	TIPv4 end;

	static TPool fromString(const std::string &pool, char separator = '-')
	{
		TPool tPool;
		const size_t index = pool.find_first_of('-');
		const string first = pool.substr(0, index);

		tPool.start = TIPv4::fromString(first);
		tPool.end = TIPv4::fromString(
			pool.substr(first.size() + 1, -1));

		return tPool;
	}

	size_t size() const
	{
		uint64_t size = 0;

		for (size_t i = 0; i < 4; i++) {
			const int range = end.raw[i] - start.raw[i];

			if (range <= 0)
				continue;

			size += range << ((3 - i) * 8);
		}

		if (size > 0)
			size++;

		return size;
	}

	string toString (const std::string &separator = "\n") const
	{
		string repr;

		repr += "pool: \n";
		repr += "\tstart: " + start.toString("") + "\n";
		repr += "\tend  : " + end.toString("") + "\n";
		repr += "\tsize : " + to_string(size());
		repr += separator;

		return repr;
	}

};

struct TParams {
	string interface;
	TPool pool;
	TIPv4 gateway;
	TIPv4 dnsServer;
	std::string domain;
	uint64_t leaseTime;

	std::string toString(const std::string &separator = "\n") const
	{
		string repr;

		repr += "interface: ";
		repr += interface;
		repr += separator;

		repr += pool.toString();

		repr += "gateway: ";
		repr += gateway.toString();

		repr += "DNS Server: ";
		repr += dnsServer.toString();

		repr += "domain: ";
		repr += domain;
		repr += separator;

		repr += "lease time: ";
		repr += to_string(leaseTime);
		repr += separator;

		return repr;
	}
};


int extractArguments(int argc, char *argv[], TParams *t)
{
	int c;
	while ((c = getopt(argc, argv, "i:p:g:n:d:l:")) != -1) {
		switch (c){
		case 'i':
			t->interface = optarg;
			break;

		case 'p':
			t->pool = TPool::fromString(optarg);
			break;

		case 'g':
			t->gateway = TIPv4::fromString(optarg);
			break;

		case 'n':
			t->dnsServer = TIPv4::fromString(optarg);
			break;

		case 'd':
			t->domain = optarg;
			break;

		case 'l':
			t->leaseTime = stoi(optarg, nullptr, 10);
			break;

		default:
			cerr << "invalid arguments" << endl;
			return -1;
		}
	}

	return 0;
}

bool checkExistInterface(const std::string &interface)
{
	set<string> interfaces = Util::allDevices();

	auto it = interfaces.find(interface);
	return it != interfaces.end();
}

// http://www.microhowto.info/howto/calculate_an_internet_protocol_checksum_in_c.html
uint16_t ip_checksum(void* vdata,size_t length) {
	// Cast the data pointer to one that can be indexed.
	char* data=(char*)vdata;

	// Initialise the accumulator.
	uint32_t acc=0xffff;
	size_t i;

	// Handle complete 16-bit blocks.
	for (i=0;i+1<length;i+=2) {
		uint16_t word;
		memcpy(&word,data+i,2);
		acc+=ntohs(word);
		if (acc>0xffff) {
			acc-=0xffff;
		}
	}

	// Handle any partial block at the end of the data.
	if (length&1) {
		uint16_t word=0;
		memcpy(&word,data+length-1,1);
		acc+=ntohs(word);
		if (acc>0xffff) {
			acc-=0xffff;
		}
	}

	// Return the checksum in network byte order.
	return htons(~acc);
}

struct TCon {
	int listenSock;
	pcap_t *sendSock;
	struct sockaddr_in addrIn;
	struct sockaddr_in addrOut;

	~TCon()
	{
		//close(listenSock);
	}

	int init(const std::string &interface)
	{
		int ret;

		listenSock = socket(PF_INET, SOCK_DGRAM, 0);
		if (listenSock <= 0 ) {
			cerr << "problem pri vytvoreni soketu" << endl;
			return -1;
		}

		int option = 1;
		ret = setsockopt(
			listenSock,
			SOL_SOCKET,
			SO_REUSEADDR,
			&option,
			sizeof(option)
		);

		if (ret < 0) {
			cerr << "problem pri zmene interface" << endl;
			close(listenSock);
			return -2;
		}

		ret = setsockopt(
			listenSock,
			SOL_SOCKET,
			SO_BINDTODEVICE,
			interface.c_str(),
			interface.length() + 1
		);

		if (ret < 0) {
			cerr << "problem s nastavenim rozhrania, ste root?" << endl;
			close(listenSock);
			return -3;
		}

		memset(&addrIn, 0, sizeof(addrIn));
		addrIn.sin_family = AF_INET;
		addrIn.sin_addr.s_addr = htonl(INADDR_ANY);
		addrIn.sin_port = htons(67);

		ret = bind(
			listenSock,
			(struct sockaddr *) &addrIn,
			sizeof(addrIn)
		);

		if (ret < 0) {
			cerr << "chybny bind" << endl;
			close(listenSock);
			return -4;
		}

		pcap_t *handle;
		char *errPcap;

		sendSock = pcap_open_live(interface.c_str(), 512, -1, -1, errPcap);

		memset(&addrOut, 0, sizeof(addrOut));

	}
};

static const std::vector<uint8_t> testVec = {
	0x01, 0x01, 0x06, 0x00, 0x8c, 0xd0, 0x94, 0x73,
	0x00, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x08, 0x00, 0x27, 0x49,
	0x03, 0xa4, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x63, 0x82, 0x53, 0x63,
	0x35, 0x01, 0x01, 0x0c, 0x07, 0x69, 0x73, 0x61,
	0x32, 0x30, 0x31, 0x35, 0x37, 0x12, 0x01, 0x1c,
	0x02, 0x03, 0x0f, 0x06, 0x77, 0x0c, 0x2c, 0x2f,
	0x1a, 0x79, 0x2a, 0x79, 0xf9, 0x21, 0xfc, 0x2a,
	0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00
};

/*
 *
		//promenna pro ziskani adresy zarizeni a portu
	struct sockaddr_in sendAddrPort;
	memset(&sendAddrPort, 0, sizeof(sendAddrPort));
	int sendAddrLen = sizeof(sendAddrPort);
 */
#include <poll.h>

void processPacket(const vector<uint8_t> &data, TCon *t)
{
	uint8_t buffer[data.size()];
	for (size_t i = 0; i < data.size(); i++) {		buffer[i] = data[i];
//cout << hex << unsigned(data[i]) << " ";
	}

	TDHCPHeader *header = (TDHCPHeader *) buffer;
	TDHCPData *tdhcpData = (TDHCPData *) (buffer + sizeof(TDHCPHeader));
	//cout << header->toString("\n");
	cout << "--------" << endl;

	uint8_t *options = buffer + sizeof(TDHCPHeader)+ sizeof(TDHCPData);
	uint8_t type = 0;
	for (size_t m = 0; m < (data.size() - sizeof(TDHCPHeader) - sizeof(TDHCPData)) ;m++) {
		//	cout << hex << unsigned(*(options)) << " ";
//options++;
//continue;

		switch (*options) {
		case 0x35: // DHCP Message type
			options++; // length
			options++; // data
			//cout << "0x35" <<  *options << endl;
			type = *options;
			m = 100000;
			options++;
			cout << unsigned(*options) << endl;
			options++;
			cout << unsigned(*options) << endl;
			break;


		case 0xff:
			m = 100000;
			break;
		default: {
			options++;
			uint8_t length = *options;
			options++;
			options += length;
		}
		}
	}

	cout << "\nincome type: " << unsigned(type) << endl;
	bool static offer = false;

	if (type == 1 ) {
		offer = true;
		DHCPServerOffer offer;
		offer.setEthMAC({0x08, 0x00, 0x27, 0x3d, 0x98, 0x95});
		offer.udpHeader.udpDestinationPort = 0x4400; // 68
		offer.udpHeader.udpSourcePort = 0x4300; // 67
		offer.setTransactionID(header->transactionID);

		offer.dhcpHeader.yourIPAddr[0] = 192;
		offer.dhcpHeader.yourIPAddr[1] = 168;
		offer.dhcpHeader.yourIPAddr[2] = 1;
		offer.dhcpHeader.yourIPAddr[3] = 11;

		offer.dhcpHeader.serverIPAddr[0] = 192;
		offer.dhcpHeader.serverIPAddr[1] = 168;
		offer.dhcpHeader.serverIPAddr[2] = 1;
		offer.dhcpHeader.serverIPAddr[3] = 123;

		offer.op36payload[0] = 192;
		offer.op36payload[1] = 168;
		offer.op36payload[2] = 1;
		offer.op36payload[3] = 123;

		offer.op01payload[0] = 255;
		offer.op01payload[1] = 255;
		offer.op01payload[2] = 255;
		offer.op01payload[3] = 0;

		offer.op1cpayload[0] = 192;
		offer.op1cpayload[1] = 168;
		offer.op1cpayload[2] = 1;
		offer.op1cpayload[3] = 255;

		offer.op03payload[0] = 192;
		offer.op03payload[1] = 168;
		offer.op03payload[2] = 1;
		offer.op03payload[3] = 1;

		offer.op06payload[0] = 192;
		offer.op06payload[1] = 168;
		offer.op06payload[2] = 1;
		offer.op06payload[3] = 4;

		offer.op06payload[4] = 8;
		offer.op06payload[5] = 8;
		offer.op06payload[6] = 8;
		offer.op06payload[7] = 8;

		offer.ipHeader.headerChecksum = 0xb639;

		for (size_t i = 0; i < 6; i++)
			offer.ethHeader.dstMACAddr[i] = tdhcpData->clientHardwareAddress[i];

		for (size_t i = 0; i < 16; i++)
			offer.dhcpData.clientHardwareAddress[i] = tdhcpData->clientHardwareAddress[i];


		//cout << offer.toString("\n") << endl;

		cout << "send offer" << endl;

		pcap_sendpacket(t->sendSock, (const u_char *) &offer, sizeof(offer));
	}
	else if (type == 3) {
		cout << "request" << endl;
		DHCPServerOffer offer;
		offer.setEthMAC({0x08, 0x00, 0x27, 0x3d, 0x98, 0x95});
		offer.udpHeader.udpDestinationPort = 0x4400; // 68
		offer.udpHeader.udpSourcePort = 0x4300; // 67
		offer.setTransactionID(header->transactionID);

		offer.dhcpHeader.yourIPAddr[0] = 192;
		offer.dhcpHeader.yourIPAddr[1] = 168;
		offer.dhcpHeader.yourIPAddr[2] = 1;
		offer.dhcpHeader.yourIPAddr[3] = 11;

		offer.dhcpHeader.serverIPAddr[0] = 192;
		offer.dhcpHeader.serverIPAddr[1] = 168;
		offer.dhcpHeader.serverIPAddr[2] = 1;
		offer.dhcpHeader.serverIPAddr[3] = 123;

		offer.op36payload[0] = 192;
		offer.op36payload[1] = 168;
		offer.op36payload[2] = 1;
		offer.op36payload[3] = 123;

		offer.op01payload[0] = 255;
		offer.op01payload[1] = 255;
		offer.op01payload[2] = 255;
		offer.op01payload[3] = 0;

		offer.op1cpayload[0] = 192;
		offer.op1cpayload[1] = 168;
		offer.op1cpayload[2] = 1;
		offer.op1cpayload[3] = 255;

		offer.op03payload[0] = 192;
		offer.op03payload[1] = 168;
		offer.op03payload[2] = 1;
		offer.op03payload[3] = 1;

		offer.op06payload[0] = 192;
		offer.op06payload[1] = 168;
		offer.op06payload[2] = 1;
		offer.op06payload[3] = 4;

		offer.op06payload[4] = 8;
		offer.op06payload[5] = 8;
		offer.op06payload[6] = 8;
		offer.op06payload[7] = 8;

		offer.op35payload = 0x05;

		offer.ipHeader.headerChecksum = 0xb639;

		for (size_t i = 0; i < 6; i++)
			offer.ethHeader.dstMACAddr[i] = tdhcpData->clientHardwareAddress[i];

		for (size_t i = 0; i < 16; i++)
			offer.dhcpData.clientHardwareAddress[i] = tdhcpData->clientHardwareAddress[i];


		cout << offer.toString("\n") << endl;

		cout << "send" << endl;

		pcap_sendpacket(t->sendSock, (const u_char *) &offer, sizeof(offer));
		cout << "send request" << endl;
//exit(1);
	}
	else  {
		cout << "unknown" << endl;

	}

	//}
}

int main(int argc, char *argv[])
{/*
	if (argc != 13) {
		cerr << "invalid number of arguments" << endl;
		return EXIT_FAILURE;
	}

	TParams params;
	if (extractArguments(argc, argv, &params) == -1) {
		cerr << "invalid arguments" << endl;
		cerr << helpMessage << endl;
		return EXIT_FAILURE;
	}

	if (!checkExistInterface(params.interface)) {
		cerr << "spatny interface" << endl;
		return EXIT_FAILURE;
	}

	// TODO semantiku zadanych prikazov

	cout << params.toString() << endl;
*/
	TCon con;
	con.init("eth0");

	struct pollfd pfd[1];
	pfd[0].fd = con.listenSock;
	pfd[0].events = POLLIN | POLLERR | POLLRDBAND;
	pfd[0].revents = 0;

	int milliseconds = 1000 * 100;

	while (true) {
		int ret;
		if ((ret = poll(pfd, 1, milliseconds)) < 0) {
			cerr << "pool: " << errno << endl;
			close(con.listenSock);
			return EXIT_FAILURE;
		}

		if (ret == 0) {
			cerr << "timeout error" << endl;
			return -4;
		}

		if (pfd[0].revents & POLLERR) {
			cerr << "port seems to be closed" << endl;
			return -5;
		}

		if (pfd[0].revents & (POLLRDBAND | POLLIN)) {
			/*int size;
			uint8_t buf[1024] = {0};
sleep(1);
			size = read(con.listenSock, buf, sizeof(buf)-1);
				buf[ret] = 0x00;




std::vector<uint8_t> mnau;
cout << "size: " << ret << endl;
for (size_t i = 0; i < ret; i++) {
	mnau.push_back(buf[i]);
	cout << Util::intToHex(buf[i], "0x") << " "<< endl;
}
cout << "cout wawa" << endl;
processPacket(mnau, &con);
break;*/

			int ret;
			uint8_t buf[1024] = {0};
			std::vector<uint8_t> mnau;
			while ((ret = read(con.listenSock, buf, sizeof(buf)-1)) > 0) {
				buf[ret] = 0x00;

				cout << "\nsize: " << ret << endl;
				for (size_t i = 0; i < ret; i++) {
					//	cout << Util::intToHex(buf[i], "0x") << " ";
					mnau.push_back(buf[i]);

				}


				for (size_t m = 0; m < 1024; m++)
					buf[m] = 0;

				cout << endl;
				processPacket(mnau, &con);
				mnau.clear();

			}
		}
	}

/*
int ret;
	uint8_t buf[1024] = {0};
	while ((ret = read(con.listenSock, buf, sizeof(buf)-1)) > 0) {
			buf[ret] = 0x00;

		cout << "size: " << ret << endl;
		for (size_t i = 0; i < ret; i++)
			cout << Util::intToHex(buf[i], "0x") << " ";


	}

*/
}
