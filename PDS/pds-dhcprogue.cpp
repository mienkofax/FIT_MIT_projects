#include <string>
#include <getopt.h>
#include <iostream>
#include <cstring>
#include <netinet/in.h>
#include "Util.h"
#include "ServerMessage.h"

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

#include <poll.h>

using namespace std;

static const string helpMessage =
	"R(Help message)";

/**
 * IP Pool.
 */
struct TPool {
	TIPv4 start;
	TIPv4 end;

	static TPool fromString(
		const string &pool, char separator = '-')
	{
		TPool tPool = {0};
		const size_t index = pool.find_first_of('-');
		const string first = pool.substr(0, index);

		tPool.start = TIPv4::fromString(first);
		tPool.end = TIPv4::fromString(
			pool.substr(first.size() + 1, -1));

		return tPool;
	}

	/**
	 * Velkost IP poolu.
	 */
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

	string toString (
		const string &separator = "\n") const
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

/**
 * Struktura zo vstupnymi parametrami, ktore
 * su prevedene do internej podoby.
 */
struct TParams {
	string interface;
	TPool pool;
	TIPv4 gateway;
	TIPv4 dnsServer;
	string domain;
	uint64_t leaseTime;

	string toString(
		const string &separator = "\n") const
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

/**
 * Rozparsovanie argumentov zo vstupu a overenie,
 * ci boli zadane vsetky potrebne argumenty.
 */
 // Todo overit duplicitu argumentov a ich semantiku
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

/**
 * Overenie ci zadane rozhranie existuje.
 */
int checkExistInterface(const string &interface)
{
	set<string> allDevices;
	if (Util::allDevices(allDevices) < 0)
		return -1;

	auto it = allDevices.find(interface);
	if (it == allDevices.end()) {
		string err;
		err += "unknown device name: ";
		err += interface;
		err += "\n";
		err += "all found devices: ";

		if (allDevices.size() > 1) {
			for (const auto &dev : allDevices)
				err += dev + ", ";

			// remove last two chars
			err.pop_back();
			err.pop_back();
		}
		else {
			err += "ziadne zariadenie nebolo najdete";
		}

		cerr << err << endl;
		return -1;
	}

	return 0;
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

/**
 * Struktura obsahujuca file descriptory na sokety, pripadne
 * na handler pre posielanie udajov pomocou pcap kniznice.
 */
struct TCon {
	int listenSock;
	pcap_t *sendSock;
	struct sockaddr_in addrIn;

	int init(const string &interface)
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
		char errBuffer[PCAP_ERRBUF_SIZE] = {0};

		// otvorenie pcap handleru
		sendSock = pcap_open_live(
			interface.c_str(),  // device
			512,                // snapLen
			1,                  // promisc
			1000,               // to_ms
			errBuffer           // error buffer
		);
	}
};


/**
 * Citanie zo vstupneho soketu bez cakania.
 */
int directRead(TCon *con, vector<uint8_t> &bufferVec)
{
	uint8_t buf[1024] = {0};
	int ret = 0;

	ret = (int) read(con->listenSock, buf, sizeof(buf) - 1);

	for (size_t i = 0; i < ret; i++) {
		bufferVec.push_back(buf[i]);
		cout << unsigned(buf[i]) << " ";
	}
	cout << endl;

	return ret;
}

/**
 * Citanie zo vstupneho soketu s timeoutom, po uplynuti
 * casu sa vrati -1. Ak sa prijmu nejake data pouzije
 * pre ich nacitanie directRead(). Cakanie je ms.
 */
int poolRead(
	TCon *con, vector<uint8_t> &bufferVec, int timeout)
{
	struct pollfd pfd[1];
	pfd[0].fd = con->listenSock;
	pfd[0].events = POLLIN | POLLERR | POLLRDBAND;
	pfd[0].revents = 0;

	int ret;
	if ((ret = poll(pfd, 1, timeout)) < 0) {
		cerr << "pool: " << errno << endl;
		close(con->listenSock);
		return -1;
	}

	if (ret == 0) {
		cerr << "timeout error" << endl;
		return -4;
	}

	if (pfd[0].revents & POLLERR) {
		cerr << "port seems to be closed" << endl;
		return -2;
	}

	if (pfd[0].revents & (POLLRDBAND | POLLIN)) {
		cout << "read\n";
		return directRead(con, bufferVec);
	}

	return -1;
}

bool m_stop = false;
void signalHandler(int signum)
{
	cerr << "koniec aplikacie" << endl;
	m_stop = true;
}


/**
 * Zistenie o aky typ dhcp spravy sa jedna a o
 * identifikaciu servera.
 */
void extractOptions(
	DHCPServerInfo &info, uint8_t *options, size_t size)
{
	for (size_t m = 0; m < size; m++) {
		switch (*options) {
		case 0x35:     // DHCP Message type
			options++; // length
			options++; // data
			info.messageType = *options;
			options++;
			break;

		case 0x36:     // DHCP server identifier
			options++; // move to length
			options++; //move to value

			for (size_t p = 0; p < 4; p++)
				info.serverIdentifier[p] = *(options + p);

			options = options + 4;

		default:
			options++;
			uint8_t length = *options;
			options++;
			options += length;
		}
	}
}

void extractPacketInformation(
	uint8_t *buffer,
	DHCPServerInfo &info,
	size_t size
)
{
	TDHCPHeader *header = (TDHCPHeader *) buffer;
	TDHCPData *tdhcpData = (TDHCPData *) (buffer + sizeof(TDHCPHeader));

	extractOptions(
		info,
		buffer + sizeof(TDHCPHeader) + sizeof(TDHCPData),
		size - sizeof(TDHCPHeader) - sizeof(TDHCPData)
	);

	info.transactionID = header->transactionID;

	for (size_t i = 0; i < 6; i++)
		info.dstMACAddress[i] = tdhcpData->clientHardwareAddress[i];

	for (size_t i = 0; i < 16; i++)
		info.clientHWAddress[i] = tdhcpData->clientHardwareAddress[i];
}

void processPacket(const vector<uint8_t> &data, TCon *t)
{
	uint8_t buffer[data.size()];
	for (size_t i = 0; i < data.size(); i++)
		buffer[i] = data[i];

	DHCPServerInfo info;
	extractPacketInformation(
		buffer,
		info,
		data.size()
	);

	cout << info.toString("\n") << endl;

	uint8_t type = info.messageType;
		if (type == 1 ) {
		DHCPServerOffer offer;
		offer.setEthMAC({0x08, 0x00, 0x27, 0x3d, 0x98, 0x95});
		offer.udpHeader.udpDestinationPort = 0x4400; // 68
		offer.udpHeader.udpSourcePort = 0x4300; // 67
		offer.setTransactionID(info.transactionID);

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
			offer.ethHeader.dstMACAddr[i] = info.clientHWAddress[i];

		for (size_t i = 0; i < 16; i++)
			offer.dhcpData.clientHardwareAddress[i] = info.clientHWAddress[i];


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
		offer.setTransactionID(info.transactionID);

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
				offer.ethHeader.dstMACAddr[i] = info.clientHWAddress[i];

			for (size_t i = 0; i < 16; i++)
				offer.dhcpData.clientHardwareAddress[i] = info.clientHWAddress[i];


			cout << offer.toString("\n") << endl;

		cout << "send" << endl;

		pcap_sendpacket(t->sendSock, (const u_char *) &offer, sizeof(offer));
		cout << "send request" << endl;
	}
	else {
		cout << "unknown" << endl;

	}

	//}
}

int main(int argc, char *argv[])
{
	signal(SIGINT, signalHandler);
	/*
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

	cout << params.toString() << endl;
*/
	TCon con;
	con.init("wlp8s0");

	vector<uint8_t> vec;
	const int milliseconds = 10 * 1000; // 10 s
	while (!m_stop) {
		poolRead(&con, vec, milliseconds);

		processPacket(vec, &con);
	}
}
