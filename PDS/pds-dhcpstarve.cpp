#include <pcap.h>
#include <cstdlib>
#include <iostream>
#include "PcapUtil.h"
#include <getopt.h>
#include <ctime>
#include <cstring>
#include "Common.h"
#include <unistd.h>
#include <map>
#include <chrono>
#include <netinet/in.h>
#include <net/if.h>
#include <sys/ioctl.h>

#include "ClientMessage.h"

using namespace std;

int main(int argc, char *argv[])
{
	uint8_t mac[6];

	int ret = PcapUtil::MACAddress("wlp8s0", mac);
	for (auto a : mac)
		cout << hex << unsigned(a) << ":";

	if (argc != 3) {
		cerr << "invalid number of arguments" << endl;
		return EXIT_FAILURE;
	}
	string devName;
	int c;
	while ((c = getopt(argc, argv, "i:")) != -1) {
		switch (c){
		case 'i':
			devName = optarg;
			break;
		default:
			cerr << "invalid arguments" << endl;
			return EXIT_FAILURE;
		}
	}

	const set<string> allDevices = PcapUtil::allDevices();
	auto it = allDevices.find(devName);
	if (it == allDevices.end()) {
		string err;
		err += "unknown device name: ";
		err += devName;
		err += "\n";
		err += "all fond devices: ";

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
		return EXIT_FAILURE;
	}
	srand(time(nullptr));

	//std::map<uint32_t, DHCPMsgInfo> packets;

	pcap_t *handle;
	char errbuff[PCAP_ERRBUF_SIZE] = {0};
	handle = pcap_open_live(devName.c_str(), 512, 1, -1, errbuff);
	if (strlen(errbuff) > 0) {
		cerr << errbuff << endl;
		return -1;
	}

	struct bpf_program filter;
	char filter_exp[] = "port 68";
	bpf_u_int32 ip;
	if (pcap_compile(handle, &filter, filter_exp, 0, ip) == -1) {
		printf("Bad filter - %s\n", pcap_geterr(handle));
		return 2;
	}
	if (pcap_setfilter(handle, &filter) == -1) {
		printf("Error setting filter - %s\n", pcap_geterr(handle));
		return 2;
	}

	for (size_t j = 0; j < 2; j++) {

	vector<uint8_t> l2MAC = {{0xf8, 0xd1, 0x11, 0x07, 0xb9, 0x3e}}; //TODO nacitat z rozhrania
	vector<uint8_t> fakeMAC = {0x2c, 0xa4, 0x0d, 0x04, j, rand()};
	uint32_t transactionID = 0xa078944 + j + rand();

	DHCPDiscovery msg;
	msg.setEthMAC(l2MAC); // pc
	msg.setFakeClientMAC(fakeMAC);
	msg.setTransactionID(transactionID);

	pcap_sendpacket(handle, (const u_char *) &msg, sizeof(msg));

	//packets.emplace(make_pair(transactionID, DHCPMsgInfo{123,
	//	PcapUtil::timestamp(),PcapUtil::timestamp(),PcapUtil::timestamp(),1}));

	uint64_t startTime = PcapUtil::timestamp();

	while (true) {
		uint8_t *buf = new uint8_t[1024];

		struct pcap_pkthdr header;
		header.len = 0;
		buf = (uint8_t *) pcap_next(handle, &header);

		if (PcapUtil::timestamp() - startTime > 1000)
			break;

		if (header.len == 0)
			continue;

		DHCPMessage *dhcpMsg = DHCPMessage::fromRaw(buf, header.caplen);
//		cout << dhcpMsg->toString("\n");

		//cout << sizeof(DHCPMessage) << endl;
		//cout << header.len << endl;
		uint8_t *options = buf + sizeof(DHCPMessage);
		PacketInfo info;
		for (size_t m = 0; m < 64;m++) {
			//cout << hex << unsigned(*(options)) << endl;

			switch (*options) {
			case 0x35: // DHCP Message type
				options++; // length
				options++; // data
				info.dhcpMessageType = *options;
				options++;
				break;

			case 0x36: // DHCP server identifier
				options++; // move to length
				options++; //move to value

				for (size_t p = 0; p < 4; p++) {
					info.dhcpServerIdentifier[p] = *(options + p);
					//cout << ":" << dec << unsigned(*(options + p)) << endl;
				}

				options = options + 4;
			default:
				options++;
				uint8_t length = *options;
				options++;
				options += length;
			}
		}

		//cout << info.toString("\n") << endl;

		//auto it = packets.find(dhcpMsg->dhcpHeader.transactionID);
	//	if (it ==  packets.end())
		//	cerr << "koniec" << endl;
		//else {
			if (info.dhcpMessageType == 1)
				cout << "discoery" << endl;
			else if (info.dhcpMessageType == 2) {
				cout << "offer" << endl;

				//cout << dhcpMsg->toString("\n") << endl;

				DHCPRequest wawa;
				wawa.setFakeClientMAC(fakeMAC);
				wawa.setTransactionID(htonl(htonl(transactionID)));

				wawa.setEthMAC(l2MAC); // ntb wifi
				wawa.setFakeClientMAC(fakeMAC);
				//wawa.dhcpData.op1payload = 0x03;

				for (size_t p = 0; p < 4; p++) {
					wawa.op1payload[p] = dhcpMsg->dhcpHeader.yourIPAddr[p];
					wawa.op2payload[p] = dhcpMsg->dhcpHeader.serverIPAddr[p];

					cout << dec << unsigned(dhcpMsg->dhcpHeader.serverIPAddr[p]) << endl;
				}

				pcap_sendpacket(handle, (const u_char *) &wawa, sizeof(wawa));
				cout << "send" << endl;


			}
			else if (info.dhcpMessageType == 3) {
				cout << "ack" << endl;
				break;
			}
			else {
				cout << "unknown" << endl;
			}
		//}
	}
	}


	pcap_close(handle);

	return EXIT_SUCCESS;
}
