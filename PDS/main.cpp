#include <pcap.h>
#include <cstdlib>
#include <iostream>
#include "PcapUtil.h"
#include <getopt.h>
#include <ctime>
#include <cstring>
#include "DHCPMsg.h"
#include <unistd.h>
#include <map>
#include <chrono>
#include <netinet/in.h>

using namespace std;

int main(int argc, char *argv[])
{
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

	std::map<uint32_t, DHCPMsgInfo> packets;

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

	vector<uint8_t> l2MAC = {0x68, 0x5d, 0x43, 0x2b, 0xc5, 0x37};
	vector<uint8_t> fakeMAC = {0x2c, j, 0x0d, 0x04, 0x75, 0x33};
	uint32_t transactionID = 0xa078944 + j;

	DHCPMessage msg;
	//msg.setEthMAC({0x1c, 0x1b, 0x0d, 0x04, 0x7d, 0x50}); // pc
	msg.setEthMAC(l2MAC); // ntb wifi
	msg.setFakeClientMAC(fakeMAC);
	msg.setTransactionID(transactionID);

	pcap_sendpacket(handle, (const u_char *) &msg, sizeof(msg));

	packets.emplace(make_pair(transactionID, DHCPMsgInfo{123,
		PcapUtil::timestamp(),PcapUtil::timestamp(),PcapUtil::timestamp(),1}));

	uint64_t startTime = PcapUtil::timestamp();

	while (true) {
		uint8_t *buf = new uint8_t[1024];

		struct pcap_pkthdr header;
		header.len = 0;
		buf = (uint8_t *) pcap_next(handle, &header);

		if (PcapUtil::timestamp() - startTime > 3000)
			break;

		if (header.len == 0)
			continue;

		DHCPMessage *dhcpMsg = DHCPMessage::fromRaw(buf, header.caplen);
		dhcpMsg->toString("\n");
		auto it = packets.find(dhcpMsg->dhcpHeader.transactionID);
		if (it ==  packets.end())
			cerr << "koniec" << endl;
		else {
			if (dhcpMsg->dhcpHeader.opCode == 1)
				cout << "discoery" << endl;
			else if (dhcpMsg->dhcpHeader.opCode == 2) {
				cout << "offer" << endl;

				cout << dhcpMsg->toString("\n") << endl;

				DHCPMessage wawa;
				wawa.setFakeClientMAC(fakeMAC);
				wawa.setTransactionID(htonl(htonl(transactionID)+1));

				wawa.setEthMAC(l2MAC); // ntb wifi
				wawa.setFakeClientMAC(fakeMAC);
				wawa.dhcpData.op1payload = 0x03;

				for (size_t i = 0; i < 4; i++) {
					wawa.dhcpData.op3payload[i] = dhcpMsg->dhcpHeader.yourIPAddr[i];
					wawa.dhcpData.op5payload[i] = dhcpMsg->dhcpHeader.serverIPAddr[i];
				}

				pcap_sendpacket(handle, (const u_char *) &wawa, sizeof(wawa));
				cout << "send" << endl;
				exit(1);

			} else
				cout << "unknown" << endl;
		}
	}
	}


	pcap_close(handle);

	return EXIT_SUCCESS;
}
