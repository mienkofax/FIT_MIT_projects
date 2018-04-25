#include <pcap.h>
#include <cstdlib>
#include <iostream>
#include "PcapUtil.h"
#include <getopt.h>
#include <ctime>
#include <cstring>
#include "DHCPMsg.h"

using namespace std;

void my_packet_handler(
	u_char *args,
	const struct pcap_pkthdr* header,
	const u_char* packet
);

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

	DHCPMessage msg;
	msg.setEthMAC({0x1c, 0x1b, 0x0d, 0x04, 0x7d, 0x50});
	msg.setFakeClientMAC({0x2c, 0x1b, 0x0d, 0x04, 0x7d, 0x50});
	msg.setTransactionID(0x78945);

	auto *buffer = new uint8_t[1024];
	size_t velkost = msg.raw(buffer);
	pcap_sendpacket(handle, (const u_char *) buffer, velkost);


	pcap_loop(handle, 10, my_packet_handler, NULL);
	pcap_close(handle);

	return EXIT_SUCCESS;
}

void my_packet_handler(
	u_char *args,
	const struct pcap_pkthdr* header,
	const u_char* packet
)
{
	/*const uint8_t *buf = (const uint8_t *) packet;

	for (int k = 0; k < header->caplen; k++)
		cout << PcapUtil::intToHex(buf[k], "0x") << " ";*/

	TEthHeader *h1 = (TEthHeader *) packet;
	cout << h1->toString("\n") << endl;

	TIP4Header *h2 = (TIP4Header *) (packet + h1->raw().size());
	cout << h2->toString("\n") << endl;

	TUDPHeader *h3 = (TUDPHeader *) (packet + h1->raw().size() + h2->raw().size());
	cout << h3->toString("\n") << endl;

	TDHCPHeader *h4 = (TDHCPHeader *) (packet + h1->raw().size() + h2->raw().size() + h3->raw().size());
	cout << h4->toString("\n") << endl;

	if (h4->opCode == 2) {
		h4->opCode = 3;

		/*setEthMAC({0x1c, 0x1b, 0x0d, 0x04, 0x7d, 0x50});
		msg.setFakeClientMAC({0x2c, 0x1b, 0x0d, 0x04, 0x7d, 0x50});
		msg.setTransactionID(0x78945);*/

	}



	static int i = 0;
	cout << i << "wawa" << endl;

	if (i == 5)
		pcap_breakloop((pcap_t*) args);

	i++;
}
