#include <pcap.h>
#include <cstdlib>
#include <iostream>
#include "PcapUtil.h"
#include <getopt.h>
#include <ctime>
#include <cstring>
#include "DHCPMsg.h"

using namespace std;

int main(int argc, char *argv[])
{
	string devName;
	int c;
	while ((c = getopt(argc, argv, "i:")) != -1) {
		switch (c){
		case 'i':
			devName = optarg;
			break;
		default:
			cerr << "invalid number of arguments" << endl;
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

	cout << sizeof(DHCPMessage)<< endl;
	Tmp tt;
	{

		for (int i = 0; i < 6; i++) {
			tt.dstMac[i] = 0xff;
			tt.srcMac[i] = 0;
			tt.srcAddr[i] = 0;
			tt.dstAddr[i] = 0xff;
		}
		tt.ipType = 0x0008;
		tt.version = 0x45;
		tt.servField = 0;
		tt.totLength = 0x2001;
		tt.identification = 0;

		tt.hdrFlags = 0x40;
		tt.offset = 0;
		tt.ttl = 0;
		tt.protocol = 0x11;
		tt.ipSum = 0;

		tt.srcPort = 0x4400;
		tt.dstPort = 0x4300;

		tt.len = 0x0c01;
		tt.udpCheckSum = 0;


	}

	pcap_t *handle;
	char errbuff[PCAP_ERRBUF_SIZE] = {0};
	handle = pcap_open_live(devName.c_str(), 512, 1, -1, errbuff);
	if (strlen(errbuff) > 0) {
		cerr << errbuff << endl;
		return -1;
	}


	DHCPMessage msg;

	struct pcap_pkthdr pcapHeader;

	Tmp wawa;
	cout << "weee" << endl;
	cout << pcap_sendpacket(handle, (const u_char *) &msg, sizeof(msg)) << endl;
	//cout << sizeof(msg) << endl;

//	TEthHeader header = {{0,1,2,3,4,5}, {6,7,8,9,10,11}, 6};

//	cout << header.toString("\n") << endl;

	srand(time(nullptr));
	return EXIT_SUCCESS;
}
