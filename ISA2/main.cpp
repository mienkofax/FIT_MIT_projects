/**
 * @file main.cpp
 * @Author Peter Tisovčík <xtisov00@stud.fit.vutbr.cz>
 * @date October, 2016
 */

#include <algorithm>
#include <iostream>
#include <exception>
#include "ArgumentParser.h"
#include "ArgumentValidator.h"
#include "LayerMessage.h"
#include "PcapReader.h"
#include <iomanip>
#include "PcapReader.h"
#include "Statistics.h"

using namespace std;

int main(int argc, char * argv[])
{
	ArgumentParser args;
	args.setOption("i", "input file (pcap format)",
		true,
		"<file>",
		true);
	args.setOption("f", "filter type: mac, ipv4, ipv6, tcp, udp",
		true,
		"filter",
		true);
	args.setOption("v", "5C:D5:96:2C:38:63(MAC), 192.168.1.1(IPv4), 2001::1(IPv6), 80 (TCP, UDP), top10(mac, ipv4, ipv6, tcp, udp)",
		true,
		"<file>",
		true);
	args.setOption("s", "filter for source address",
		false,
		"",
		false);
	args.setOption("d", "filter for destination address",
		false,
		"",
		false);

	try {
		if (!args.validateArguments(argc, argv))
			return 1;
	}
	catch (exception &ex) {
		cerr << ex.what() << endl;
		return 10;
	}

	Top10 stat;
	stat.insertMessage("peto", 10, 30);
	stat.insertMessage("peto98", 5, 30);
	stat.insertMessage("peto1", 11, 30);
	stat.insertMessage("peto0", 11, 30);
	stat.insertMessage("peto2", 11, 30);
	stat.insertMessage("peto3", 11, 30);
	stat.insertMessage("peto4", 11, 30);
	stat.insertMessage("peto5", 11, 30);
	stat.insertMessage("peto6", 11, 30);
	stat.insertMessage("peto7", 11, 30);
	stat.insertMessage("peto8", 11, 30);
	stat.insertMessage("peto9", 11, 30);

	stat.showStatistics();
	


return 5;
	GenericLayerMessageFactory factory;
	factory.registerLayer(LINK_LAYER);
	factory.registerLayer(NETWORK_LAYER);

	//ifstream file("binfile", ios::in|ios::binary|ios::ate);
	ifstream file("pcap/ipv6test2.pcap", ios::in|ios::binary);

	Input input({file, std::vector<uint8_t>()});
	
	PcapReaderFromFile reader(input.file);

	reader.skip(24);
	factory.create(input, NETWORK_LAYER);
	cout << endl;
//	factory.create(file, MAC, SOURCE);
	
/*	char *buffer = reader.read(2000);
	for (int i = 0; i < 2000; i++) {
		cout << setfill('0') << setw(2) << hex <<(uint32_t)(uint8_t) buffer[i] << " ";
	}

*/	return 0;
}
