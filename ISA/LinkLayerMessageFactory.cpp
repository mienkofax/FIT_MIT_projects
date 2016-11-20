/**
 * @file LinkLayerMessageFactory.cpp
 * @Author Peter Tisovčík <xtisov00@stud.fit.vutbr.cz>
 * @date October, 2016
 */

#include "ArgumentValidator.h"
#include "LinkLayerMessageFactory.h"
#include "PcapReaderFromFile.h"

using namespace std;

#define PCAP_HEADER_TIMESTAMP 4
#define PCAP_HEADER_MICROSECONDS 4
#define PCAP_HEADER_FRAME_LENGTH 4
#define PCAP_HEADER_CAPTURE_LENGTH 4

#define ETHERNET_PREAMBLE 7
#define ETHERNET_SFD 1
#define ETHERNET_ADDRESS 6
#define ETHERNET_TYPE 2
#define ETHERNET_VLAN_TYPE 2
#define ETHERNET_VLAN 4
#define ETHERNET_8021Q_TRAILER 4

#define ETHERNET_PROTOCOL_8021Q   0x8100  /* Extended header */
#define ETHERNET_PROTOCOL_8021ad  0x88a8 /* Q-in-Q */

shared_ptr<LayerMessage> LinkLayerMessage::create(Input &input, const Layer&)
{
	PcapReaderFromFile reader(input.file);
	shared_ptr<LayerMessage> message = getLayerMessage();
	LayerData address;
	int readSize = 0;
	int frameType = 0;
	bool trailer = false;
	bool isVLAN = false;

	message->address.clear();

	// Preskocenie pcap hlavicky
	reader.skip(PCAP_HEADER_TIMESTAMP + PCAP_HEADER_MICROSECONDS
		+ PCAP_HEADER_FRAME_LENGTH);

	// Nacitanie velkosti daneho packetu
	readSize = reader.readIntBigEndian(PCAP_HEADER_CAPTURE_LENGTH);
	address.value1 = readSize;

	readSize -= 2 * ETHERNET_ADDRESS;
	address.destinationAddress.push_back(
		Normalization::getMac(reader.readString(ETHERNET_ADDRESS, ":", true)));
	address.sourceAddress.push_back(
		Normalization::getMac(reader.readString(ETHERNET_ADDRESS, ":", true)));

	readSize -= ETHERNET_TYPE;
	frameType = reader.readIntLittleEndian(ETHERNET_TYPE);

	if (frameType == ETHERNET_PROTOCOL_8021ad) {
		readSize -= ETHERNET_TYPE + ETHERNET_VLAN_TYPE;
		reader.skip(ETHERNET_VLAN_TYPE);
		frameType = reader.readIntLittleEndian(ETHERNET_TYPE);
	}

	while (1) {
		if (frameType != ETHERNET_PROTOCOL_8021Q)
			break;

		isVLAN = true;
		readSize -= ETHERNET_VLAN_TYPE;
		reader.skip(ETHERNET_VLAN_TYPE);

		readSize -= ETHERNET_VLAN_TYPE;
		frameType = reader.readIntLittleEndian(ETHERNET_VLAN_TYPE);

		//address.dataSize += ETHERNET_VLAN_TYPE;

		//trailer = true;
	}

	if (trailer)
		readSize -= ETHERNET_8021Q_TRAILER;

	message->data = reader.readUint8Vector(readSize);
	message->nextProtocol = frameType;

	address.dataSize = message->data.size();
	message->address[MAC] = address;

	message->address[MAC].isVLAN = isVLAN;

	if (trailer)
		reader.skip(ETHERNET_8021Q_TRAILER);

	return message;
}
