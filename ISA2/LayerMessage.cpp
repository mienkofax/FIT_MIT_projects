/**
 * @file LayerMessage.cpp
 * @Author Peter Tisovčík <xtisov00@stud.fit.vutbr.cz>
 * @date October, 2016
 */


#include "LayerMessage.h"
#include "PcapReader.h"
#include <iostream>
using namespace std;

LayerMessageFactory *GenericLayerMessageFactory::findFactory(const Layer &layer)
{
	auto search = m_layers.find(layer);

	if (search != m_layers.end())
		return search->second;
	else
		throw invalid_argument("Unknown layer, please register this layer");
}

LayerMessage *GenericLayerMessageFactory::create(Input &input, const Layer &layer)
{
	LayerMessage *msg;

	msg = findFactory(LINK_LAYER)->create(input, layer);
	if (layer == LINK_LAYER)
		return msg;

	input.data = msg->data;
	msg = findFactory(NETWORK_LAYER)->create(input, layer);
	if (layer == NETWORK_LAYER)
		return msg;

	input.data = msg->data;
	msg = findFactory(TRANSPORT_LAYER)->create(input, layer);
	if (layer == TRANSPORT_LAYER)
		return msg;
	
	throw invalid_argument("unknown layer type");
}

void GenericLayerMessageFactory::registerLayer(const Layer &layer)
{
	LayerMessageFactory *factoryObject;
	switch(layer) {
	case LINK_LAYER:
		factoryObject = new LinkLayerMessage();
		m_layers.insert(std::make_pair(layer, factoryObject));
		break;
	case NETWORK_LAYER:
		factoryObject = new NetworkLayerMessage();
		m_layers.insert(std::make_pair(layer, factoryObject));
		break;
	case TRANSPORT_LAYER:
		factoryObject = new TransportLayerMessage();
		m_layers.insert(std::make_pair(layer, factoryObject));
	}
}

GenericLayerMessageFactory::~GenericLayerMessageFactory()
{
	for (auto item : m_layers)
		delete item.second;
}
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

LayerMessage *LinkLayerMessage::create(Input &input, const Layer&)
{
	PcapReaderFromFile reader(input.file);
	LayerMessage *message = new LayerMessage();
	int readSize = 0;
	int frameType = 0;
	bool trailer = false;

	// Skip pcap header
	reader.skip(PCAP_HEADER_TIMESTAMP + PCAP_HEADER_MICROSECONDS
		+ PCAP_HEADER_FRAME_LENGTH);

	readSize = reader.readIntBigEndian(PCAP_HEADER_CAPTURE_LENGTH);

	readSize -= 2 * ETHERNET_ADDRESS;
	message->macDestination = reader.readUint8Vector(ETHERNET_ADDRESS);
	message->macSource= reader.readUint8Vector(ETHERNET_ADDRESS);

	readSize -= ETHERNET_TYPE;
	frameType = reader.readIntLittleEndian(ETHERNET_TYPE);

	if (frameType == ETHERNET_PROTOCOL_8021ad) {
		readSize -= ETHERNET_TYPE + ETHERNET_VLAN_TYPE;
		reader.skip(ETHERNET_VLAN_TYPE);
		frameType = reader.readIntLittleEndian(ETHERNET_TYPE);
	}

	if (frameType == ETHERNET_PROTOCOL_8021Q) {
		readSize -= ETHERNET_VLAN_TYPE;
		reader.skip(ETHERNET_VLAN_TYPE);

		readSize -= ETHERNET_VLAN_TYPE;
		frameType = reader.readIntLittleEndian(ETHERNET_VLAN_TYPE);

		trailer = true;
	}


	if (trailer)
		readSize -= ETHERNET_8021Q_TRAILER;

	message->data = reader.readUint8Vector(readSize);
	message->ipVersion = frameType;
	message->show();

	return message;
}

#define N_IP4_VERSION_AND_IHL 1
#define N_IP4_TYPE_OF_SEVICE 1
#define N_IP4_LENGTH 2
#define N_IP4_IDENTIFICATION 2
#define N_IP4_OFFSET 2
#define N_IP4_TTL 1
#define N_IP4_PROTOCOL 1
#define N_IP4_CRC 2
#define N_IP4_ADDRESS 4

#define N_IP4_VERSION_MASK 0xf0
#define N_IP4_IHL_MASK 0x0f /*unsupported */
#define N_IP4_IHL_SHIFT 2

#define N_IP4_VERSION 4
#define N_IP6_VERSION 6

#define N_IP6_TRAFFIC_CLASS 1
#define N_IP6_FLOW_LABEL 2
#define N_IP6_PAYLOAD_LENGTH 2
#define N_IP6_NEXT_HEADER 1
#define N_IP6_HOP_LIMIT 1
#define N_IP6_ADDRESS_LENGTH 16

LayerMessage *NetworkLayerMessage::create(Input &input, const Layer&)
{
	PcapReaderFromVector reader(input.data);
	LayerMessage *message = new LayerMessage();
	int ipVersion = 0;
	int tmpData = 0;
	int readSize = 0;

	tmpData = reader.readIntLittleEndian(N_IP4_VERSION_AND_IHL);
	message->ipVersion = (tmpData & N_IP4_VERSION_MASK) >> 4;
	readSize -= N_IP4_VERSION_AND_IHL;

	cout << "TMP DATA:" << tmpData;

	if (ipVersion == N_IP4_VERSION) {
		reader.skip(N_IP4_TYPE_OF_SEVICE);
		readSize -= N_IP4_TYPE_OF_SEVICE;
		readSize += reader.readIntLittleEndian(N_IP4_LENGTH) - N_IP4_LENGTH;

		reader.skip(N_IP4_IDENTIFICATION + N_IP4_OFFSET + N_IP4_TTL);
		readSize -= N_IP4_IDENTIFICATION + N_IP4_OFFSET + N_IP4_TTL;

		readSize -= N_IP4_PROTOCOL;
		message->protocol = reader.readIntLittleEndian(N_IP4_PROTOCOL);

		reader.skip(N_IP4_CRC);
		message->ipDestination = reader.readUint8Vector(N_IP4_ADDRESS);
		message->ipSource = reader.readUint8Vector(N_IP4_ADDRESS);
		readSize -= N_IP4_CRC + 2 * N_IP4_ADDRESS;

		message->data = reader.readUint8Vector(readSize);

		message->show();
	} else if (message->ipVersion == N_IP6_VERSION) {
		reader.skip(N_IP6_TRAFFIC_CLASS + N_IP6_FLOW_LABEL);
		readSize = reader.readIntLittleEndian(N_IP6_PAYLOAD_LENGTH);

		message->protocol = reader.readIntLittleEndian(N_IP6_NEXT_HEADER);
		reader.skip(N_IP6_HOP_LIMIT);

		message->ipSource = reader.readUint8Vector(N_IP6_ADDRESS_LENGTH);
		message->ipDestination = reader.readUint8Vector(N_IP6_ADDRESS_LENGTH);

		//https://en.wikipedia.org/wiki/List_of_IP_protocol_numbers
		cout << "message protocol: " << message->protocol;
		if (message->protocol != 0x06 && message->protocol != 0x11)
			return message;

		message->data = reader.readUint8Vector(readSize);
		message->show();
	}

	return message;
}


LayerMessage *TransportLayerMessage::create(Input &input, const Layer&)
{
	PcapReaderFromVector reader(input.data);
	LayerMessage *message = new LayerMessage();

	
	return message;
}
