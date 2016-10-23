/**
 * @file NetworkLayerMessageFactory.cpp
 * @Author Peter Tisovčík <xtisov00@stud.fit.vutbr.cz>
 * @date October, 2016
 */

#include "LayerMessage.h"
#include "ArgumentValidator.h"
#include "NetworkLayerMessageFactory.h"
#include "PcapReaderFromVector.h"

using namespace std;

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

shared_ptr<LayerMessage> NetworkLayerMessage::create(Input &input, const Layer&)
{
	PcapReaderFromVector reader(input.data);
	shared_ptr<LayerMessage> message = getLayerMessage();
	LayerData address;
	int ipVersion = 0;
	int tmpData = 0;
	int readSize = 0;

	if (message->nextProtocol != 0x0800 && message->nextProtocol != 0x86dd)
		throw runtime_error("Unsupported network protocol "
		+ to_string(message->nextProtocol));

	tmpData = reader.readIntLittleEndian(N_IP4_VERSION_AND_IHL);
	ipVersion = (tmpData & N_IP4_VERSION_MASK) >> 4;
	readSize -= N_IP4_VERSION_AND_IHL;

	if (ipVersion == N_IP4_VERSION) {
		reader.skip(N_IP4_TYPE_OF_SEVICE);
		readSize -= N_IP4_TYPE_OF_SEVICE;
		readSize += reader.readIntLittleEndian(N_IP4_LENGTH) - N_IP4_LENGTH;

		reader.skip(N_IP4_IDENTIFICATION + N_IP4_OFFSET + N_IP4_TTL);
		readSize -= N_IP4_IDENTIFICATION + N_IP4_OFFSET + N_IP4_TTL;

		readSize -= N_IP4_PROTOCOL;
		message->nextProtocol = reader.readIntLittleEndian(N_IP4_PROTOCOL);

		reader.skip(N_IP4_CRC);
		address.sourceAddress.push_back(
			Normalization::getIPv4(reader.readString(N_IP4_ADDRESS, ".")));
		address.destinationAddress.push_back(
			Normalization::getIPv4(reader.readString(N_IP4_ADDRESS, ".")));
		readSize -= N_IP4_CRC + 2 * N_IP4_ADDRESS;

		message->data = reader.readUint8Vector(readSize);

		address.dataSize = message->data.size();
		message->address[IPV4] = address;
	} else if (ipVersion == N_IP6_VERSION) {
		reader.skip(N_IP6_TRAFFIC_CLASS + N_IP6_FLOW_LABEL);
		readSize = reader.readIntLittleEndian(N_IP6_PAYLOAD_LENGTH);

		message->nextProtocol = reader.readIntLittleEndian(N_IP6_NEXT_HEADER);
		reader.skip(N_IP6_HOP_LIMIT);

		address.sourceAddress.push_back(Normalization::getIPv6(reader.readString(N_IP6_ADDRESS_LENGTH, ".")));
		address.destinationAddress.push_back(Normalization::getIPv6(reader.readString(N_IP6_ADDRESS_LENGTH, ".")));

		message->data = reader.readUint8Vector(readSize);

		address.dataSize = message->data.size();
		message->address[IPV6] = address;
	}

	return message;
}
