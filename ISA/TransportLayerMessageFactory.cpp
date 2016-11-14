/**
 * @file TransportLayerMessageFactory.cpp
 * @Author Peter Tisovčík <xtisov00@stud.fit.vutbr.cz>
 * @date October, 2016
 */

#include "ArgumentValidator.h"
#include "PcapReaderFromVector.h"
#include "TransportLayerMessageFactory.h"

using namespace std;

#define PORT 2
#define TCP_SEQUENCE_NUMBER 4
#define TCP_ACK 4
#define TCP_OPTION 1
#define TCP_DATA_OFFSET_MASK 0xf0
#define UDP_DATA_LEN 2
#define UDP_CHECKSUM 2

shared_ptr<LayerMessage> TransportLayerMessage::create(Input &input, const Layer&)
{
	PcapReaderFromVector reader(input.data);
	shared_ptr<LayerMessage> message = getLayerMessage();
	LayerData address;
	int readSize = 0;

	if (message->nextProtocol != 0x06 && message->nextProtocol != 0x11)
		throw runtime_error("Unsupported transport protocol " + to_string(message->nextProtocol) + "\n");

	address.sourceAddress.push_back(to_string(reader.readIntLittleEndian(PORT)));
	address.destinationAddress.push_back(to_string(reader.readIntLittleEndian(PORT)));
	readSize -= 2 * PORT;

	if (message->nextProtocol == 0x06) {
		readSize -= TCP_SEQUENCE_NUMBER + TCP_ACK;
		reader.skip(TCP_SEQUENCE_NUMBER + TCP_ACK);

		readSize -= TCP_OPTION;
		readSize += ((reader.readIntLittleEndian(TCP_OPTION) & TCP_DATA_OFFSET_MASK) >> 4)*4;
		reader.skip(readSize);

		message->data = reader.readToEnd();

		address.dataSize = message->data.size();
		message->address[TCP] = address;
	}
	else if (message->nextProtocol == 0x11) {
		readSize += reader.readIntLittleEndian(UDP_CHECKSUM) - UDP_DATA_LEN;

		readSize -= UDP_CHECKSUM;
		reader.skip(UDP_CHECKSUM);

		message->data = reader.readToEnd();

		address.dataSize = message->data.size();
		message->address[UDP] = address;
	}

	return message;
}
