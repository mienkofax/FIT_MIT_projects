/**
 * @file LayerMessage.cpp
 * @Author Peter Tisovčík <xtisov00@stud.fit.vutbr.cz>
 * @date October, 2016
 */

 #include <exception>

#include "LayerMessage.h"

using namespace std;

void LayerMessage::showItem(const std::string &itemName,
	const std::vector<std::string> &items)
{
	cout << itemName << ": ";
	for (const auto &item : items)
		cout << item << ", ";
	cout << endl;
}

void LayerMessage::showItem(const std::string &itemName,
	const std::vector<std::uint8_t> &items)
{
	cout << itemName << ": ";
	for (const auto &item : items)
		cout << setfill('0') << setw(2) << hex << (uint32_t) item << " ";
	cout << endl;
}

void LayerMessage::showItem(const std::string &itemName, const int &number)
{
	cout << itemName << ": ";
		cout << setfill('0') << setw(2) << hex << number << " ";
	cout << endl;
}

void LayerMessage::show() {
	cout << "\n------------------layer_data----------------\n";
	for (const auto &item : address) {
		showItem("source address", item.second.sourceAddress);
		showItem("destiation address", item.second.destinationAddress);
		showItem("size", item.second.dataSize);
	}

	showItem("next protocol", nextProtocol);
	showItem("data", data);
}

Layer LayerMessage::extractLayer(const string &protocol)
{
	if (protocol == "mac")
		return LINK_LAYER;
	else if (protocol == "ipv4" || protocol == "ipv6")
		return NETWORK_LAYER;
	else if (protocol == "tcp" || protocol == "udp")
		return TRANSPORT_LAYER;

	throw invalid_argument("Unsupported protocol " + protocol);
}

Protocols LayerMessage::extractProtocol(const string &protocol)
{
	if (protocol == "mac")
		return MAC;
	else if (protocol == "ipv4")
		return IPV4;
	else if (protocol == "ipv6")
		return IPV6;
	else if (protocol == "tcp")
		return TCP;
	else if (protocol == "udp")
		return UDP;

	throw invalid_argument("Unsupported protocol " + protocol);
}

Layer LayerMessage::extractHighestLayer(const std::vector<string> &filter)
{
	Layer layer = LINK_LAYER;

	for (const auto &item : filter) {
		Layer currentLayer = extractLayer(item);

		if (layer < currentLayer)
			layer = currentLayer;
	}

	return layer;
}
