/**
 * @file LayerMessage.h
 * @Author Peter Tisovčík <xtisov00@stud.fit.vutbr.cz>
 * @date October, 2016
 */

#pragma once

#include <fstream>
#include <map>
#include <string>
#include <vector>
#include <iomanip>
#include <iostream>
using namespace std;

struct Input {
	std::ifstream &file;
	std::vector<uint8_t> data;
};
enum Layer {
	LINK_LAYER,
	NETWORK_LAYER,
	TRANSPORT_LAYER,
};

enum COMMUNICATION_TYPE {
	MAC,
	IPV4,
	IPV6,
	TCP,
	UDP,
};
enum POSITION_IN_COMMUNICATION {
	SOURCE,
	DESTINATION,
	SOURCE_AND_DESTINATION,
};

class LayerMessage {
public:
	std::vector<uint8_t> macDestination;
	std::vector<uint8_t> macSource;
	std::vector<uint8_t> ipDestination;
	std::vector<uint8_t> ipSource;
	long portSource;
	long portDestination;
	std::vector<uint8_t> data;
	long port;
	long ipVersion;
	long protocol;

	void showItem(std::string itemName, std::vector<uint8_t> items)
	{
		cout << itemName << ": ";
		for (auto item : items)
			cout << setfill('0') << setw(2) << hex << (uint32_t) item << " ";
		cout << endl;
	}
	
	void showItem(std::string itemName, int number)
	{
		cout << itemName << ": ";
			cout << setfill('0') << setw(2) << hex << number << " ";
		cout << endl;
	}

	void show() {
		showItem("mac destination", macDestination);
		showItem("mac Source", macSource);
		showItem("ip destination", ipDestination);
		showItem("ip source", ipSource);
		showItem("data", data);
		showItem("port", port);
		showItem("ip version", ipVersion);
		showItem("next heaer", protocol);
	}
};

class LayerMessageFactory {
public:
	virtual LayerMessage *create(Input &input, const Layer &layer) = 0;
	virtual ~LayerMessageFactory() {}
};

class GenericLayerMessageFactory : public LayerMessageFactory {
public:
	LayerMessage *create(Input &input, const Layer &layer) override;
	~GenericLayerMessageFactory() override;
	void registerLayer(const Layer &layer);
	LayerMessageFactory *findFactory(const Layer &layer);

private:
	std::map<const Layer, LayerMessageFactory*> m_layers;
};

class LinkLayerMessage : public LayerMessageFactory {
public:
	LayerMessage *create(Input &input, const Layer &layer) override;
	~LinkLayerMessage() override {}
};

class NetworkLayerMessage : public LayerMessageFactory {
public:
	LayerMessage *create(Input &input, const Layer &layer) override;
	~NetworkLayerMessage() override {}
};

class TransportLayerMessage : public LayerMessageFactory {
public:
	LayerMessage *create(Input &input, const Layer &layer) override;
	~TransportLayerMessage() override {}
};
