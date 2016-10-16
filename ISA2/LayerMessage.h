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
#include <memory>

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

enum Protocols {
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

struct LayerData {
	std::vector<std::string> sourceAddress;
	std::vector<std::string> destinationAddress;
};

class LayerMessage {
public:
	std::vector<uint8_t> data;
	long nextProtocol;
	bool destination;
	bool source;

	std::map<const Protocols, LayerData> address;

	void showItem(std::string itemName, std::vector<std::string> items)
    {
        cout << itemName << ": ";
        for (auto item : items)
            cout << item << ", ";
        cout << endl;
    }

	void showItem(std::string itemName, std::vector<std::uint8_t> items)
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
		cout << "\n------------------layer_data----------------\n";
		for (auto item : address) {
			showItem("source address", item.second.sourceAddress);
			showItem("destiation address", item.second.destinationAddress);
		}

		showItem("next protocol", nextProtocol);
		showItem("data", data);
	}
};

class LayerMessageFactory {
public:
	virtual std::shared_ptr<LayerMessage> create(Input &input, const Layer &layer) = 0;
	virtual ~LayerMessageFactory() {}

	void setLayerMessage(std::shared_ptr<LayerMessage> message)
	{
		m_message = message;
	}

protected:
	std::shared_ptr<LayerMessage> m_message;

	std::shared_ptr<LayerMessage> getLayerMessage()
	{
		if (!m_message)
			m_message.reset(new LayerMessage());

		return m_message;
	}
};

class GenericLayerMessageFactory : public LayerMessageFactory {
public:
	std::shared_ptr<LayerMessage> create(Input &input, const Layer &layer) override;
	~GenericLayerMessageFactory() override;
	void registerLayer(const Layer &layer);
	std::shared_ptr<LayerMessageFactory> findFactory(const Layer &layer);

private:
	std::map<const Layer, std::shared_ptr<LayerMessageFactory>> m_layers;
};

class LinkLayerMessage : public LayerMessageFactory {
public:
	std::shared_ptr<LayerMessage> create(Input &input, const Layer &layer) override;
	~LinkLayerMessage() override {}
};

class NetworkLayerMessage : public LayerMessageFactory {
public:
	std::shared_ptr<LayerMessage> create(Input &input, const Layer &layer) override;
	~NetworkLayerMessage() override {}
};

class TransportLayerMessage : public LayerMessageFactory {
public:
	std::shared_ptr<LayerMessage> create(Input &input, const Layer &layer) override;
	~TransportLayerMessage() override {}
};
