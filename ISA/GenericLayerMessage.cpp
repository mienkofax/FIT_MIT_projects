/**
 * @file GenericLayerMessage.cpp
 * @Author Peter Tisovčík <xtisov00@stud.fit.vutbr.cz>
 * @date October, 2016
 */

3include <iostream>

#include "LayerMessage.h"
#include "PcapReader.h"
#include "ArgumentValidator.h"

#include "GenericLayerMessageFactory.h"
#include "NetworkLayerMessageFactory.h"
#include "LinkLayerMessageFactory.h"
#include "TransportLayerMessageFactory.h"

using namespace std;

shared_ptr<LayerMessageFactory> GenericLayerMessageFactory::findFactory(const Layer &layer)
{
	auto search = m_layers.find(layer);

	if (search != m_layers.end())
		return search->second;
	else
		throw invalid_argument("Unknown layer, please register this layer");
}

shared_ptr<LayerMessage> GenericLayerMessageFactory::create(Input &input, const Layer &layer)
{
	shared_ptr<LayerMessage> msg(new LayerMessage);

	msg = findFactory(LINK_LAYER)->create(input, layer);
	if (layer == LINK_LAYER)
		return msg;

	input.data = msg->data;
	findFactory(NETWORK_LAYER)->setLayerMessage(msg);
	msg = findFactory(NETWORK_LAYER)->create(input, layer);
	if (layer == NETWORK_LAYER)
		return msg;

	input.data = msg->data;
	findFactory(TRANSPORT_LAYER)->setLayerMessage(msg);
	msg = findFactory(TRANSPORT_LAYER)->create(input, layer);
	if (layer == TRANSPORT_LAYER)
		return msg;

	throw invalid_argument("Unknown layer type " + to_string(layer));
}

void GenericLayerMessageFactory::registerLayer(const Layer &layer,
	shared_ptr<LayerMessageFactory> factoryObject)
{
	m_layers.insert(std::make_pair(layer, factoryObject));
}

GenericLayerMessageFactory::~GenericLayerMessageFactory()
{
}
