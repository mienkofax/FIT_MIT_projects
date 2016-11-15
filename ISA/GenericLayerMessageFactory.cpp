/**
 * @file GenericLayerMessageFactory.cpp
 * @Author Peter Tisovčík <xtisov00@stud.fit.vutbr.cz>
 * @date October, 2016
 */

#include <exception>

#include "GenericLayerMessageFactory.h"

using namespace std;

shared_ptr<LayerMessageFactory> GenericLayerMessageFactory::findFactory(
	const Layer &layer)
{
	auto search = m_layers.find(layer);

	if (search != m_layers.end())
		return search->second;

	throw runtime_error("Unregistered layer factory " + to_string(layer));
}

shared_ptr<LayerMessage> GenericLayerMessageFactory::create(Input &input,
	const Layer &layer)
{
	shared_ptr<LayerMessage> msg(new LayerMessage);

	// Spracovanie linkovej vrstvy
	msg = findFactory(LINK_LAYER)->create(input, layer);

	// Spracovanie sietovej vrstvy
	input.data = msg->data;
	findFactory(NETWORK_LAYER)->setLayerMessage(msg);

	msg = findFactory(NETWORK_LAYER)->create(input, layer);
	if (layer == NETWORK_LAYER || layer == LINK_LAYER)
		return msg;

	// Spracovanie transportnej vrstvy
	input.data = msg->data;
	findFactory(TRANSPORT_LAYER)->setLayerMessage(msg);

	msg = findFactory(TRANSPORT_LAYER)->create(input, layer);
	if (layer == TRANSPORT_LAYER)
		return msg;

	return msg;
}

void GenericLayerMessageFactory::registerLayer(const Layer &layer,
	shared_ptr<LayerMessageFactory> factoryObject)
{
	m_layers.insert(std::make_pair(layer, factoryObject));
}
