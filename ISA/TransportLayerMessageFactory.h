/**
 * @file TransportLayerMessageFactory.h
 * @Author Peter Tisovčík <xtisov00@stud.fit.vutbr.cz>
 * @date October, 2016
 */

#pragma once

#include <memory>

#include "LayerMessage.h"
#include "LayerMessageFactory.h"

using namespace std;

class TransportLayerMessage : public LayerMessageFactory {
public:
	std::shared_ptr<LayerMessage> create(Input &input, const Layer &layer) override;
	~TransportLayerMessage() override {}
};
