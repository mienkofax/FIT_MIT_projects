/**
 * @file NetworkLayerMessageFactory.h
 * @Author Peter Tisovčík <xtisov00@stud.fit.vutbr.cz>
 * @date October, 2016
 */

#pragma once

#include <memory>

#include "LayerMessage.h"
#include "LayerMessageFactory.h"

using namespace std;

/*
 * Trieda spracuvava Sietovu vrstvu, konkretne IPv4 a IPv6 protokoly.
 */
class NetworkLayerMessage : public LayerMessageFactory {
public:
	std::shared_ptr<LayerMessage> create(Input &input, const Layer &layer) override;
	~NetworkLayerMessage() override {}
};
