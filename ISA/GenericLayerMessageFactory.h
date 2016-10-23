/**
 * @file GenericLayerMessageFactory.h
 * @Author Peter Tisovčík <xtisov00@stud.fit.vutbr.cz>
 * @date October, 2016
 */

#pragma once

#include <iomanip>
#include <map>
#include <memory>
#include <vector>

#include "LayerMessage.h"
#include "LayerMessageFactory.h"

/*
 * Obecna trieda, ktora sa stara o spravu jednotlivych vrstiev,
 * registruje vrstvy alebo vytvara pozadavane vrstvy. Pri volani
 * metody creat sa na zaklade indexu vyberie pozadovana vrstva a
 * na nu sa zavola create.
 */
class GenericLayerMessageFactory : public LayerMessageFactory {
public:
	/*
	 * Vytvora vrstvy na zaklade toho o ktoru vrstvu sa ziada. Ak sa
	 * ziada o transportnu vrstvu, je potrebne najprv vytvorit linkovu,
	 * sietovu a az potom sa moze vytvorit transportna vrstva. Dalej
	 * zdiela spravu medzi jednotlivymi vrstvami.
	 * @param &input struktura obsahujuca dva mozne typy vstupu subor/vektor
	 * @param &layer vrstva, po ktoru je potrebne spracovat
	 * @return sprava s naplnenymi udajmi o adresach a velkostiach dat
	 */
	std::shared_ptr<LayerMessage> create(Input &input,
		const Layer &layer) override;

	~GenericLayerMessageFactory() override
	{
	}

	/*
	 * Registruje sietovu vrstvu na zaklade protokolu. Protokol je index
	 * na objekt, reprezentujuci danu triedu.
	 * @param &layer vrstva, ktora identifikuje/indexuje dany objekt
	 * @param &factoryObject zdielany ukazovatel na novy objekt, ktory
	 * ma naimplementovane rovnake rozhranie ako LayerMessageFactory
	 */
	void registerLayer(const Layer &layer,
		std::shared_ptr<LayerMessageFactory> factoryObject);

	/*
	 * Vyhladanie pozadovaneho objektu na zaklade vrstvy, ktora ho indexuje.
	 * @param &layer vrstva, ktora indexuje dany objekt
	 * @return zdielany ukazovatel na pozadovanu vrstvu
	 */
	std::shared_ptr<LayerMessageFactory> findFactory(const Layer &layer);

private:
	std::map<const Layer, std::shared_ptr<LayerMessageFactory>> m_layers;
};
