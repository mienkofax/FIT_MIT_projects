/**
 * @file LayerMessageFactory.h
 * @Author Peter Tisovčík <xtisov00@stud.fit.vutbr.cz>
 * @date October, 2016
 */

#pragma once

#include <map>
#include <memory>
#include <string>
#include <vector>

#include "LayerMessageFactory.h"

/*
 * Rozhranie pre jednotlive vrstvy, ktore musi byt naimplementove.
 * Rozhranie poskytuje metodu create, v ktorej sa implementuje spracovanie
 * jednotlivych protokolov na danej vrstve.
 */
class LayerMessageFactory {
public:
	/*
	 * Metoda sluzi na spracovanie jednotlivych protokolov na danej vrstve.
	 * @param &input struktura obsahujuca dva mozne typy vstupu subor/vektor
	 * @param &layer vrstva, po ktoru je potrebne spracovat
	 * @return sprava s naplnenymi udajmi o adresach a velkostiach dat
	 * na jednotlivych vrstvach
	 */
	virtual std::shared_ptr<LayerMessage> create(Input &input, const Layer &layer) = 0;

	virtual ~LayerMessageFactory()
	{
	}

	/*
	 * Nastavi spravu pre danu vrstvu. Dvovodom je aby sa v jednej spravne
	 * uchovavali informacie o vsetkych vrstvach a aby sa nevytvarala sprava
	 * na kazdej vrstve samostatne.
	 * @param message sprava s naplnenymi udajmi o adresach a velkostiach data
	 * na jednotlivych vrstvach
	 */
	void setLayerMessage(std::shared_ptr<LayerMessage> message)
	{
		m_message = message;
	}

protected:
	std::shared_ptr<LayerMessage> m_message;

	/*
	 * V pripade ze neexistuje sprava je vytvorena ak existuje vrati sa.
	 * @param message sprava s naplnenymi udajmi o adresach a velkostiach data
	 * na jednotlivych vrstvach
	 */
	 std::shared_ptr<LayerMessage> getLayerMessage()
 	{
 		if (!m_message)
 			m_message.reset(new LayerMessage());

 		return m_message;
 	}
};
