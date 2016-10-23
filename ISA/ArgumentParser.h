/**
 * @file ArgumentParser.h
 * @Author Peter Tisovčík <xtisov00@stud.fit.vutbr.cz>
 * @date October, 2016
 */

#pragma once

#include <list>
#include <string>
#include <vector>

#include "Option.h"
#include "LayerMessage.h"

const std::vector<std::string> filterType = {"mac", "ipv4", "ipv6", "tcp", "udp"};

/*
 * Trieda obsahuje informacie o prepinacoch, ktore maju byt zadane
 * a na zaklade nich kontroluje, ci boli dodrzane poziadavky na zadanie.
 * Medzi poziadavky patri, aby sa neopakovali zadane prepinace, aby sa
 * kontrolovalo ci boli zadane potrebne parametre. Kontroluje sa aj
 * existencia zadnych filtrov ci su validne a ci su podporovane.
 * Dalej sa kontroluje hodnota filtrov, ci boli zadane validne adresy.
 */
class ArgumentParser {
public:
	ArgumentParser():
		m_isTop10(false)
	{
	}

	/*
	 * Vrati ci bol zadany prepinac pre zobrazenie statistiky pre top10
	 * @return True ak bola zadana hodnota filtra top10
	 */
	bool isTop10()
	{
		return m_isTop10;
	}

	/*
	 * Vrati zadanu argument filtra, text ktory je potrebny za prepinacom -f.
	 * @return vektor zadanych filtrov
	 */
	std::vector<std::string> getFilter()
	{
		return split(getArgument("f"), ',');
	}

	/*
	 * Vrati spravu, ktora obsahuje udaje o jedotlivych filtroch a hodnotach
	 * daneho filtra.
	 * @return sprava obsahujuca udaje o zadanych hodnotach a filtroch
	 */
	LayerMessage getLayersMessage()
	{
		return m_layerMessage;
	}

	/*
	 * Vlozenie udajov o volbe, ktora sa zadava ako parameter programu.
	 * @param shortName kratky nazov prepinaca
	 * @param description popis prepinaca
	 * @param required ci je nutne zadat tento prepinaca
	 * @param argName hodnota argumentu prepinaca
	 * @param argRequired ci je nutne zadat hodnotu prepinaca
	 */
	void setOption(std::string shortName, std::string description,
		bool required, std::string argName, bool argRequired)
	{
		m_options.push_back(Option(shortName, description, required,
				argName, argRequired));
	}

	/*
	 * Zobrazenie informacie o vsetkych arguemntoch.
	 */
	void show();

	/*
	 * Hlavna metoda, ktora kontroluje a parsuje zadane arguemnty.
	 * @param &argc pocet zadanych udajov
	 * @param *argv[] zadany retazec
	 */
	bool validateArguments(const int &argc, char *argv[]);

	/*
	 * Vracia argument zadaneho parametra.
	 * @param &arg kratke meno parametra
	 * @return argument zadaneho parametra
	 */
	std::string getArgument(const std::string &arg);

private:
	std::list<Option> m_options;
	LayerMessage m_layerMessage;
	bool m_isTop10;

	/*
	 * Kontrola ci boli zadane len podporodane hodnoty filtrov.
	 * Prechadza za jednotlivymi kategoriami filtrov, ktore su oddelene
	 * bodkociarkou a nasledne sa dana kategoria rozdeli na jednotlive
	 * retazce na zaklade ciarky. Rozparsovane hodnoty sa nasledne
	 * kontroluju podla typu filtra ci dana hodnota je validna.
	 * @param &filterArgumets vektor zadanych filtrov
	 * @return true ak boli zadane validne hodnoty filtrov
	 */
	bool checkFilterValues(const std::vector<std::string> &filterArgumets);

	/*
	 * Kontrola ci boli zadane len existujuce filre.
	 * @param &filterArgumets zadane filtre
	 * @return true ak boli zadane len podporovane filtre
	 */
	bool checkFiltersExists(const std::vector<std::string> &filterArgumets);

	/*
	 * V pripade, ze bola zadana hodnota filtra ako Top10, je potrebne,
	 * overit ci bol zadany len jeden filter.
	 * @return true ak sa jednalo o top10
	 */
	bool checkIsTop10();

	/*
	 * Na zaklade protokolu vrati normalizovanu adresu.
	 * @param &data retazec, ktory sa ma normalizovat
	 * @param &protocol typ protokolu, podla ktoreho sa rozpozna adresa
	 * @return normalizovany string
	 */
	std::string getNormalizeString(const std::string &data,
		const std::string &protocol);

	/*
	 * Doplnenie udajov o zadnych parametroch. Kontrola ci bol zadany
	 * parameter, ktory ma byt zadany. Kontrola ci sa nezadalo viacero
	 * parametrov naraz.
	 * @param &argc pocet zadanych retazcov
	 * @param *argv[] zadany retazec
	 */
	void extractArguments(const int &argc, char *argv[]);

	/*
	 * Kontrola ci boli zadane argumenty, ktore su vyzadovane
	 */
	void validateRequired();

	/*
	 * Vytvori string pre funkciu getopt. Za arguemnty, ktore vyzaduju argument
	 * ako napriklad: prepinac pre subor vyzaduje nazov suboru.
	 * @return  retazec obsahujuci string pre funkciu getopt
	 */
	std::string getOptString();

	/*
	 * Vyberie sa pozadovany volba prikazoveho riadku na zaklade zadaneho
	 * kratkeho mena.
	 * @param &shortName Kratky nazov prepinaca
	 * @return referencia na objekt Option, ktory obsahuje udaje o danej volbe
	 */
	Option &getOption(const std::string &shortName);

	/*
	 * Rozdelenie retazca na zaklade oddelovacieho znaku.
	 * @param &text retazec na Rozdelenie
	 * @param &separator retazec na zaklade, ktoreho sa ma rozdelit zadany vstup
	 * @return vektor rozdelenych hodnot
	 */
	std::vector<std::string> split(const std::string &text, const char &separator);

	/*
	 * Vrati vektor zadanych udajov z retazca, ktory obsahuje hodnoty oddelene
	 * ciarkou a overi spravnost zadanych jednotlivuch udajov. Overi ci su,
	 * ip, mac adresy alebo porty zadane spravne.
	 * @param &str String obsahuju hodnoty filtrov, ktore su oddelene ciarkou
	 * @param &filter typ filtra
	 * @return vektor hodnot z prikazoveho riadku
	 */
	std::vector<std::string> getFilterValue(const std::string &str, const std::string &filter);
};
