#pragma once

#include <map>
#include <string>
#include <vector>

#include "AbstractModulator.h"

class Demodulator final : public AbstractModulator {
public:
	Demodulator();

	void process() override;
	void save() override;

private:
	/**
	 * Metoda nacita int cislo, z ktoreho sa pomocou masiek daju ziskat
	 * informacie o subore a datach. Dalej metoda nacita data zo vstupneho
	 * suboru a ulozi ich do vektoru.
	 */
	void loadSamples();

	/**
	 * Jednotlive vzorky mozu byt modulovane na zaklade rozdielnych presnosti.
	 * Metoda vycita sposob modulacie a nastavi presnost, s ktorou sa moze
	 * porovnavat.
	 */
	void setEps(int format);

	/**
	 * Porovnanie ci sa dva vzorky rovnaju s pozadovanou presnostou. Presnost
	 * sa nastavuje pomocou metody setEps() a musi byt nastavena pred prvym
	 * volanim tejto metody.
	 */
	bool equalSamples(int sample1, int sample2) const;

	/**
	 * Posuv vsetkych faz o 2pi aby sa mohla otestovat dalsia moznost rozlozenia faz.
	 */
	void shiftPhases();

	/**
	 * Zistenie o aku fazu/signalovu jednotku sa jedna.
	 *
	 * Prechov vsetkymi fazami a overenie ci sa vsetky faze v danej signalovej
	 * jednotke rovnaju, ak ano detekovali sme fazu.
	 */
	std::string detectPhase(int pocet, int startIndex);

	/**
	 * Zistenie faze jednotlivych dvojic bitov a zaroven zistenie dlzky jednej
	 * signalovej jednotky.
	 *
	 * Na zaciatku predpodkladame, ze faza je presne taka ista akou sme si to
	 * namodulovali. Vypocitame si referencnu hodnotu a porovname ju s hodnotou zo
	 * suboru, ak tieto hodnoty sedia vieme ze sme sa trafili do spravnej faze.
	 * Ak tato hodnota nesedi posunieme vsetky faze o 2pi.
	 *
	 * Nasledne zistime kolkym vzorkam odpoveda dany posun, tak zistime dlzku
	 * signalovej jednotky.
	 */
	int detectCountOfSamples();

	/**
	 * Overenie synchronizacnej sekvencie na vstupe.
	 */
	bool checkSyncSequence(int countSamples);

	/**
	 * Metoda extrahuje data zo vstupu (preskakuje synchronizacnu sekvenciu).
	 *
	 * dataSize obsahuje pocet dat, ktore sa nachadzaju na vstupe
	 * syncSize obsahuje pocet synchronizacnych dat, tkore sa nachadaju na vstupe
	 */
	void extractDataFromSamples(int countSamples);

private:
	int m_format;
	int m_eps;
	std::vector<int> m_list;
	std::map<std::string, double> m_phases;
	std::vector<std::string> m_outputData;
};
