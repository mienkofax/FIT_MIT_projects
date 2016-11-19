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

/*
 * Reprezentuje mozny typ vstupu. Je vyuzite pri spracovani
 * jednotlivych vrsiev aby suhlasilo rozhranie a aby stacilo
 * na linkovej vrstve nacitat informacie zo suboru a v dalsich
 * vrstvach sa moze citat priamo z pamate a nemusi sa pristupovat
 * do suboru.
 */
struct Input {
	std::ifstream &file;
	std::vector<uint8_t> data;
};

/*
 * Zoznam podporovanych vrstiev.
 */
enum Layer {
	LINK_LAYER,
	NETWORK_LAYER,
	TRANSPORT_LAYER,
};

/*
 * Zoznam podporovanych protokolov.
 */
enum Protocols {
	MAC,
	IPV4,
	IPV6,
	TCP,
	UDP,
};

/*
 * Reprezentuje adresy v urcitom protokole.
 */
struct LayerData {
	std::vector<std::string> sourceAddress;
	std::vector<std::string> destinationAddress;
	size_t dataSize;
	size_t value1;
	bool isVLAN = false;
};

/*
 * Trieda uchováva informácie potrebne pre vytvorenie statistiky
 * alebo pre ulozenie informacii o adresach zadanych ako parametre
 * programu. Adresy sa uchovavaju vzdy ku konkretnemu protokolu do
 * do mapy, kde protokol sluzi ako index.
 */
class LayerMessage {
public:
	std::vector<uint8_t> data;
	long nextProtocol;
	bool destination;
	bool source;

	std::map<const Protocols, LayerData> address;

	/*
	 * Zobrazenie udajov o datach v triede.
	 */
	void show();

	/*
	 * Zisti zo zadaneho retazca, ktory obsahuje protokol, na ktorej
	 * vrstve sa nachadza dany protokol.
	 * @param &protocol retazec obsahujuci protokol
	 * @return enum vrstvy, na ktorom sa nachadza dany protokol
	 */
	Layer extractLayer(const std::string &protocol);

	/*
	 * Zistenie zo zadaneho retazca o aky prototokol sa jedna.
	 * @param &protocol retazec obsahujuci nazov protokolu
	 * @return enum zadaneho protokolu
	 */
	Protocols extractProtocol(const std::string &protocol);

	/*
	 * Zistenie zo dananych protokolov, ktory je na najvyssej vrstve.
	 * Pouzite na parsovanie sprav aby sa vedelo, po ktoru vrstvu sa ma
	 * parsovat pcap sprava.
	 * @param &filter vektor zadanych filtrov - protokolov
	 * @return enum vrstvy, ktora je najvyssie
	 */
	Layer extractHighestLayer(const std::vector<std::string> &filter);

private:
	void showItem(const std::string &itemName, const int &number);
	void showItem(const std::string &itemName,
		const std::vector<std::uint8_t> &items);
	void showItem(const std::string &itemName,
		const std::vector<std::string> &items);
};
