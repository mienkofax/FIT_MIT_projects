/**
 * @file PcapReader.h
 * @Author Peter Tisovčík <xtisov00@stud.fit.vutbr.cz>
 * @date October, 2016
 */

#pragma once

#include <exception>
#include <string>
#include <vector>

#include "PcapReader.h"

/*
 * Implementacia rozhranie pre citanie pcap zo suboru.
 */
class PcapReaderFromFile : public PcapReader {
public:
	PcapReaderFromFile(std::ifstream &file):
		m_file(file)
	{
	}

	/*
	 * Nacita cislo o danej velkosti ako big endian.
	 * @param &size pocet bajtov nacitaneho cisla
	 * @return nacitane cislo
	 */
	virtual long readIntBigEndian(const size_t &size) override;

	/*
	 * Nacita cislo o danej velkosti ako little endian.
	 * @param &size pocet bajtov nacitaneho cisla
	 * @return nacitane cislo
	 */
	virtual long readIntLittleEndian(const size_t &size) override;

	/*
	 * Nacita retazec o zadanej dlzke v pripade potrebny je mozne jednotive
	 * bajty oddelit zadanym separatorom, popripade je mozne definovat
	 * ci vysledny string ma byt typu hex formatu.
	 * @param &size pocet bajtov, ktore sa maju nacitat ako retazec
	 * @param &separator oddelovac jednotlivych bajtov vo vyslednom retazci
	 * @param &hexFormat true ak ma ma byt retazec v hexa formate
	 * @return nacitany retazec v zadanom formate
	 */
	virtual std::string readString(const size_t &size,
		const std::string &separator, const bool &hexFormat = false) override;

	/*
	 * Vytvori vektor dat o danej velkosti.
	 * @param &size velkost nacitanych dat
	 * @param vektor obsahujuci nacitane data
	 */
	virtual std::vector<uint8_t> readUint8Vector(const size_t &size) override;

	/*
	 * Preskocenie urciteho poctu bajtov, ktore nie je potrebne citat.
	 * @param &size velkost dat, ktore sa maju preskocit
	 */
	virtual void skip(const size_t &size) override;

	/*
	 * Nacita urcity pocet bajtov zo suboru.
	 * @param &size velkost nacitanych dat
	 * @return ukazovatel na pole nacitanych dat
	 */
	char *read(const size_t &size);

private:
	std::ifstream &m_file;
};
