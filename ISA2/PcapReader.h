/**
 * @file PcapReader.h
 * @Author Peter Tisovčík <xtisov00@stud.fit.vutbr.cz>
 * @date October, 2016
 */

#pragma once

#include <fstream>
#include <exception>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <vector>

using namespace std;

class PcapReader {
public:
	virtual long readIntBigEndian(size_t size) = 0;
	virtual long readIntLittleEndian(size_t size) = 0;
	virtual std::vector <uint8_t> readUint8Vector(size_t size) = 0;
	virtual void skip(size_t size) = 0;
};

class PcapReaderFromFile : public PcapReader {
public:
	PcapReaderFromFile(std::ifstream &file);

	long readIntBigEndian(size_t size);
	long readIntLittleEndian(size_t size);
	std::vector<uint8_t> readUint8Vector(size_t size);
	void skip(size_t size);
	char *read(size_t size);

private:
	std::ifstream &m_file;
};

class PcapReaderFromVector : public PcapReader {
public:
	PcapReaderFromVector(std::vector<uint8_t> data);

	long readIntBigEndian(size_t size);
	long readIntLittleEndian(size_t size);
	std::vector<uint8_t> readUint8Vector(size_t size);
	void skip(size_t size);

private:
	std::vector<uint8_t> m_data;
	int currentPosition;
}; 
