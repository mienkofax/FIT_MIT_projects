/**
 * @file PcapReader.cpp
 * @Author Peter Tisovčík <xtisov00@stud.fit.vutbr.cz>
 * @date October, 2016
 */

#include <sstream>

#include "PcapReaderFromVector.h"
#include <iostream>
using namespace std;

#define BYTE_SIZE   8

long PcapReaderFromVector::readIntBigEndian(const size_t &size)
{
	long number = 0;

	for (size_t i = 0; i < size; i++)
		number |= (uint8_t) m_data[currentPosition + i];

	currentPosition += size;
	return number;
}

long PcapReaderFromVector::readIntLittleEndian(const size_t &size)
{
	long number = 0;

	for (size_t i = 0; i < size; i++)
		number = number << BYTE_SIZE | m_data[currentPosition + i];

	currentPosition += size;
	return number;
}

string PcapReaderFromVector::readString(const size_t &size,
	const string &separator, const bool &hexFormat)
{
	stringstream stream;

	for (size_t i = 0; i < size; i++) {
		if (hexFormat)
			stream << setfill('0') << setw(2) << hex << (uint32_t) (uint8_t)m_data[currentPosition +i];
		else
			stream << (uint32_t) (uint8_t)m_data[currentPosition +i];

		stream << ((i+1 == size) ? "" : separator);
	}
	currentPosition += size;
	return stream.str();
}

string PcapReaderFromVector::readIPv6(const size_t &size)
{
	stringstream stream;

	for (size_t i = 0; i < size; i++) {
		stream << setfill('0') << setw(2) << hex << (uint32_t) (uint8_t)m_data[currentPosition + i];
		if ((i+1)%2 == 0 && i != 0 && i+1 != size)
			stream << ":";
	}

	currentPosition += size;
	return stream.str();
}

std::vector<uint8_t> PcapReaderFromVector::readUint8Vector(const size_t &size)
{
	std::vector<uint8_t> data;

	for (size_t i = 0; i < size && currentPosition < m_data.size(); i++) {
		data.push_back(m_data[currentPosition]);

		currentPosition++;
	}

	return data;
}

void PcapReaderFromVector::skip(const size_t &size)
{
	currentPosition += size;
}

std::vector<uint8_t> PcapReaderFromVector::readToEnd()
{
	std::vector<uint8_t> data;

	for (size_t i = currentPosition; i < m_data.size(); i++)
		data.push_back(m_data[i]);

	currentPosition = m_data.size() - 1;
	return data;
}
