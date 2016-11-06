/**
 * @file PcapReader.cpp
 * @Author Peter Tisovčík <xtisov00@stud.fit.vutbr.cz>
 * @date October, 2016
 */

#include <stdexcept>
#include <sstream>

#include "PcapReaderFromFile.h"

using namespace std;

#define BYTE_SIZE   8

long PcapReaderFromFile::readIntBigEndian(const size_t &size)
{
	char *buffer = read(size);
	long number = 0;

	for (size_t i = 0; i < size; i++)
		number |= (uint8_t) (buffer[i]) << i * BYTE_SIZE;

	delete [] buffer;
	return number;
}

long PcapReaderFromFile::readIntLittleEndian(const size_t &size)
{
	char *buffer = read(size);
	long number = 0;

	for (size_t i = 0; i < size; i++)
		number = number << BYTE_SIZE | (uint8_t) (buffer[i]);

	delete [] buffer;
	return number;
}

string PcapReaderFromFile::readString(const size_t &size,
	const string &separator, const bool &hexFormat)
{
	char *buffer = read(size);
	stringstream stream;

	for (size_t i = 0; i < size; i++) {
		if (hexFormat)
			stream << setfill('0') << setw(2) << hex
				<< (uint32_t) (uint8_t)buffer[i];
		else
			stream << (uint32_t) (uint8_t)buffer[i];

		stream << ((i + 1) == size ? "" : separator);
	}

	delete [] buffer;
	return stream.str();
}

std::vector<uint8_t> PcapReaderFromFile::readUint8Vector(const size_t &size)
{
	std::vector<uint8_t> data;
	char *buffer = read(size);

	for (size_t i = 0; i < size; i++)
		data.push_back((uint8_t) buffer[i]);

	delete [] buffer;
	return data;
}

void PcapReaderFromFile::skip(const size_t &size)
{
	m_file.seekg(size, std::ios::cur);
}

char *PcapReaderFromFile::read(const size_t &size)
{
	char *buffer = new char[size];
	m_file.read(buffer, size);

	if (!m_file.good() || !m_file) {
		delete buffer;
		throw invalid_argument("Problem with read from file");
	}

	return buffer;
}
