/**
 * @file PcapReader.cpp
 * @Author Peter Tisovčík <xtisov00@stud.fit.vutbr.cz>
 * @date October, 2016
 */

#include <sstream>

#include "PcapReader.h"

using namespace std;

PcapReaderFromFile::PcapReaderFromFile(ifstream &file):
	m_file(file)
{
}

string PcapReaderFromFile::readString(size_t size, string separator)
{
	char *buffer = read(size);
	stringstream stream;

	for (size_t i = 0; i < size; i++) {
		stream << setfill('0') << setw(2) << hex << (uint32_t) (uint8_t)buffer[i];
		stream << (i+1 == size ? "" : separator);
	}

	return stream.str();
}

char *PcapReaderFromFile::read(size_t size)
{
	char *buffer = new char[size];
	m_file.read(buffer, size);

	if (!m_file)
		throw invalid_argument("problem pri citani suboru");

	return buffer;
}

long PcapReaderFromFile::readIntBigEndian(size_t size)
{
	char *buffer = read(size);
	long number = 0;

	for (size_t i = 0; i < size; i++)
		number |=(uint8_t) (buffer[i]) << i * 8;

	return number;
}

long PcapReaderFromFile::readIntLittleEndian(size_t size)
{
	char *buffer = read(size);
	long number = 0;

	for (size_t i = 0; i < size; i++)
		number = number << 8 | (uint8_t) (buffer[i]);

	return number;
}

std::vector<uint8_t> PcapReaderFromFile::readUint8Vector(size_t size)
{
	std::vector<uint8_t> data;
	char *buffer = read(size);

	for (size_t i = 0; i < size; i++)
		data.push_back((uint8_t) buffer[i]);

	return data;
}

void PcapReaderFromFile::skip(size_t size)
{
	m_file.seekg(size, std::ios::cur);
}

PcapReaderFromVector::PcapReaderFromVector(std::vector<uint8_t> data):
	m_data(data),
	currentPosition(0)
{
}

long PcapReaderFromVector::readIntBigEndian(size_t)
{
	throw runtime_error("unsupported method readIntBigEndian");
}

long PcapReaderFromVector::readIntLittleEndian(size_t size)
{
	long number = 0;

	for (size_t i = 0; i < size; i++)
		number = number << 8 | m_data[currentPosition + i];

	currentPosition += size;
	return number;
}

string PcapReaderFromVector::readString(size_t size, string separator)
{
	stringstream stream;

	for (size_t i = 0; i < size; i++)
		stream << setfill('0') << setw(2) << hex << (uint32_t) (uint8_t)m_data[currentPosition +i] << separator;

	currentPosition += size;
	return stream.str();
}

std::vector<uint8_t> PcapReaderFromVector::readUint8Vector(size_t size)
{
	std::vector<uint8_t> data;

	for (size_t i = 0; i < size; i++)
		data.push_back(m_data[currentPosition + i]);

	currentPosition += size;
	return data;
}

void PcapReaderFromVector::skip(size_t size)
{
	currentPosition += size;
}
