/**
 * @file PcapReader.cpp
 * @Author Peter Tisovčík <xtisov00@stud.fit.vutbr.cz>
 * @date October, 2016
 */

#include "PcapReader.h"

using namespace std;

PcapReaderFromFile::PcapReaderFromFile(ifstream &file):
	m_file(file)
{
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

//	cout << "num: ";
 //       cout << setfill('0') << setw(2) << hex << (uint8_t)m_data[4] << endl;

	for (size_t i = 0; i < size; i++)
		number = number << 8 | m_data[currentPosition + i];

	currentPosition += size;
	return number;
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
