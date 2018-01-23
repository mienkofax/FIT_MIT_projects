#include <fstream>
#include <iostream>

#include "sndfile.hh"

#include "Modulator.h"
#include "ModulatorException.h"

using namespace std;

#define SAMPLE_RATE 18000
#define CHANELS 1
#define FORMAT (SF_FORMAT_WAV | SF_FORMAT_PCM_24)
#define AMPLITUDE (1.0 * 0x7F000000)
#define FREQ (1000.0 / SAMPLE_RATE)
#define SAMPLES 15

const double CALC = FREQ * 2 * M_PI;

Modulator::Modulator():
	m_length(0)
{
}

Modulator::~Modulator()
{
	delete [] m_buffer;
}

void Modulator::loadData(queue<char> &q)
{
	ifstream stream;
	stream.open(inputFileName().c_str());

	if (!stream.is_open())
		throw ModulatorException("problem with open input file");

	char c;
	// load 0 or 1 to queue
	while (stream.get(c)) {
		if (c == '0' || c == '1') {
			q.push(c);
			continue;
		}

		// skip character of new line
		if (c == '\n' || c == '\r')
			continue;

		throw ModulatorException("unsupported character in input file");
	}

	if (q.size() % 2 != 0)
		throw ModulatorException("file contains an odd number of numbers");

	stream.close();
}

void Modulator::setSyncSequence(std::queue<char> &q)
{
	for (char c : SYNC_SEQUENCE)
		q.push(c);
}

void Modulator::process()
{
	queue<char> q;

	setSyncSequence(q);
	loadData(q);

	m_buffer = new int[q.size() * SAMPLES];

	long i = 0;
	while (!q.empty()) {
		char c1, c2;

		c1 = q.front();
		q.pop();
		c2 = q.front();
		q.pop();

		for (int j = 0; j < SAMPLES; i++, j++)
			modulate(c1, c2, i);
	}

	m_length = i;
}

void Modulator::save()
{
	SndfileHandle outputFile;
	outputFile = SndfileHandle(rawFileName(inputFileName(), "txt") + ".wav", SFM_WRITE, FORMAT, CHANELS, SAMPLE_RATE);

	outputFile.write(m_buffer, m_length);
}

void Modulator::modulate(char c1, char c2, long i)
{
	if (c1 == '0' && c2 == '0')
		m_buffer[i] = int(AMPLITUDE * sin(CALC * i + PHASE_SHIFT_3p4));

	if (c1 == '0' && c2 == '1')
		m_buffer[i] = int(AMPLITUDE * sin(CALC * i + PHASE_SHIFT_1p4));

	if (c1 == '1' && c2 == '0')
		m_buffer[i] = int(AMPLITUDE * sin(CALC * i + PHASE_SHIFT_5p4));

	if (c1 == '1' && c2 == '1')
		m_buffer[i] = int(AMPLITUDE * sin(CALC * i + PHASE_SHIFT_7p4));
}
