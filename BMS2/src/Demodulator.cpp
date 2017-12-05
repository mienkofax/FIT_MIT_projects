#include <map>
#include <iostream>
#include <queue>
#include <fstream>

#include "sndfile.hh"
#include "Demodulator.h"
#include "ModulatorException.h"

#define SAMPLE_RATE 18000
#define CHANELS 1
#define FORMAT (SF_FORMAT_WAV | SF_FORMAT_PCM_24)
double AMPLITUDE = (1.0 * 0x7F000000);
#define FREQ (1000.0 / SAMPLE_RATE)
#define SAMPLES 15

const double CALC = FREQ * 2 * M_PI; //todo dorobit kontrolu f

using namespace std;

static const map<string, double> PHASE = {
	{"00", PHASE_SHIFT_3p4},
	{"01", PHASE_SHIFT_1p4},
	{"10", PHASE_SHIFT_5p4},
	{"11", PHASE_SHIFT_7p4},
};

Demodulator::Demodulator():
	m_format(0),
	m_eps(0),
	m_phases(PHASE)
{
}

void Demodulator::setEps(int format)
{
	if ((format & SF_FORMAT_SUBMASK) == SF_FORMAT_PCM_32)
		m_eps = 0;
	else if ((format & SF_FORMAT_SUBMASK) == SF_FORMAT_PCM_24)
		m_eps = (int) pow(2, 8);
	else if ((format & SF_FORMAT_SUBMASK) == SF_FORMAT_PCM_16)
		m_eps = (int) pow(2, 16);

	else ModulatorException("unsupported PCM format");
}

bool Demodulator::equalSamples(int sample1, int sample2) const
{
	return abs(sample1 - sample2) <= m_eps;
}

void Demodulator::loadSamples()
{
	int sample = 0;

	SndfileHandle inputFile;
	inputFile = SndfileHandle(inputFileName().c_str());

	// load format from file
	m_format = inputFile.format();

	// check that format is wav
	if ((m_format & SF_FORMAT_TYPEMASK) != SF_FORMAT_WAV)
		throw ModulatorException("invalid input wav format");

	// copy data to vector
	while (inputFile.read(&sample, 1) == 1)
		m_list.push_back(sample);
}

void Demodulator::shiftPhases()
{
	for (auto &phase : m_phases)
		phase.second = 2 * M_PI - phase.second;
}

void Demodulator::save()
{
	string file = rawFileName(inputFileName(), "wav") + ".txt";
	ofstream out(file);

	if (!out.is_open())
		cerr << "problem with save data to " + file;

	if (m_outputData.empty())
		return;

	for (auto &item : m_outputData)
		out << item;

	out << "\n";
	out.close();
}

string Demodulator::detectPhase(int pocet, int startIndex)
{
	int refSample = 0;
	int loadedSample = 0;
	int originalIndex = startIndex;

	for (auto &phase : m_phases) {
		startIndex = originalIndex;

		for (int i = 0; i < pocet; i++, startIndex++) {
			refSample = int(AMPLITUDE * sin(CALC * startIndex + phase.second));
			loadedSample = m_list.at(startIndex);

			if (!equalSamples(refSample, loadedSample))
				continue;

			if (i == pocet - 1)
				return phase.first;
		}
	}

	throw ModulatorException("incorrect phase");
}

int Demodulator::detectCountOfSamples()
{
	size_t index = 0;
	int refSample = int(AMPLITUDE * sin(CALC * index + m_phases["00"]));
	int loadedSample = m_list.at(index);

	// start compare with default phase shift
	if (!equalSamples(refSample, loadedSample)) {
		shiftPhases();

		refSample = int(AMPLITUDE * sin(CALC * index + m_phases["00"]));
		if (!equalSamples(refSample, loadedSample))
			throw ModulatorException("invalid sync sequence");
	}

	// move to next sample
	index++;

	// detect phase and count of samples in phase shift
	while (index < m_list.size()) {
		refSample = int(AMPLITUDE * sin(CALC * index + m_phases["00"]));
		loadedSample = m_list.at(index);

		if (!equalSamples(refSample, loadedSample)) {
			refSample = int(AMPLITUDE * sin(CALC * index + m_phases["11"]));

			if (!equalSamples(refSample, loadedSample))
				throw ModulatorException("incorrect sync sequence");
			else
				break;
		}
		else {
			index++;
		}
	}

	return index;
}

void Demodulator::process()
{
	loadSamples();
	setEps(m_format);

	int countSamples = detectCountOfSamples();

	// if any signal unit is made of different samples
	if (m_list.size() % countSamples !=0)
		throw ModulatorException("count of sample is differ");

	if (!checkSyncSequence(countSamples))
		throw ModulatorException("invalid sync sequence");

	extractDataFromSamples(countSamples);
}

bool Demodulator::checkSyncSequence(int countSamples)
{
	// i - iteration over the samples
	// j - iteration over pair of bits (signal unit)
	for (size_t i = 0, j = 0; j < SYNC_SEQUENCE.size(); i++, j += 2) {
		const string &loadedPhase = detectPhase(countSamples, i * countSamples);
		const string &refPhase = SYNC_SEQUENCE.substr(j, 2);

		if (loadedPhase != refPhase)
			return false;
	}

	return true;
}

void Demodulator::extractDataFromSamples(int countSamples)
{
	// count of data, which are present on the input
	const size_t dataSize = m_list.size() / countSamples;

	// count of sync data, which are present on the input
	const size_t syncSize = SYNC_SEQUENCE.size() / 2;

	for (size_t i = syncSize; i < dataSize; i++)
		m_outputData.push_back(detectPhase(countSamples, i * countSamples));
}
