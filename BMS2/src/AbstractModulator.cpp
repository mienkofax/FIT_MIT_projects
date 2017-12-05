#include <iostream>
#include "AbstractModulator.h"
#include "ModulatorException.h"

using namespace std;

AbstractModulator::~AbstractModulator()
{
}

void AbstractModulator::setInputFileName(const string &filename)
{
	m_inputFileName = filename;
}

string AbstractModulator::inputFileName() const
{
	return m_inputFileName;
}

string AbstractModulator::rawFileName(const string &filename, const string &extension) const
{
	// last start index of extension in filename
	size_t index = filename.find_last_of(".");

	// if extension is not found
	if (index == string::npos)
		throw ModulatorException("missing *." + extension + " file extension");

	// compare extension with filename extension
	if (index != filename.size() - 4)
		throw ModulatorException("missing *." + extension + " file extension");

	if (filename.substr(index + 1, filename.size() - 1) != extension)
		throw ModulatorException("missing *." + extension + " file extension");

	return filename.substr(0, index);
}
