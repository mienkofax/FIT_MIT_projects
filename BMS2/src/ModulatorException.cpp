#include "ModulatorException.h"

using namespace std;

ModulatorException::ModulatorException(const string &message):
	m_message(message)
{
}

string ModulatorException::message() const
{
	return m_message;
}
