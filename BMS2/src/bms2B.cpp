#include <iostream>

#include "Demodulator.h"
#include "ModulatorException.h"

using namespace std;

int main(int argc, char** argv)
{
	if (argc != 2) {
		cerr << "wrong arguments were entered" << endl;
		return EXIT_FAILURE;
	}

	try {
		Demodulator demodulator;
		demodulator.setInputFileName(argv[1]);
		demodulator.process();
		demodulator.save();
	}
	catch (const ModulatorException &ex) {
		cerr << ex.message() << endl;
		return EXIT_FAILURE;
	}
	catch (const std::exception &ex) {
		cerr << ex.what() << endl;
		return EXIT_FAILURE;
	}
	catch (...) {
		cerr << "unknown error" << endl;
		return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}
