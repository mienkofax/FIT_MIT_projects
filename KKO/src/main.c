/**
 * @file main.c
 * @Author Peter Tisovčík (xtisov00) <xtisov00@stud.fit.vutbr.cz>
 * @date 18 February, 2018
 * @brief Subor s ukazkou aplikacie gif2bmp(...) funkcie pre prevedenie
 * gif brazku do bmp.
 */

#include <stdlib.h>
#include <getopt.h>
#include <cstdio>

#include "gif2bmp.h"

using namespace std;

const string helpMessage =
R"(Convert gif image to bmp file.
Parameters:
	-h help message
	-i <file> input file
	-o <file> output file
	-l <file> log file with input gif size and output bmp size)";

class Inputs {
public:
	Inputs():
		input(stdin),
		output(stdout)
	{
	}

	~Inputs()
	{
		close(input);
		close(output);
	}

private:
	void close(FILE *file)
	{
		if (file != nullptr)
			fclose(file);
	};

public:
	FILE* input;
	FILE* output;
};

int main(int argc, char *argv[])
{
	Inputs files;
	string logFileName = "";

	if (argc > 7)
		cerr << "too many arguments" << endl;

	int c;
	while ((c = getopt(argc, argv, "hi:o:l:")) != -1) {
		switch (c) {
		case 'h':
			cout << helpMessage << endl;
			return EXIT_SUCCESS;
		case 'i':
			if ((files.input = fopen(optarg, "rb")) == NULL) {
				cerr << "problem with read input file" << endl;
				return EXIT_FAILURE;
			}
			break;
		case 'o':
			if ((files.output = fopen(optarg, "wb")) == NULL) {
				cerr << "problem with open output file" << endl;
				return EXIT_FAILURE;
			}
			break;
		case 'l':
			logFileName = optarg;
			break;
		default:
			cerr << "invalid argument" << endl;
			return EXIT_FAILURE;
		}
	}

	tGIF2BMP stat;
	int retCode = gif2bmp(&stat, files.input, files.output);

	if (retCode == 0 && !logFileName.empty()) {
		ofstream out(logFileName);

		if (out.is_open()) {
			out << "login = xtisov00" << endl;
			out << "uncodedSize = " << stat.bmpSize << endl;
			out << "codedSize = " << stat.gifSize << endl << endl;
			out.close();
		}
		else {
			cerr << "problem with save data to " + logFileName << endl;
		}
	}

	return retCode;
}
