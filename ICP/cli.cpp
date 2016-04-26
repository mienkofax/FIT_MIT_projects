#include <iostream>
#include "cli.h"

using namespace std;

void CLI::drawGameDesk(int deskSize)
{
	cout << "   ";
	for (int i = 0; i < deskSize; i++)
		cout << i << " ";

	cout << endl;

	for (int i = 0; i < deskSize; i++){
		cout << i << ": ";
		for (int j = 0; j < deskSize; j++) {

		}
		cout << endl;
	}
}
