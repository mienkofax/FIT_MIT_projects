#include <iostream>
#include "Strategy.h"

using namespace std;

int Alg1::executeMove(TPoint* point) {
	return 10;
}

int Alg2::executeMove(TPoint* point) {
	int x = 5, y = 4;
	cout << "SURADNICE PC:::";
	cin >>x; cin >> y;
	point->x = x; point->y = y;
	return 20;
}
