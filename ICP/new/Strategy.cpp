#include <iostream>
#include "Strategy.h"
#include <memory>
#include "GameBoard.h"
#include <ctime>

using namespace std;

Strategy::Strategy()
{}

bool Alg1::executeMove(TPoint* point,  GameBoard bo)
{
	int max = 0;
	int deskSize = bo.getDeskSize();
	vector <TPoint> points;

	for(int i = 0; i < deskSize; i++) {
		for(int j = 0; j < deskSize; j++) {
			points = bo.moveStone({j,i}, BLACK);
			if(points.size() > max) {
				point->x = j;
				point->y = i;
				max = points.size();
			}
		}
	}

	//ak neexistuje tah vrati sa false
	if (max == 0)
		return false;

	return true;
}

bool Alg2::executeMove(TPoint* point,  GameBoard bo) {
	int max = 0;
	int deskSize = bo.getDeskSize();
	vector <TPoint> points;
	vector <TPoint> pointsPossib;

	for(int i = 0; i < deskSize; i++) {
		for(int j = 0; j < deskSize; j++) {
			points = bo.moveStone({j,i}, BLACK);
			if(points.size() > 0)
				pointsPossib.push_back({j,i});
		}
	}

	if (pointsPossib.size() == 0)
		return false;

	*point = pointsPossib[rand() % pointsPossib.size()];
	return true;
}
