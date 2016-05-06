/**
 * Strategy
 *
 * @author			Klára Nečasová <xnecas24>
 * @author			Peter Tisovčík <xtisov00>
 */

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
	int deskSize = bo.getDeskSize();
	vector <TPoint> points;
	bool move = false;

	//prechod bodmi a najdenie prvych validnych suradnic
	for(int i = 0; i < deskSize && !move; i++) {
		for(int j = 0; j < deskSize; j++) {
			points = bo.moveStone({j,i}, BLACK);
			if(points.size() > 0) {
				point->x = j;
				point->y = i;
				move = true;
			}
		}
	}

	if (!move)
		return true;

	return false;
}

bool Alg2::executeMove(TPoint* point,  GameBoard bo) {
	int deskSize = bo.getDeskSize();
	vector <TPoint> points;
	vector <TPoint> pointsPossib;

	//prechod bodmi, ulozenie validnych suradnic do vektoru
	for(int i = 0; i < deskSize; i++) {
		for(int j = 0; j < deskSize; j++) {
			points = bo.moveStone({j,i}, BLACK);
			if(points.size() > 0)
				pointsPossib.push_back({j,i});
		}
	}

	if (pointsPossib.size() == 0)
		return true;

	//nahodny vyber suradnic, ktore su validne
	*point = pointsPossib[rand() % pointsPossib.size()];
	return false;
}
