#include <iostream>
#include "Strategy.h"
#include <memory>
#include "GameBoard.h"
#include <time>

using namespace std;

Strategy::Strategy() :
	board(nullptr)
{}

Alg1::Alg1(shared_ptr<GameBoard> board)
{
	this->board = board;
}

int Alg1::executeMove(TPoint* point)
{
	int max = 0;
	int deskSize = board->getDeskSize();
	vector <TPoint> points;
	for(int i = 0; i < deskSize; i++) {
		for(int j = 0; j < deskSize; j++) {
			points = findingMoves({i, j}, BLACK);
			if(points.size() > max) {
				point.x = i;
				point.y = j;
			}
		}
	}
	return 10;
}


Alg2::Alg2(shared_ptr<GameBoard> board)
{
	this->board = board;
}

int Alg2::executeMove(TPoint* point) {
	/*int x = 5, y = 4;
	cout << "SURADNICE PC:::";
	cin >>x; cin >> y;
	point->x = x; point->y = y;
	cout << board->getDeskSize();
	return 20;*/

	int max = 0;
	int deskSize = board->getDeskSize();
	vector <TPoint> points;
	vector <TPoint> pointsPossib;
	for(int i = 0; i < deskSize; i++) {
		for(int j = 0; j < deskSize; j++) {
			points = findingMoves({i, j}, BLACK);
			if(points.size() > 0) {
				pointsPossib.push_back({i, j});
			}
		}
	}
	point = pointsPossib[rand() % deskSize];
	return 20;
}
