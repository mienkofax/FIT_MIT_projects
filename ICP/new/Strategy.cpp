#include <iostream>
#include "Strategy.h"
#include <memory>
#include "GameBoard.h"

using namespace std;

Strategy::Strategy() :
	board(nullptr)
{}

Alg1::Alg1(shared_ptr<GameBoard> board)
{
	this->board = board;
}

int Alg1::executeMove(TPoint* point) {
	return 10;
}


Alg2::Alg2(shared_ptr<GameBoard> board)
{
	this->board = board;
}

int Alg2::executeMove(TPoint* point) {
	int x = 5, y = 4;
	cout << "SURADNICE PC:::";
	cin >>x; cin >> y;
	point->x = x; point->y = y;
	cout << board->getDeskSize();
	return 20;
}
