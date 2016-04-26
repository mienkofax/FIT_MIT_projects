#include <iostream>
#include "game.h"
#include <string>
#include <fstream>
#include <memory>

using namespace std;

OthelloGame::OthelloGame(int deskSize) :
	board(new OthelloBoard(deskSize)),
	history(vector <TGameMove> ())
{

}
void OthelloGame::setPlayer(int player1, int player2)
{
	//nastavenie hraca jedna
	if (player1 == PLAYER_PC)
		this->p1 = new Pc();
	else
		this->p1 = new Person();

	//nastavenie aby mohol tahat hrac p1
	this->p1->move = true;

	//nastavenie hraca dva
	if (player2 == PLAYER_PC)
		this->p2 = new Pc();
	else
		this->p2 = new Person();

	this->p1->color = WHITE;
	this->p2->color = BLACK;
}
void OthelloGame::nextPlayer()
{
	if (this->p1->move) {
		this->p1->move = false;
		this->p2->move = true;
	} else {
		this->p1->move = true;
		this->p2->move = false;
	}
}
int OthelloGame::getActPlayerColor()
{
	if (this->p1->move)
		return this->p1->color;
	else
		return this->p2->color;
}
void OthelloGame::addMoveToHistory(vector <TPoint> points, int color)
{
	int moveNumber;

	//zistenie posledneho tahu, ak este nebol ziaden tah vytvori sa 1. tah
	if (this->history.size() == 0)
		moveNumber = 1;
	else
		moveNumber = this->history.back().moveNumber + 1;

	this->history.push_back({moveNumber, points, color});
}
bool OthelloGame::move(TPoint point, int color, bool isPass)
{
	vector <TPoint> points;
	int count;
	bool returnCode = false;

	points = board->move(point, color);
	count = points.size();

	//ak bol tah dobry vrati sa aspon jedna suradnica, ktora sa prekreslila
	if (count > 0) {
		points.push_back(point);
		addMoveToHistory(points, color);
		returnCode = true;
	}

	if (count > 0 && isPass)
		returnCode = true;

	return returnCode;
}

bool OthelloGame::saveGameData(string filename)
{
	ofstream gameFile;
	gameFile.open(filename);

	if (!gameFile.is_open())
		return false;

	//prechod jednotlivymi polozkami v historii
	for (TGameMove record : this->history)
	{
		gameFile << record.moveNumber << endl;
		gameFile << record.color << endl;
		gameFile << record.points.size() << endl;
		for (TPoint point : record.points) {
			gameFile << point.x << " " << point.y << endl;
		}
	}

	gameFile.close();

	return true;
}
bool OthelloGame::loadGameData(string filename)
{
	ifstream gameFile;
	gameFile.open(filename);

	if (!gameFile.is_open())
		return false;

	int countMove;
	int x,y;
	TGameMove record;
	while (gameFile >> record.moveNumber >> record.color >> countMove) {
		for (int i = 0; i < countMove; i++) {
			gameFile >> x >> y;
			cout << x << "," << y << endl;
			record.points.push_back({x, y});
		}
		this->history.push_back(record);

	}

	updateGameBoard();
}
void OthelloGame::updateGameBoard()
{
	for (TGameMove move : this->history)
		for (TPoint point : move.points)
			board->setField(point, move.color);
}





///
