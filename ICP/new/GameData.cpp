#include <iostream>
#include "GameData.h"
#include <string>
#include <fstream>
#include <memory>

using namespace std;

GameData::GameData() :
	history(vector <TGameMove> ())
{

}

//prida zadane body do historie
void GameData::addMoveToHistory(vector <TPoint> points, int color)
{
	int moveNumber;

	//zistenie posledneho tahu, ak este nebol ziaden tah vytvori sa 1. tah
	if (this->history.size() == 0)
		moveNumber = 1;
	else
		moveNumber = this->history.back().moveNumber + 1;

	this->history.push_back({moveNumber, points, color, true});
}

bool GameData::saveGameData(string filename, int deskSize, int players)
{
	ofstream gameFile;
	gameFile.open(filename);

	if (!gameFile.is_open())
		return false;

	//prechod jednotlivymi polozkami v historii
	gameFile << deskSize << endl;
	gameFile << players << endl;
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

bool GameData::loadGameData(string filename)
{
	ifstream gameFile;
	gameFile.open(filename);

	if (!gameFile.is_open())
		return false;

	int countMove;
	int x,y;
	TGameMove record;
	record.isActive = true;
	gameFile >> countMove >> countMove; //tmp data deskSize
	while (gameFile >> record.moveNumber >> record.color >> countMove) {
		record.points.clear();
		for (int i = 0; i < countMove; i++) {
			gameFile >> x >> y;
			record.points.push_back({x, y});
		}
		this->history.push_back(record);

	}
}
vector <TGameMove> GameData::getHistory()
{
	return this->history;
}

vector <TGameMove>  GameData::undo() {
	vector <TGameMove> moves;

	for (vector<TGameMove>::reverse_iterator rit = history.rbegin();
			rit != history.rend(); ++rit) {
				if ((*rit).isActive) {
					(*rit).isActive = false;
					break;
				}
			}

	for (TGameMove move : history)  {
		if (!move.isActive)
			break;
		moves.push_back(move);
	}


	return moves;
}

bool GameData::getHa() {
	int count = 0;
	for (TGameMove move: history) {
		if (move.isActive)
		 	count++;
	}
	cout << "pocet aktivnych::" << count << endl;
	if (count > 0)
		return true;

	return false;
}

bool GameData::getHa2 () {
	int count = 0;
	for (TGameMove move: history) {
		if (move.isActive)
		 	count++;
	}
	cout << "pocet aktivnych::" << count << endl;
	if (count == history.size())
		return false;

	return true;
}

void GameData::removeInvalidData() {
	int index = 0;
	for (TGameMove move : history) {
		if (!move.isActive)
			break;
		index++;
	}
	cout << "INDEX:: " << index << endl;

	while (history.size() > index)
		history.pop_back();

}

vector <TGameMove> GameData::redo() {
	vector <TGameMove> moves;

	for (TGameMove &move : history) {
		if (!move.isActive) {
			move.isActive = true;
			break;
		}

	}


	for (TGameMove move : history)  {
		if (!move.isActive)
			break;
		moves.push_back(move);
	}

	return moves;
}
