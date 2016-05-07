/**
 * @file			GameData.cpp
 * @author			Klára Nečasová <xnecas24>
 * @author			Peter Tisovčík <xtisov00>
 */

#include <iostream>
#include <string>
#include <fstream>
#include <memory>
#include "GameData.h"

using namespace std;

GameData::GameData() :
	history(vector <TGameMove> ())
{}

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

bool GameData::saveGameData(string filename, int deskSize, int players, int algorithm)
{
	ofstream gameFile;
	gameFile.open(filename);

	if (!gameFile.is_open())
		return false;

	//prechod jednotlivymi polozkami v historii
	gameFile << deskSize << endl;
	gameFile << players << endl;
	gameFile << algorithm << endl;
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
	int countMove, x, y;
	ifstream gameFile;
	gameFile.open(filename);

	if (!gameFile.is_open())
		return false;

	TGameMove record;
	record.isActive = true;
	gameFile >> countMove >> countMove;

	while (gameFile >> record.moveNumber >> record.color >> countMove) {
		record.points.clear();
		for (int i = 0; i < countMove; i++) {
			gameFile >> x >> y;
			record.points.push_back({x, y});
		}
		this->history.push_back(record);
	}
	return true;
}

vector <TGameMove>  GameData::undo() {
	vector <TGameMove> moves;

	//prechd vektorom od konca a najdene prveho tahu, ktory sa ma zmenit
	for (vector<TGameMove>::reverse_iterator rit = history.rbegin();
			rit != history.rend(); ++rit) {
				if ((*rit).isActive) {
					(*rit).isActive = false;
					break;
				}
			}

	//tahy, ktore sa maju zmenit
	for (TGameMove move : history)  {
		if (!move.isActive)
			break;
		moves.push_back(move);
	}

	return moves;
}

vector <TGameMove> GameData::redo() {
	vector <TGameMove> moves;

	//prechod historiou a najdenie tahu, ktory bol spat
	for (TGameMove &move : history) {
		if (!move.isActive) {
			move.isActive = true;
			break;
		}

	}

	//pridanie tahov, ktore sa maju prekreslit
	for (TGameMove move : history)  {
		if (!move.isActive)
			break;
		moves.push_back(move);
	}

	return moves;
}

vector <TGameMove> GameData::getHistory()
{
	return this->history;
}

void GameData::removeInvalidData() {
	size_t index = 0;

	//spocita sa kolko je mozne spravit krookov vpred
	for (TGameMove move : history) {
		if (!move.isActive)
			break;
		index++;
	}

	while (history.size() > index)
		history.pop_back();
}

bool GameData::checkUndo() {
	int count = 0;

	//spocita sa kolko je mozne este spravit navratov
	for (TGameMove move: history) {
		if (move.isActive)
		 	count++;
	}

	if (count > 0)
		return true;

	return false;
}

bool GameData::checkRedo() {
	size_t count = 0;
	for (TGameMove move: history) {
		if (move.isActive)
		 	count++;
	}

	if (count == history.size())
		return false;

	return true;
}
