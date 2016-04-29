#include <iostream>
#include "GameManager.h"
#include "GameBoard.h"
#include "GameData.h"
#include "Player.h"
#include <fstream>

using namespace std;

GameManager::GameManager() : games(vector <TGame> ()), activeGameIndex(0),firstGame(true)
{

}

//vytvorenie novej hry
void GameManager::newGame(int deskSize, int players)
{
	Player *p1 = new Human();
	Player *p2;
	if (players == 1) {
		p2 = new PC(new Alg2());
	} else
		p2 = new Human();


	p1->active = true;
	p2->active = false;//kvoli prechodom medzi hrami
	p1->setColor(WHITE);
	p2->setColor(BLACK);

	if (!firstGame)
		activeGameIndex++;
	else
		firstGame = false;

	this->games.push_back({GameBoard(deskSize), GameData(), p1, p2});
	game = &games[this->activeGameIndex];
	getBoard();
}

int GameManager::getStone(TPoint point)
{
	return games[this->activeGameIndex].board.getStone(point);
}

void GameManager::getBoard() {
	vector <TPoint> points;

	int color;

	if(isActiveP1()) {

		color = games[this->activeGameIndex].p1->getColor();
	} else {
		color = games[this->activeGameIndex].p2->getColor();
	}

	cout << color;

	int deskSize = 8;
	for (int i = 0; i < deskSize; i++) {
		for (int j = 0; j < deskSize; j++) {
			if (games[this->activeGameIndex].board.getStone({i,j}) == 8)
				games[this->activeGameIndex].board.setStone({i,j}, NON_DEFINE);
			points = games[this->activeGameIndex].board.testStone({i,j}, color);
		}
	}

	for (int p : games[this->activeGameIndex].board.gameDesk) {
		cout << p;
	}

}

int GameManager::moveStone(TPoint point, bool isPass)
{


	int color;
	vector <TPoint> points;
	int count;
	bool returnCode = false;

	if(isActiveP1()) {
		games[this->activeGameIndex].p1->getNextMove(&point);
		color = games[this->activeGameIndex].p1->getColor();
	} else {
		games[this->activeGameIndex].p2->getNextMove(&point);
		color = games[this->activeGameIndex].p2->getColor();
	}



	points = games[this->activeGameIndex].board.moveStone(point, color);
	count = points.size();

	//ak bol tah dobry vrati sa aspon jedna suradnica, ktora sa prekreslila
	if (count > 0) {
		if (activeRedoUndo) {
			games[this->activeGameIndex].data.removeInvalidData();
			activeRedoUndo = false;
		}

		points.push_back(point);
		games[this->activeGameIndex].data.addMoveToHistory(points, color);
		returnCode = true;
	}

	if (count > 0 && isPass)
		returnCode = true;

	if (returnCode) {
		nextPlayer();



	}
	getBoard();

	updateScore();

	return returnCode;
}

void GameManager::nextPlayer() {
	if (isActiveP1()) {
		games[this->activeGameIndex].p1->active = false;
		games[this->activeGameIndex].p2->active = true;
	} else
	{
		games[this->activeGameIndex].p1->active = true;
		games[this->activeGameIndex].p2->active = false;
	}
}

void GameManager::updateScore() {
	int colorP1 = games[this->activeGameIndex].p1->getColor();
	int colorP2 = games[this->activeGameIndex].p2->getColor();

	game->p1->setScore(games[this->activeGameIndex].board.getCountStone(colorP1));
	game->p2->setScore(games[this->activeGameIndex].board.getCountStone(colorP2));
}

bool GameManager::loadGame(string filename)
{
	ifstream gameFile;
	gameFile.open(filename);

	if (!gameFile.is_open())
		return false;

	int deskSize, players;
	gameFile >> deskSize;
	gameFile >> players;
	gameFile.close();

	newGame(deskSize, players);

	bool returnCode = games[this->activeGameIndex].data.loadGameData(filename);

	cout << endl;
	for (TGameMove move : games[this->activeGameIndex].data.getHistory()) {
			games[this->activeGameIndex].board.updateBoard(move.points, move.color);

			for (TPoint p : move.points)
				cout << p.x << "," << p.y << "," << move.color << endl;
	}

	return returnCode;
}

bool GameManager::saveGame(string filename)
{
	if (games.size() == false) return false;

	int players = 2;

	if (games[this->activeGameIndex].p2->getType() == 1)
		players = 1;

	return games[this->activeGameIndex].data.saveGameData(filename, games[this->activeGameIndex].board.getDeskSize(), players);
}

bool GameManager::isActiveP1()
{
	if (games.size() == 0) return false;
	return games[this->activeGameIndex].p1->active;
}

bool GameManager::isActiveP2()
{
	if (games.size() == 0) return false;
	return games[this->activeGameIndex].p2->active;
}
bool GameManager::livePlayer() {

	if (isActiveP2() && games[this->activeGameIndex].p2->typ == 1)
		return false;

	return true;
}
bool GameManager::isEmpty()
{
	if (games.size() == 0)
		return true;
	return false;
}
int GameManager::getGameID() {
	if (isEmpty())
		return -1;
	return this->activeGameIndex;
}
int GameManager::getDeskSize() {
	return games[this->activeGameIndex].board.getDeskSize();
}
bool GameManager::changeGame(int gameIndex) {
	if (gameIndex > games.size() || gameIndex < 0)
		return false;

	activeGameIndex = gameIndex;


	game = &games[this->activeGameIndex];
	return true;
}

int GameManager::getP1Score() {
	return games[this->activeGameIndex].p1->getScore();
}
int GameManager::getP2Score() {
	return games[this->activeGameIndex].p2->getScore();
}
bool GameManager::updateBoard(vector<TGameMove> moves) {
	games[this->activeGameIndex].board.initDesk();


	updateScore();

	if (moves.size() == 0)
		return false;


	for (TGameMove move : moves) {
		games[this->activeGameIndex].board.updateBoard(move.points, move.color);
	}




	return true;
}

bool GameManager::undo() {
	bool activePoints = games[this->activeGameIndex].data.getHa();
	vector <TGameMove> moves = games[this->activeGameIndex].data.undo();
	cout <<"velkost::" << moves.size()<< endl;
	if (games[this->activeGameIndex].p2->getType() == 1) {
		activePoints = games[this->activeGameIndex].data.getHa();
		cout <<"velkost:ssss:" <<games[this->activeGameIndex].p2->getType()<< endl;
		moves = games[this->activeGameIndex].data.undo();
		//nextPlayer();
	}


	if (games[this->activeGameIndex].p2->getType() == 0 && activePoints) {

		nextPlayer();

	}



	updateBoard(moves);

	return activePoints;
}

bool GameManager::redo() {
	bool activePoints = games[this->activeGameIndex].data.getHa2();
	vector <TGameMove> moves = games[this->activeGameIndex].data.redo();
	if (games[this->activeGameIndex].p2->getType() == 1) {
		activePoints = games[this->activeGameIndex].data.getHa2();
		moves = games[this->activeGameIndex].data.redo();
		//nextPlayer();
	}

	if (games[this->activeGameIndex].p2->getType() == 0 && activePoints) {
		nextPlayer();
	}

updateBoard(moves);
	return activePoints;
}
