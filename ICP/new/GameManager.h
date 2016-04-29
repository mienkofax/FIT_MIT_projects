#ifndef GAME_MANAGER_H
#define GAME_MANAGER_H

#include <iostream>
#include <vector>
#include "GameBoard.h"
#include "GameData.h"
#include "Player.h"

typedef struct {
	GameBoard board;
	GameData data;
	Player *p1;
	Player *p2;
} TGame;

class GameManager
{
	std::vector <TGame> games;
	int activeGameIndex;
	bool firstGame;
	TGame *game;
	bool activeRedoUndo;
	bool limitRedo;
	bool limitUndo;
public:
	GameManager();

	void getBoard();

	void nextPlayer();
	void newGame(int, int);
	bool loadGame(std::string);
	bool saveGame(std::string);
	bool setChange(int);
	void endGame(int);
	bool changeGame(int);

	int getDeskSize();
	bool updateBoard(std::vector<TGameMove>);

	int getActivePlayer();
	int moveStone(TPoint, bool);
	void setPCAlg();
	int getStone(TPoint);

	int getP1Score();
	int getP2Score();

	void updateScore();

	bool isActiveP1();
	bool isActiveP2();

	bool livePlayer();
	int getGameID();
	bool isEmpty();

	bool undo();
	bool redo();
};

#endif
