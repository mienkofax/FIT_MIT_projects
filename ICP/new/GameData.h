#ifndef GAME_DATA_H
#define GAME_DATA_H

#include <iostream>
#include <vector>
#include "GameBoard.h"

typedef struct {
	int moveNumber;		//cislo tahu
	std::vector <TPoint> points;
	int color; 			//farba na ktoru sa to ma prekreslit
	bool isActive;
} TGameMove;

class GameData
{
	std::vector <TGameMove> history;

public:
	GameData();
	std::vector <TGameMove> undo();
	std::vector <TGameMove> redo();
	void addMoveToHistory(std::vector <TPoint>, int);
	bool saveGameData(std::string, int, int); //vynimka
	bool loadGameData(std::string); //vynimka
	void removeInvalidData();
	bool getHa();
	bool getHa2();
	std::vector <TGameMove> getHistory();
};

#endif
