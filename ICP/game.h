#ifndef GAME_H
#define GAME_H

#include <iostream>
#include <memory>
#include "board.h"
#include "player.h"

typedef struct {
	int moveNumber;		//cislo tahu
	std::vector <TPoint> points;
	int color; 			//farba na ktoru sa to ma prekreslit
} TGameMove;

class OthelloGame
{

public:
	OthelloBoard *board;
	std::unique_ptr <OthelloBoard> board;
	//OthelloBoard uniqptr::<board>;
	std::vector <TGameMove> history;
	Player *p1;
	Player *p2;
	OthelloGame(int);
	void setPlayer(int, int);
	void nextPlayer();
	int getActPlayerColor();
	bool move(TPoint, int, bool);
	void  addMoveToHistory(std::vector <TPoint>, int);
	bool saveGameData(std::string);
	bool loadGameData(std::string);
	void updateGameBoard();

};
#endif
