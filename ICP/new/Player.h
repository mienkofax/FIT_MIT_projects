#ifndef PLAYER_H
#define PLAYER_H

#include <iostream>
#include "GameBoard.h"
#include "Strategy.h"

class Player
{
protected:
	int color;
	int score = 2;
public:
	int typ = -1;
	int active;
	virtual bool getNextMove(TPoint*) {};
	int getScore();

	void setScore(int);
	int getColor();
	void setColor(int);
	int getType();
protected:
	void setType(int);
};
class Human :public Player
{
	int typ = 0;
public:
	Human();
	 bool getNextMove(TPoint*) ;
};


class PC : public Player
{
	int typ = 1;
	//GameBoard board;
	Strategy *algp;
public:
	PC();
 bool getNextMove(TPoint*);
 PC(Strategy* aaaaa1);
};



#endif
