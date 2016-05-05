#ifndef PLAYER_H
#define PLAYER_H

#include <iostream>
#include "GameBoard.h"
#include "Strategy.h"
#include <memory>

class Player
{
protected:
	int color;
	int score;
	int typ;
	void setType(int);

public:
	Player();
	bool active;
	virtual bool getNextMove(TPoint*, GameBoard board) {};
	int getColor();
	void setColor(int);
	int getScore();
	void setScore(int);
	int getType();
};

class Human : public Player
{
public:
	Human();
	bool getNextMove(TPoint*, GameBoard board) ;
};

class PC : public Player
{
	std::shared_ptr<Strategy> algo;

public:
	PC(std::shared_ptr<Strategy>);
	bool getNextMove(TPoint*, GameBoard board);
};

#endif
