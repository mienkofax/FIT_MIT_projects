#ifndef STRATEGY_H
#define STRATEGY_H

#include <iostream>
#include "GameBoard.h"
#include <vector>
#include <memory>

class Strategy
{
public:
	Strategy();
	virtual bool executeMove(TPoint*,  GameBoard board) = 0;
};

class Alg1 : public Strategy
{
public:
	bool executeMove(TPoint*, GameBoard board);
};

class Alg2 : public Strategy
{
public:
	bool executeMove(TPoint*,  GameBoard board);
};

#endif
