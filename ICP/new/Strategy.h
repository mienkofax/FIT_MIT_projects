#ifndef STRATEGY_H
#define STRATEGY_H

#include <iostream>
#include "GameBoard.h"

class Strategy
{
public:
	virtual int executeMove(TPoint*) = 0;
};

class Alg1 : public Strategy
{
public:
	int executeMove(TPoint*);
};

class Alg2 : public Strategy
{
public:
	int executeMove(TPoint*);
};

#endif
