#ifndef STRATEGY_H
#define STRATEGY_H

#include <iostream>
#include "GameBoard.h"
#include <vector>
#include <memory>

class Strategy
{
protected:
	std::shared_ptr<GameBoard> board;
public:
	Strategy();
	virtual int executeMove(TPoint*) = 0;
};

class Alg1 : public Strategy
{
public:
	Alg1(std::shared_ptr<GameBoard> board);
	int executeMove(TPoint*);
};

class Alg2 : public Strategy
{
public:
	Alg2(std::shared_ptr<GameBoard> board);
	int executeMove(TPoint*);
};

#endif
