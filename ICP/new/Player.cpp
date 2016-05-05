#include <iostream>
#include "Player.h"
#include "Strategy.h"

using namespace std;

Player::Player() :
	color(0),
	score(2),
	typ(0),
	active(false)
{}

void Player::setType(int type)
{
	this->typ = type;
}

int Player::getColor()
{
	return this->color;
}

void Player::setColor(int color) {
	this->color = color;
}

int Player::getScore()
{
	return this->score;
}

void Player::setScore(int score)
{
	this->score = score;
}
int Player::getType()
{
	return this->typ;
}

bool Human::getNextMove(TPoint *point, GameBoard board) {
	return true;
}

bool PC::getNextMove(TPoint *point, GameBoard board) {
	cout << "som PC a volam alg\n";
	cout << algo->executeMove(point, board);
	return true;
}

Human::Human() {
	setType(0);
}

PC::PC(shared_ptr<Strategy> aa) : algo(aa) {
	setType(1);
}
