#include <iostream>
#include "Player.h"
#include "Strategy.h"

using namespace std;
void Player::setType(int type)
{
	this->typ = type;
}
int Player::getType()
{
	return this->typ;
}

bool Human::getNextMove(TPoint *point) {
	return true;
}

bool PC::getNextMove(TPoint *point) {
	cout << "som PC";
	cout << algp->executeMove(point);
	return true;
}
Human::Human() {
	setType(0);
}
PC::PC(Strategy* aaaaa1) : algp(aaaaa1)  {
	setType(1);
}
void Player::setColor(int color) {
	this->color = color;
}

int Player::getColor()
{
	return this->color;
}
int Player::getScore() {
	return this->score;
}
void Player::setScore(int score) {
	this->score = score;
}
