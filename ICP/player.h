#ifndef PLAYER_H
#define PLAYER_H

#include <iostream>
#include <string.h>

#define PLAYER_PC 1
#define PLAYER_PERSON 2

class Player
{

	public: //private
		int typ; //enum
		int color;
		bool move = false;
		int score; //pocet kamenov
		int getScore();
		void setScore(int);
		int getColor();
};
class Person : public Player {
public:
	int typ = 1;
};
class Pc : public Player {
public:
	int typ = 2;

};
#endif
