#include <iostream>
#include "GameBoard.h"
#include "GameData.h"
#include "GameManager.h"
#include <vector>
#include "Player.h"
#include "Strategy.h"

class CLI {
	void renderMap(GameManager &board);
	bool strToInt(std::string s, int *value);
	bool getCoordinate(std::string s, TPoint *point);
	bool createNewGame(int *deskSize, int *countPlayers, int *alg);
	void help();
public:
	void show();

};
