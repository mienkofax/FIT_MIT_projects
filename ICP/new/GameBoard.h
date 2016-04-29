#ifndef GAME_BOARD_H
#define GAME_BOARD_H

#include <iostream>
#include <vector>

typedef struct {
	int x;
	int y;
} TPoint;

#define WHITE 1
#define BLACK 0
#define NON_DEFINE -1

class GameBoard
{
	int deskSize;

	bool checkCoordinates(TPoint);
	bool isFieldEmpty(TPoint); //vymysliet vynimku
	int getPointIndex(TPoint);

public:
	std::vector <int> gameDesk;
	void setStone(TPoint, int); //vynimka
	GameBoard(int);
	void updateBoard(std::vector <TPoint>, int);
	int getDeskSize();
	std::vector <TPoint> findingMoves(TPoint, int);
	std::vector <TPoint> moveStone(TPoint, int);
	std::vector <TPoint> testStone(TPoint, int);
	int getStone(TPoint); //vynimka
	void initDesk();
	int getCountStone(int);
	~GameBoard();
};

#endif
