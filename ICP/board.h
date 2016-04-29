#ifndef BOARD_H
#define BOARD_H

#include <iostream>
#include <vector>

typedef struct {
	int x;
	int y;
} TPoint;

#define WHITE 1
#define BLACK 0
#define NON_DEFINE -1

class OthelloBoard
{
	std::vector <int> gameDesk;
	TPoint deskSize;

	bool checkStep();
	bool updateScore();
	int getPointIndex(TPoint);
	bool checkCoordinates(TPoint);
	bool isFieldEmpty(TPoint); //vymysliet vynimku
	std::vector <TPoint> findingMoves(TPoint, int);

	public:
		OthelloBoard(TPoint);
		std::vector <TPoint> move(TPoint, int);
		int getField(TPoint); //vynimka
		void setField(TPoint, int); //vynimka
		~OthelloBoard();
};
#endif
