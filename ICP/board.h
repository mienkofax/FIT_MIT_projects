#ifndef BOARD_H
#define BOARD_H

#include <iostream>
#include <vector>

typedef struct {
	bool empty;
	int color;
} TGameField;

typedef struct {
	int x;
	int y;
} TPoint;

#define WHITE 1
#define BLACK 0
#define NON_DEFINE -1

class OthelloBoard
{
	public: //prepisat na private 
		std::vector <int> gameDesk; //private
		int deskSize; //private

	public:
		OthelloBoard(int); //TODO tiez TPoint
		std::vector <TPoint> move(TPoint, int);
		int getField(TPoint); //vynimka
		void setField(TPoint, int); //vynimka
		~OthelloBoard();
		bool isFieldEmpty(TPoint); //vymysliet vynimku
		std::vector <TPoint> findingMoves(TPoint, int);

	private:
		bool checkStep();
		bool updateScore();
		int getPointIndex(TPoint);
		bool checkCoordinates(TPoint);

};
#endif
