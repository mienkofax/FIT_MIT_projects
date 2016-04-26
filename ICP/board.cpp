#include <iostream>
#include "board.h"

using namespace std;

//inicializuje potrebnu mapu na zadanu velkost
OthelloBoard::OthelloBoard(int deskSize) :
	deskSize(deskSize),
	gameDesk(vector <int> (deskSize*deskSize,NON_DEFINE))
{

}

//zisti ci sa nachadza na danom policku nejaka hodnota
bool OthelloBoard::isFieldEmpty(TPoint point)
{
	if (getField(point) == NON_DEFINE)
		return true;

	return false;
}

//urobi tah hry
vector <TPoint> OthelloBoard::move(TPoint point, int color)
{
	vector <TPoint> points;

	if (!isFieldEmpty(point))
		return points;

	//nastavenie bodu, po ktory chcem vykreslit
	setField(point, color);

	//zistenie vsetkych moznosti na prekreslenie z daneho bodu
	points = findingMoves(point,color);


	if (points.size() == 0) {
		setField(point, NON_DEFINE); //vratenie povodnej hodnoty na policko
		return points;
	}

	for( TPoint p : points) {
		cout << p.x << "," << p.y << endl;
		setField({p.x,p.y}, color);
	}

	return points;
}

//najdenie tahov
//snazi sa prehladat okolie zadaneho bodu a vyberie policka, ktore je mozne prekreslit,
//vrati ich ako vector
vector <TPoint> OthelloBoard::findingMoves(TPoint point, int color)
{
	TPoint startPoint = point;
	vector <TPoint> endPoints;
	vector <TPoint> directions;

	//vygenerovanie vsetkych smerov pre prehladavanie
	//(-1,-1), (-1,0), (-1,1), (0,-1), (0,1), (1,-1), (1,0), (1,1)
	for (int i = 0; i < 3; i++) {
		for (int j = 0; j < 3; j++) {
			if (i == 1 && j == 1) continue; //TODO asi netreba ani
			directions.push_back({i-1, j-1});
		}
	}

	//Prechod vygenerovanymi suradnicami
	for( TPoint po : directions) {
		int x = po.x;
		int y = po.y;

		for (int i = 0; i < deskSize; i++) {
			if(!checkCoordinates(point) || isFieldEmpty(point))
				break;

			point.x += x;
			point.y += y;

			//cout << "X: " << point.x-x << " Y:" << point.y-y << getField(point)<< endl;

			if (getField(point) == color) {
				if (startPoint.x == point.x-x && startPoint.y == point.y-y)
					break;
				//spatne dohladanie vsetkych bodov na vykreslenie
				for (int k = 0; k < deskSize; k++) {
					point.x -= x; point.y -= y;
					if (point.x == startPoint.x && point.y == startPoint.y)
						break;

					endPoints.push_back({point.x, point.y});
				}

				break;
			} //koniec ifu
		}
		point.x = startPoint.x; point.y = startPoint.y;
	}

	return endPoints;
}

//nastavi hodnotu na danom policku
void OthelloBoard::setField(TPoint point, int color)
{
	int index = getPointIndex(point);
	gameDesk[index] = color;
}

//vrati hodnotu daneho policka
int OthelloBoard::getField(TPoint point)
{
	return gameDesk[getPointIndex(point)];
}

//zistit ci su vlozene validne suradnice
bool OthelloBoard::checkCoordinates(TPoint point)
{
	if (point.x >= 0 && point.x < deskSize && point.y >= 0 && point.y < deskSize)
		return true;
	return false;
}

//dostane index na zaklade suradnic
int OthelloBoard::getPointIndex(TPoint point)
{
	return deskSize * point.y + point.x;
}

OthelloBoard::~OthelloBoard()
{
	gameDesk.clear();
}
