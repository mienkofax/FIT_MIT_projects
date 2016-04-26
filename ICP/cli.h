#include <iostream>

class CLI
{
public:
	void drawGameDesk(int);

};

#include <iostream>
#include "board.h"

using namespace std;

OthelloBoard::OthelloBoard(int as)
{
	this->deskSize = 8;

	for (int i = 0; i < (deskSize*deskSize); i++)
		gameDesk.push_back({true, -1});




	cout << "Velkost dosky: " << gameDesk.size() << endl;


	cout << getPointIndex({2,3}) << endl;

	setField({5,4}, BLACK);

	setField({4,4}, WHITE);
	setField({4,5}, WHITE);
	setField({5,4},BLACK);
	setField({5,5}, WHITE);
	setField({6,5}, BLACK);


	move({3, 4}, BLACK);

	cout << isFieldEmpty({4,7});
	cout << endl;

	cout << "   0 1 2 3 4 5 6 7\n";
	for (int i = 0; i < deskSize; i++){
		cout << i << ": ";
		for (int j = 0; j < deskSize; j++) {
			TGameField field = getField({j, i});
			if (!field.empty) {
				if (field.color == 0) cout << "+";
				else cout << "-";
			}
			else {
				cout << " ";
			}
			cout << " ";
		}
		cout << endl;
	}

	vector <TPoint> haha = rePaintPoints({3,4}, BLACK);
	for( TPoint po : haha) {
		cout << po.x << "," << po.y << endl;
	}


}

//zisti ci sa nachadza na danom policku nejaka hodnota
bool OthelloBoard::isFieldEmpty(TPoint point)
{
	TGameField field = getField(point);

	if (field.empty && field.color < 0)
		return true;

	return false;
}

//urobi tah hry
bool OthelloBoard::move(TPoint newPoint, int color)
{
	if (isFieldEmpty(newPoint))
		cout << "Mozeme presunut\n";
	else
		cout << "Policko obsadene\n";

	setField(newPoint, color);

	//rePaint(newPoint, BLACK);

	return false;
}

//snazi sa prehladat okolie zadaneho bodu a vyberie policka, ktore je mozne prekreslit
vector <TPoint> OthelloBoard::rePaintPoints(TPoint point, int color)
{
	TPoint startPoint = point;
	vector <TPoint> endPoints;
	vector <TPoint> directions;
	bool paint = false;

	//(-1,-1), (-1,0), (-1,1), (0,-1), (0,1), (1,-1), (1,0), (1,1)
	for (int i = 0; i < 3; i++) {
		for (int j = 0; j < 3; j++) {
			if (i == 1 && j == 1) continue;
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

			//cout << "X: " << point.x << " Y:" << point.y << getField(point).color << endl;

			if (getField(point).color == color) {
				if (startPoint.x == point.x-x && startPoint.y == point.y-y) {
					cout << "*nemozem vykreslit\n";
					break;
				}
				cout << "Prekreslujem po: " << point.x-x << "," << point.y-y << endl;
				endPoints.push_back({point.x-x, point.y-y});
				paint = true;
				break;
			}


		}
		point = startPoint;
	}

	return endPoints;
}

//nastavi hodnotu na danom policku
void OthelloBoard::setField(TPoint point, int color)
{
	int index = getPointIndex(point);
	gameDesk[index] = {false, color};
}

//vrati hodnotu daneho policka
TGameField OthelloBoard::getField(TPoint point)
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
