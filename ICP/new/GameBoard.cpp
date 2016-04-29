#include <iostream>
#include "GameBoard.h"

using namespace std;

//inicializuje potrebnu mapu na zadanu velkost
GameBoard::GameBoard(int deskSize) :
	deskSize(deskSize),
	gameDesk(vector <int> (deskSize*deskSize,NON_DEFINE))
{
	initDesk();
}



void GameBoard::initDesk() {
	int start = deskSize / 2 -1;

	std::fill(gameDesk.begin(), gameDesk.end(), NON_DEFINE);

	setStone({start,start}, WHITE);
 	setStone({start + 1,start}, BLACK);
 	setStone({start,start + 1}, BLACK);
 	setStone({start + 1,start + 1}, WHITE);
}

void GameBoard::updateBoard(vector <TPoint> points, int color)
{
	for (TPoint point : points)
		setStone(point, color);
}

//zisti ci sa nachadza na danom policku nejaka hodnota
bool GameBoard::isFieldEmpty(TPoint point)
{
	if (getStone(point) == NON_DEFINE || getStone(point) == 8)
		return true;

	return false;
}

//urobi tah hry
vector <TPoint> GameBoard::moveStone(TPoint point, int color)
{
	vector <TPoint> points;

	if (!isFieldEmpty(point))
		return points;

	//nastavenie bodu, po ktory chcem vykreslit
	setStone(point, color);

	//zistenie vsetkych moznosti na prekreslenie z daneho bodu
	points = findingMoves(point,color);


	if (points.size() == 0) {
		setStone(point, NON_DEFINE); //vratenie povodnej hodnoty na policko
		return points;
	}

	for( TPoint p : points) {
		cout << p.x << "," << p.y << endl;
		setStone({p.x,p.y}, color);
	}

	return points;
}

vector <TPoint> GameBoard::testStone(TPoint point, int color)
{
	vector <TPoint> points;

	if (!isFieldEmpty(point))
		return points;

	//nastavenie bodu, po ktory chcem vykreslit
	setStone(point, color);

	//zistenie vsetkych moznosti na prekreslenie z daneho bodu
	points = findingMoves(point,color);


		if (points.size() > 0) {
			setStone(point, 8); //vratenie povodnej hodnoty na policko

		}
		else
			setStone(point, NON_DEFINE);


	return points;
}

//najdenie tahov
//snazi sa prehladat okolie zadaneho bodu a vyberie policka, ktore je mozne prekreslit,
//vrati ich ako vector
vector <TPoint> GameBoard::findingMoves(TPoint point, int color)
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

			//cout << "X: " << point.x-x << " Y:" << point.y-y << endl;

			if (getStone(point) == color) {
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
void GameBoard::setStone(TPoint point, int color)
{
	int index = getPointIndex(point);
	gameDesk[index] = color;
}

//vrati hodnotu daneho policka
int GameBoard::getStone(TPoint point)
{
	return gameDesk[getPointIndex(point)];
}

//zistit ci su vlozene validne suradnice
bool GameBoard::checkCoordinates(TPoint point)
{
	if (point.x >= 0 && point.x < deskSize && point.y >= 0 && point.y < deskSize)
		return true;
	return false;
}

//dostane index na zaklade suradnic
int GameBoard::getPointIndex(TPoint point)
{
	return deskSize * point.y + point.x;
}

int GameBoard::getDeskSize()
{
	return this->deskSize;
}
int GameBoard::getCountStone(int color) {
	int count = 0;
	for (int i : gameDesk) {
		if (i == color)
			count++;
	}

	return count;
}

GameBoard::~GameBoard()
{
	gameDesk.clear();
}
