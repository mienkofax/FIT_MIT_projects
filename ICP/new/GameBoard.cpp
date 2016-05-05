/**
 * Game Board
 *
 * @author			Klára Nečasová <xnecas24>
 * @author			Peter Tisovčík <xtisov00>
 */

#include <iostream>
#include "GameBoard.h"

using namespace std;

GameBoard::GameBoard(int deskSize) :
	deskSize(deskSize),
	gameDesk(vector <int> (deskSize*deskSize,NON_DEFINE))
{
	initDesk();
}

bool GameBoard::checkCoordinates(TPoint point)
{
	if (point.x >= 0 && point.x < deskSize && point.y >= 0 && point.y < deskSize)
		return true;
	return false;
}

bool GameBoard::isFieldEmpty(TPoint point)
{
	if (getStone(point) == NON_DEFINE || getStone(point) == HINT)
		return true;

	return false;
}

int GameBoard::getPointIndex(TPoint point)
{

	return deskSize * point.y + point.x;
}

vector <TPoint> GameBoard::findingMoves(TPoint point, int color)
{
	TPoint startPoint = point;
	vector <TPoint> endPoints;
	vector <TPoint> directions;

	//vygenerovanie vsetkych smerov pre prehladavanie
	//(-1,-1), (-1,0), (-1,1), (0,-1), (0,1), (1,-1), (1,0), (1,1)
	for (int i = 0; i < 3; i++)
		for (int j = 0; j < 3; j++)
			directions.push_back({i-1, j-1});


	//prechod vygenerovanymi suradnicami
	for( TPoint po : directions)
	{
		int x = po.x;
		int y = po.y;

		//prechod maximalne dlzkou hracej dosky
		for (int i = 0; i < this->deskSize; i++)
		{
			if(!checkCoordinates(point) || isFieldEmpty(point))
				break;

			//pripocitanie suradnic v danom smere
			point.x += x;
			point.y += y;

			//ak sa jedna o spravnu farbu a ak sa nejedna o ten isty tak sa prida
			if (getStone(point) == color)
			{
				if (startPoint.x == point.x-x && startPoint.y == point.y-y)
					break;

				//spetne dohladanie vsetkych bodov na vykreslenie
				for (int k = 0; k < deskSize; k++)
				{
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

void GameBoard::initDesk()
{
	int start = deskSize / 2 -1;

	//nastavenie hodnoty pre cely vektor
	std::fill(gameDesk.begin(), gameDesk.end(), NON_DEFINE);

	setStone({start,start}, WHITE);
 	setStone({start + 1,start}, BLACK);
 	setStone({start,start + 1}, BLACK);
 	setStone({start + 1,start + 1}, WHITE);
}

void GameBoard::setStone(TPoint point, int color)
{
	int index = getPointIndex(point);
	gameDesk[index] = color;
}

int GameBoard::getStone(TPoint point)
{
	return gameDesk[getPointIndex(point)];
}

int GameBoard::getDeskSize()
{
	return this->deskSize;
}

void GameBoard::updateBoard(vector <TPoint> points, int color) {
	for (TPoint point : points)
		setStone(point, color);
}

int GameBoard::getCountStone(int color) {
	int count = 0;
	for (int index : this->gameDesk)
	{
		if (index == color)
			count++;
	}

	return count;
}

vector <TPoint> GameBoard::moveStone(TPoint point, int color)
{
	vector <TPoint> points;

	if (!isFieldEmpty(point))
		return points;

	//nastavenie bodu, po ktory chcem vykreslit
	setStone(point, color);

	//zistenie vsetkych moznosti na prekreslenie z daneho bodu
	points = findingMoves(point,color);

	//vratenie povodnej hodnoty na policko ak to bol neplatny tah
	if (points.size() == 0)
	{
		setStone(point, NON_DEFINE);
		return points;
	}

	//obratenie kamenov
	for( TPoint p : points)
		setStone({p.x,p.y}, color);

	return points;
}

int GameBoard::moveHint(TPoint point, int color)
{
	vector <TPoint> points;

	if (!isFieldEmpty(point))
		return points.size();

	//nastavenie bodu, po ktory chcem vykreslit
	setStone(point, color);

	//zistenie vsetkych moznosti na prekreslenie z daneho bodu
	points = findingMoves(point,color);

	//vratenie povodnej hodnoty na policko ak to bol neplatny tah
	if (points.size() > 0)
		setStone(point, HINT);
	else
		setStone(point, NON_DEFINE);

	return points.size();
}
