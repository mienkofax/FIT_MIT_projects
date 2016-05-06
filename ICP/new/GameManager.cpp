/**
 * Game Manager
 *
 * @author			Klára Nečasová <xnecas24>
 * @author			Peter Tisovčík <xtisov00>
 */

#include <iostream>
#include "GameManager.h"
#include "GameBoard.h"
#include "GameData.h"
#include "Player.h"
#include <fstream>
#include <memory>

using namespace std;

GameManager::GameManager() :
	games(vector <TGame> ()),
	activeGameIndex(0),
	firstGame(true),
	game(nullptr)
{}

void GameManager::newGame(int deskSize, int players, int algorithm)
{
	shared_ptr<Player> p1(new Human());
	shared_ptr<Player> p2;

	//ak ma byt druhy pc pocitac, tak sa mu nastavi algoritmus a hracia doska
	if (players == 1) {
		shared_ptr<Strategy> alg;
		shared_ptr<GameBoard> board(new GameBoard(deskSize));

		//nastavenie vybraneho algoritmu
		if (algorithm == 1)
			alg = shared_ptr<Strategy>(new Alg1());
		else if (algorithm == 2)
			alg = shared_ptr<Strategy>(new Alg2());

		p2 = shared_ptr<Player>(new PC(alg));
	} else
		p2 = shared_ptr<Player>(new Human());

	p1->active = true;
	p2->active = false;

	p1->setColor(WHITE);
	p2->setColor(BLACK);

	if (!firstGame)
		activeGameIndex++;
	else
		firstGame = false;

	//vlozenie novej hry
	this->games.push_back({GameBoard(deskSize), GameData(), p1, p2});
	game = &games[this->activeGameIndex];

	//vlozenie moznych tahov
	getHint();
}

bool GameManager::loadGame(string filename)
{
	int deskSize, players;

	ifstream gameFile;
	gameFile.open(filename);

	if (!gameFile.is_open())
		return false;

	//nacitanie velkosti hracej dosky a pocet hracov
	gameFile >> deskSize;
	gameFile >> players;
	gameFile.close();

	newGame(deskSize, players, 2); //TODO dorobit ukladanie cisla algoritmu

	//nacitanie hernych dat
	bool returnCode = games[this->activeGameIndex].data.loadGameData(filename);

	//aplikovanie jednotlivych tahov na hraciu dosku
	for (TGameMove move : games[this->activeGameIndex].data.getHistory())
		games[this->activeGameIndex].board.updateBoard(move.points, move.color);

	return returnCode;
}

bool GameManager::saveGame(string filename)
{
	//predvolena hodnota - pocet hracov
	int players = 2;

	//v pripade, ze je druhy hrac pc tak zmenime hracov
	if (games[this->activeGameIndex].p2->getType() == 1)
		players = 1;

	//ulozenie ernych dat
	return games[this->activeGameIndex].data.saveGameData(filename, games[this->activeGameIndex].board.getDeskSize(), players);
}

bool GameManager::changeGame(size_t gameIndex)
{
	//kontrola ci je index hry v platnom rozsahu
	if (gameIndex > games.size())
		return false;

	activeGameIndex = gameIndex;

	//ukazatel na aktualnu hru
	game = &games[this->activeGameIndex];
	return true;
}

bool GameManager::undo()
{
	bool activePoints;
	vector <TGameMove> moves;

	//zistenie ci je este mozne nejake tahy vratit naspat
	activePoints = games[this->activeGameIndex].data.checkUndo();

	//vykonanie undo funkcie, navrat o jeden tah spat
	moves = games[this->activeGameIndex].data.undo();

	//ak sa jedna o hru PC - HUMAN, tak je potrebne vratit tah aj za pc
	if (games[this->activeGameIndex].p2->getType() == 1) {
		activePoints = games[this->activeGameIndex].data.checkUndo();
		moves = games[this->activeGameIndex].data.undo();
	}

	//ak sa jedna o hru PC - HUMAN, musime posunut hru na dalsieho hraca
	if (games[this->activeGameIndex].p2->getType() == 0 && activePoints)
		nextPlayer();

	//aktualizacia hracej dosky po aplkovani undo
	updateBoard(moves);

	return activePoints;
}

bool GameManager::redo()
{
	bool activePoints;
	vector <TGameMove> moves;

	//zistenie ci je urobit tah vpred
	activePoints = games[this->activeGameIndex].data.checkRedo();

	//vykonanie tahu vpred
	moves = games[this->activeGameIndex].data.redo();

	if (games[this->activeGameIndex].p2->getType() == 1) {
		activePoints = games[this->activeGameIndex].data.checkRedo();
		moves = games[this->activeGameIndex].data.redo();
	}

	//ak sa jedna o hru PC - HUMAN, musime posunut hru na dalsieho hraca
	if (games[this->activeGameIndex].p2->getType() == 0 && activePoints)
		nextPlayer();

	updateBoard(moves);
	return activePoints;
}

bool GameManager::moveStone(TPoint point, bool isPass)
{
	int color, count;
	vector <TPoint> points;

	//zistenie tahu a zistenie aka farba sa ma pouzit
	if(isActiveP1()) {
		games[this->activeGameIndex].p1->getNextMove(&point, games[this->activeGameIndex].board);
		color = games[this->activeGameIndex].p1->getColor();
	} else {
		if (livePlayer())
			games[this->activeGameIndex].p2->getNextMove(&point, games[this->activeGameIndex].board);
		else
			isPass = games[this->activeGameIndex].p2->getNextMove(&point, games[this->activeGameIndex].board);

		color = games[this->activeGameIndex].p2->getColor();
	}

	if (isPass) {
		if (getHint() > 0)
			return false;
		else {
			nextPlayer();
			getHint();
			updateScore();
			return true;
		}
	}

	//body, ktore sa maju prekreslit
	points = games[this->activeGameIndex].board.moveStone(point, color);
	count = points.size();

	//ak bol tah dobry vrati sa aspon jedna suradnica, ktora sa prekreslila
	//a pridaju sa prekreslene tahy do historie
	if (count > 0) {
		games[this->activeGameIndex].data.removeInvalidData();

		//pridanie bodu, ktory sa vytvoril k bodovm, ktore sa prekreslia
		points.push_back(point);
		games[this->activeGameIndex].data.addMoveToHistory(points, color);

		nextPlayer();
		getHint();
		updateScore();

		return true;
	}
	return false;
}

void GameManager::endGame()
{
	return;
}

bool GameManager::isActiveP1()
{
	if (games.size() == 0)
		return false;
	return games[this->activeGameIndex].p1->active;
}

bool GameManager::isActiveP2()
{
	if (games.size() == 0)
		return false;
	return games[this->activeGameIndex].p2->active;
}

bool GameManager::livePlayer()
{
	if (isActiveP2() && games[this->activeGameIndex].p2->getType() == 1)
		return false;
	return true;
}

bool GameManager::isEmpty()
{
	if (games.size() == 0)
		return true;
	return false;
}

int GameManager::getGameID()
{
	if (isEmpty())
		return -1;
	return this->activeGameIndex;
}

int GameManager::getDeskSize()
{
	if (isEmpty())
		return -1;
	return games[this->activeGameIndex].board.getDeskSize();
}

int GameManager::getP1Score()
{
	if (isEmpty())
		return -1;
	return games[this->activeGameIndex].p1->getScore();
}

int GameManager::getP2Score()
{
	if (isEmpty())
		return -1;
	return games[this->activeGameIndex].p2->getScore();
}

int GameManager::getStone(TPoint point)
{
	return games[this->activeGameIndex].board.getStone(point);
}

int GameManager::getHint()
{
	vector <TPoint> points;
	int color, deskSize, count = 0;

	//zistenie, pre ktoru farbu sa maju hladat pomocne body
	if(isActiveP1())
		color = games[this->activeGameIndex].p1->getColor();
	else
		color = games[this->activeGameIndex].p2->getColor();

	deskSize = games[this->activeGameIndex].board.getDeskSize();

	//prechod jednotlivymi suradnicami na hracej doske a zistenie
	//mozneho tahu z danej suadnice
	for (int i = 0; i < deskSize; i++) {
		for (int j = 0; j < deskSize; j++) {

			//odstranenie predchadzajucej napovedy
			if (games[this->activeGameIndex].board.getStone({j,i}) == 8)
				games[this->activeGameIndex].board.setStone({j,i}, NON_DEFINE);

			//zistenie ci je mozne na dane suradnice vykonat tah
			if (games[this->activeGameIndex].board.moveHint({j,i}, color) > 0)
				count++;
		}
	}
	return count;
}

void GameManager::nextPlayer()
{
	if (isActiveP1())
	{
		games[this->activeGameIndex].p1->active = false;
		games[this->activeGameIndex].p2->active = true;
	} else
	{
		games[this->activeGameIndex].p1->active = true;
		games[this->activeGameIndex].p2->active = false;
	}
}

void GameManager::updateScore()
{
	int colorP1 = games[this->activeGameIndex].p1->getColor();
	int colorP2 = games[this->activeGameIndex].p2->getColor();

	game->p1->setScore(games[this->activeGameIndex].board.getCountStone(colorP1));
	game->p2->setScore(games[this->activeGameIndex].board.getCountStone(colorP2));
}

void GameManager::updateBoard(vector<TGameMove> moves)
{
	//inicializuje hraciu dosku na pociatocny stav
	games[this->activeGameIndex].board.initDesk();

	//vykreslenie jednotlivych bodov postupne ako idu v historii
	for (TGameMove move : moves)
		games[this->activeGameIndex].board.updateBoard(move.points, move.color);

	getHint();
	updateScore();
}
