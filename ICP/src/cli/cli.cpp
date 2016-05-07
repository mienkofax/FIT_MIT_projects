/**
 * Othello CLI
 *
 * @author			Klára Nečasová <xnecas24>
 * @author			Peter Tisovčík <xtisov00>
 */

#include <iostream>
#include "GameBoard.h"
#include "GameData.h"
#include "GameManager.h"
#include <vector>
#include <iomanip>
#include "Player.h"
#include "Strategy.h"
#include "cli.h"

using namespace std;

void CLI::renderMap(GameManager &board) {
	int deskSize = board.getDeskSize();

	//vykreslenie pismeniek
	cout << endl << "    ";
	for (int i = 0; i < deskSize; i++)
		cout << char(i + 'a') << " ";

	cout << endl;

	//Vykreslenie hracej dosky
	for (int i = 0; i < deskSize; i++){
		cout << setfill(' ') << setw(2) << i + 1  <<": ";

		for (int j = 0; j < deskSize; j++) {
			if (board.getStone({j,i}) == WHITE)
				cout << "●";
			else if (board.getStone({j,i}) == BLACK)
				cout << "○";
			else if (board.getStone({j,i}) == 8)
				cout << ".";
			else
				cout << " ";
			cout << " ";
		}
		cout << endl;
	}
}

bool CLI::strToInt(string str, int *value) {
	char *endptr;
	*value = strtol(str.c_str(), &endptr, 10);

	if ((*endptr) != '\0')
		return false;

	return true;
}

bool CLI::getCoordinate(string str, TPoint *point) {
	int index = str.find(",");
	if (index < 1)
		return false;

	//prevod znaku na cislo
	point->x = unsigned(char(str.substr(0, index)[0]-'a'));

	str.erase(0,index+1); //odstranenie ciarky a xsovej suradnice

	if (!strToInt(str, &point->y))
		return false;

	point->y -= 1;
	return true;
}

bool CLI::createNewGame(int *deskSize, int *countPlayers, int *alg) {
	string enter;
	int option;

	cout << "Zadajte Velkost hracej dosky[8 - predvolene znak *]: ";

	//nacitanie velkosti
	cin >> enter;
	if (enter == "*") {
		cout << "Doska sa vytvori s default hodnotou 8." << endl;
		*deskSize = 8;
	} else {
		if (!strToInt(enter, &option)) {
			cout << "Zadana hodnota musi byt cislo." << endl;
			return false;
		}
		if ((option != 6 && option != 10 && option != 8 && option != 12))	{
			cout << "Velkost hracej dosky moze byt len 6,8,10,12." << endl;
			return false;
		}
		*deskSize = option;
	}

	//pocet hracov a ich typ PC - clovek
	cout << "Zadajte pocet realnych hracov, musi byt aspon jeden: ";
	cin >> enter;
	if (!strToInt(enter, &option)  || (option != 1 && option != 2)) {
		cout << "Zadana hodnota musi byt cislo 1 alebo 2." << endl;
		return false;
	} else {
		*countPlayers = option;

		if (option == 1) {
		cout << "Zadajte cislo algoritmu, ktory sa ma pouzit: ";
		cin >> enter;
			if (strToInt(enter, &option)) {
				*alg = option;
			} else {
				cout << "Zadana neplatna volba." << endl;
				return false;
			}
		}
	}

	return true;
}

void CLI::help() {
	cout << "NAPOVEDA:" << endl;
	cout << "\tx,y - zadajte suradnice oddelene ciarkou bez medzier[stlpec,riadok], napriklad:a,2";
	cout << endl;
	cout << "\tpass - v pripade, ze nemate moznost posunu zadajte \"pass\"" << endl;
	cout << "\tsave - v pripade, ze chcete ulozit rozohratu hru" << endl;
	cout << "\tload - v pripade, ze chcete nacitat rozohratu hru" << endl;
	cout << "\tundo - krok spat" << endl;
	cout << "\tredo - krok vpred" << endl;
	cout << "\tnew - pre vytvorenie novej hry" << endl;
	cout << "\tchange - pre zmenu hry" << endl;
	cout << "\tkonec - pre ukoncenie hry" << endl;
}

void CLI::show() {
	GameManager manager;
	GameManager& gameManager = manager;

	int option, deskSize, countPlayers, alg;
	string enter;
	TPoint point;

	while(1) {
		cout << "------------------------------------\n";

		if (!gameManager.isEmpty()) {
			if(manager.isActiveP1())
			cout << "Na rade je hrac 1(Biele): ";
			if (manager.isActiveP2())
			cout << "Na rade je hrac 2(Cierne): ";
		}
		else
			help();

		if (gameManager.livePlayer())
			cin >> enter;

		if (!getCoordinate(enter, &point)) {
			if (enter == "pass") {
				if (manager.moveStone({0,0}, true))
					cout << "Pass sa vykonal." << endl;
				else
					cout << "Pass je mozne zadat len ked nie je ziaden tah." << endl;
			} else if (enter == "save") {
				cout << "Zadajte subor, do ktore sa ulozi hra: ";
				cin >> enter;
				if (manager.saveGame(enter))
					cout << "Herne data boli ulozene v subore: " << enter << endl;
				else
					cout << "Problem pri ukladani hernych dat, opakujte ulozenie." << endl;
			} else if (enter == "load") {
				cout << "Zadajte nazov suboru, s ktoreho sa maju nacitat data: ";
				cin >> enter;
				if (manager.loadGame(enter))
					cout << "Data boli nacitane." << endl;
				else
					cout << "Problem pri nacitani dat" << endl;
				renderMap(gameManager);
			} else if (enter == "konec") {
				cout << "Hra bola ukoncena." << endl;
				break;
			} else if (enter == "new") {
				if (!createNewGame(&deskSize, &countPlayers, &alg))
					cout << "Nebolo mozne vytvorit hru." << endl;
				else {
					manager.newGame(deskSize, countPlayers, alg);
					renderMap(gameManager);
				}
			} else if (enter == "change") {
				cout << "Zadajte cislo hry: ";
				cin >> enter;
				if (strToInt(enter, &option) && option >=0 ) {
					if (manager.changeGame(option))
						cout << "Hra uspesne zmenena." << endl;
					else
						cout << "Bola zadana nespravna hodnota. " << endl;
					renderMap(gameManager);
				} else
					cout << "Bola zadaza zla hodnota hry." << endl;

			} else if (enter == "undo") {
				if (!gameManager.undo())
					cout << "Nie je mozne ist spat." << endl;
				renderMap(gameManager);
			} else if (enter == "redo") {
				if (!gameManager.redo())
					cout << "Nie je mozne sa vratit vpred." << endl;
				renderMap(gameManager);
			} else {
				cout << "Nepodporovany string, opakujte volbu." << endl;
			}
		} else {
			if (!gameManager.moveStone(point, false) )
				cout << "Nemozno presunut kamen, opakujte pokus." << endl;

			renderMap(gameManager);
		}

		//vypis skore, ked je aspon jedna aktivna hra
		if (!gameManager.isEmpty()) {
			cout << "SCORE::" << gameManager.getP1Score() << endl;
			cout << "SCORE::" << gameManager.getP2Score() << endl;
			cout << "ID hry: " << gameManager.getGameID() << endl;
		}
	}
}
