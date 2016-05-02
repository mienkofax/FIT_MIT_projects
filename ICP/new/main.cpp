#include <iostream>
#include "GameBoard.h"
#include "GameData.h"
#include "GameManager.h"
#include <vector>
#include "Player.h"
#include "Strategy.h"

using namespace std;

void renderMap (GameManager &board) {
	int deskSize = board.getDeskSize();
cout << endl;
	//vykreslenie pismeniek
	cout << "   ";
	for (int i = 0; i < deskSize; i++)
		//cout << char(i+'a') << " ";
		cout << i << " ";

	cout << endl;


	for (int i = 0; i < deskSize; i++){
		cout << i << ": ";
		for (int j = 0; j < deskSize; j++) {
			if (board.getStone({j,i}) == WHITE)
				//cout << "-";
				cout << "●";
			else if (board.getStone({j,i}) == BLACK)
				//cout << "+";
				cout << "○";
			else if (board.getStone({j,i}) == 8)
				cout << ".";
			else {
				cout << " ";
			}
			cout << " ";
		}
		cout << endl;
	}
}


void renderMap2(vector <int> doska) {
	int deskSize = 8;

	for (int i = 0; i < deskSize; i++){
		cout << i << ": ";
		for (int j = 0; j < deskSize; j++) {
			if (doska[deskSize*i+j] == WHITE)
				//cout << "-";
				cout << "●";
			else if (doska[deskSize*i+j] == BLACK)
				//cout << "+";
				cout << "○";
			else {
				cout << " ";
			}
			cout << " ";
		}
		cout << endl;
	}
}


bool strToInt(string s, int *value) {
	char *endptr;
	*value = strtol(s.c_str(), &endptr, 10);

	if ((*endptr) != '\0')
		return false;

	return true;
}

bool getCoordinate(string s, TPoint *point) {
	int index = s.find(",");
	if (index < 0)
		return false;


	if (!strToInt(s.substr(0,index), &point->x))
		return false;
	s.erase(0,index+1); //odstranenie ciarky a xsovej suradnice

	if (!strToInt(s.substr(0,index), &point->y))
		return false;

	return true;
}

bool createNewGame(int *deskSize, int *countPlayers, int *alg) {
	string enter;
	int option;

	cout << "Zadajte Velkost hracej dosky[8 - potvrdenie *]: ";

	//nacitanie velkosti
	cin >> enter;
	if (enter == "*") {
		cout << "Doska sa vytvori s default hodnotou 8." << endl; //TODO nefunguje
		*deskSize = 8;
	} else {
		if (!strToInt(enter, &option)) {
			cerr << "Zadana hodnota musi byt cislo." << endl;
			return false;
		}
		if ((option != 6 && option != 10 && option != 8 && option != 12))	{
			cerr << "Velkost hracej dosky moze byt len 6,8,10,12." << endl;
			return false;
		}
		*deskSize = option;
	}

	//pocet hracov a ich typ pc- clovek
	cout << "Zadajte pocet realnych hracov, musi byt aspon jeden: ";
	cin >> enter;
	if (!strToInt(enter, &option)  || (option != 1 && option != 2)) {
		cerr << "Zadana hodnota musi byt cislo 1 alebo 2." << endl;
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

void help() {
	cout << "NAPOVEDA:" << endl;
	cout << "\tx,y - zadajte suradnice oddelene ciarkou bez medzier[stlpec,riadok], napriklad[a,2].";
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

int main() {
	GameManager manager;
	GameManager& gameManager = manager;

	int option, deskSize, countPlayers, alg;
	string enter;
	TPoint point;
	bool isNewGame = true;
	int actGameIndex = -1;


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
			if (enter == "pass")
				cout << "pass" << endl;
			else if (enter == "save") {
				cout << "Zadajte subor, do ktore sa ulozi hra: ";
				cin >> enter;
				if (manager.saveGame(enter))
					cout << "Herne data boli ulozene v subore: " << enter << endl;
				else
					cerr << "Problem pri ukladani hernych dat, opakujte ulozenie." << endl;
			} else if (enter == "load") {
				cout << "Zadajte nazov suboru, s ktoreho sa maju nacitat data: ";
				cin >> enter;
				if (manager.loadGame(enter))
					cout << "Data boli nacitane." << endl;
				else
					cerr << "Problem pri nacitani dat" << endl;
				renderMap(gameManager);
			} else if (enter == "konec") {
				cout << "Hra bola ukoncena." << endl;
				break;
			} else if (enter == "new") {
				createNewGame(&deskSize, &countPlayers, &alg);
				manager.newGame(deskSize, countPlayers, alg);
				renderMap(gameManager);
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
					cerr << "Zadaza zla hodnota hry" << endl;

			} else if (enter == "undo") {
				if (!gameManager.undo())
					cout << "Nie je mozne ist spat." << endl;
				renderMap(gameManager);
			} else if (enter == "redo") {
				if (!gameManager.redo())
					cout << "Nie je mozne sa vratit vpred." << endl;
				renderMap(gameManager);
			} else {
				cerr << "Nepodporovany string, opakujte volbu." << endl;
			}
		} else {
			if (!gameManager.moveStone(point, false) )
				cout << "nemozno presunut opakujte pokus" << endl;

			renderMap(gameManager);
			//renderMap2(gameManager.getBoard());
		}

		//vypis skore, ked je aspon jedna aktivna hra
		if (!gameManager.isEmpty()) {
			cout << "SCORE::" << gameManager.getP1Score() << endl;
			cout << "SCORE::" << gameManager.getP2Score() << endl;
			cout << "ID hry: " << gameManager.getGameID() << endl;
		}






	}
	return 0;
}
