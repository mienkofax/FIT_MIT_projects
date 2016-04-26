#include <iostream>
#include "game.h"
#include "board.h"
#include <string>
#include "player.h"

using namespace std;

void renderMap (unique_ptr <OthelloBoard> board) {
	int deskSize = board->deskSize;

	//vykreslenie pismeniek
	cout << "   ";
	for (int i = 0; i < deskSize; i++)
		//cout << char(i+'a') << " ";
		cout << i << " ";

	cout << endl;


	for (int i = 0; i < deskSize; i++){
		cout << i << ": ";
		for (int j = 0; j < deskSize; j++) {
			if (board->getField({j,i}) == WHITE)
				//cout << "-";
				cout << "●";
			else if (board->getField({j,i}) == BLACK)
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

bool strToInt(string s, int *value)
{
	char *endptr;
	*value = strtol(s.c_str(), &endptr, 10);

	if ((*endptr) != '\0')
		return false;

	return true;
}

bool getCoordinate(string s, TPoint *point)
{
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

bool createNewGame(int *deskSize, int *countPlayers) {
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
			return 1;
		}
		if ((option != 6 && option != 10 && option != 8 && option != 12))	{
			cerr << "Velkost hracej dosky moze byt len 6,8,10,12." << endl;
			return 1;
		}
		*deskSize = option;
	}

	//pocet hracov a ich typ pc- clovek
	cout << "Zadajte pocet realnych hracov, musi byt aspon jeden: ";
	cin >> enter;
	if (!strToInt(enter, &option)  || (option != 1 && option != 2)) {
		cerr << "Zadana hodnota musi byt cislo 1 alebo 2." << endl;
		return 1;
	}
	*countPlayers = option;
	return true;
}

void help() {
	cout << "NAPOVEDA:" << endl;
	cout << "\ttah - zadajte suradnice oddelene ciarkou bez medzier[stlpec,riadok], napriklad[a,2].";
	cout << endl;
	cout << "\tpass - v pripade, ze nemate moznost posunu zadajte \"pass\"" << endl;
	cout << "\tsave - v pripade, ze chcete ulozit rozohratu hru" << endl;
	cout << "\tload - v pripade, ze chcete nacitat rozohratu hru" << endl;
	cout << "\tnew - pre vytvorenie novej hry" << endl;
	cout << "\tchange - pre zmenu hry" << endl;
	cout << "\tkonec - pre ukoncenie hry" << endl;
}


int main() {
	int option, deskSize, countPlayers;
	string enter;
	TPoint point;
	bool isNewGame = true;

	int actGameIndex = -1;
	vector <OthelloGame> game;

	int actColor = NON_DEFINE;
	while (1)
	{
		if (isNewGame) {
			createNewGame(&deskSize, &countPlayers);

			//vytvorenie hracej dosky
			game.push_back(OthelloGame(deskSize));
			actGameIndex++;

			if (option == 2)
				game[actGameIndex].setPlayer(PLAYER_PERSON, PLAYER_PERSON);
			else
				game[actGameIndex].setPlayer(PLAYER_PERSON, PLAYER_PC);

			game[actGameIndex].board->setField({3,3}, WHITE);
			game[actGameIndex].board->setField({4,3}, BLACK);
			game[actGameIndex].board->setField({3,4}, BLACK);
			game[actGameIndex].board->setField({4,4}, WHITE);

			isNewGame = false;
		}
		cout << "ID hry: " << actGameIndex << endl;
		cout << "Hrac1(biele): " << game[actGameIndex].p1->getScore() << endl;
		cout << "Hrac2(cierne): " << game[actGameIndex].p2->getScore() << endl;

		cout << "Na tahu je hrac: ";
		if (game[actGameIndex].p1->move)
			cout << "Hrac1." << endl;
		else
			cout << "Hrac2." << endl;

		//renderMap(game[actGameIndex].board);

		//renderMap
		int deskSize = game[actGameIndex].board->deskSize;

		//vykreslenie pismeniek
		cout << "   ";
		for (int i = 0; i < deskSize; i++)
			//cout << char(i+'a') << " ";
			cout << i << " ";

		cout << endl;


		for (int i = 0; i < deskSize; i++){
			cout << i << ": ";
			for (int j = 0; j < deskSize; j++) {
				if (game[actGameIndex].board->getField({j,i}) == WHITE)
					//cout << "-";
					cout << "●";
				else if (game[actGameIndex].board->getField({j,i}) == BLACK)
					//cout << "+";
					cout << "○";
				else {
					cout << " ";
				}
				cout << " ";
			}
			cout << endl;
		}
		//renderMap

		cout << "Zadajte suradnice[stlpec,riadok] alebo pass: ";
		cin >> enter;

		if (!getCoordinate(enter, &point)) {
			if (enter == "pass")
				cout << "pass" << endl;
			else if (enter == "save") {
				cout << "Zadajte subor, do ktore sa ulozi hra: ";
				cin >> enter;
				if (game[actGameIndex].saveGameData(enter))
					cout << "Herne data boli ulozene v subore: " << enter << endl;
				else
					cerr << "Problem pri ukladani hernych dat, opakujte ulozenie." << endl;
			} else if (enter == "load") {
				cout << "Zadajte nazov suboru, s ktoreho sa maju nacitat data: ";
				cin >> enter;
				if (game[actGameIndex].loadGameData(enter))
					cout << "Data boli nacitane." << endl;
				else
					cerr << "Problem pri nacitani dat" << endl;
			} else if (enter == "konec") {
				cout << "Hra bola ukoncena." << endl;
				break;
			} else if (enter == "new") {
				isNewGame = true;
			} else if (enter == "change") {
				cout << "Zadajte cislo hry: ";
				cin >> enter;
				if (strToInt(enter, &option) && option >=0 && option < game.size())
					actGameIndex = option;
				else
					cerr << "Zadaza zla hodnota hry" << endl;

			} else {
				cerr << "Nepodporovany string, opakujte volbu." << endl;
			}
		} else {
			if (game[actGameIndex].move(point, game[actGameIndex].getActPlayerColor(), false) ) {
				cout << "presunute" << endl;
				game[actGameIndex].nextPlayer();
			} else
				cout << "nemozno presunut opakujte pokus" << endl;
		}

	}





	return 0;
}
