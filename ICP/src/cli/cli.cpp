/**
 * @file			cli.cpp
 * @author			Klára Nečasová <xnecas24>
 * @author			Peter Tisovčík <xtisov00>
 */

#include <iostream>
#include <vector>
#include <iomanip>
#include "GameManager.h"
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
			else if (board.getStone({j,i}) == HINT)
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

	cout << "Zadajte veľkosť hracej dosky[8 - predvolená veľkosť znak: *]: ";

	//nacitanie velkosti
	cin >> enter;
	if (enter == "*") {
		cout << "Hracia doska sa vytvori s predvolenou veľkosťou 8." << endl;
		*deskSize = 8;
	} else {
		if (!strToInt(enter, &option)) {
			cout << "Zadaná hodnota musí byt číslo." << endl;
			return false;
		}
		if ((option != 6 && option != 10 && option != 8 && option != 12))	{
			cout << "Veľkosť hracej dosky môže byť len 6,8,10,12." << endl;
			return false;
		}
		*deskSize = option;
	}

	//pocet hracov a ich typ PC - clovek
	cout << "Zadajte počet reálnych hráčov, musí byť aspoň jeden: ";
	cin >> enter;
	if (!strToInt(enter, &option)  || (option != 1 && option != 2)) {
		cout << "Zadaná hodnota musí byť číslo 1 alebo 2." << endl;
		return false;
	} else {
		*countPlayers = option;

		if (option == 1) {
		cout << "Zadajte číslo algoritmu, ktorý sa ma použiť: ";
		cin >> enter;
			if (strToInt(enter, &option)) {
				*alg = option;
			} else {
				cout << "Bola zadaná neplatná voľba." << endl;
				return false;
			}
		}
	}

	return true;
}

void CLI::help() {
	cout << "NÁPOVEDA:" << endl;
	cout << "\tx,y    - zadajte súradnice oddelené čiarkou, bez medzier[stlpec,riadok], napríklad:a,2";
	cout << endl;
	cout << "\tpass   - v prípade, že nemáte možnosť urobiť ťah, zadajte pass" << endl;
	cout << "\tsave   - v prípade, že chcete uložiť rozohranú hru" << endl;
	cout << "\tload   - v prípade, že chcete načítať rozohranú hru" << endl;
	cout << "\tundo   - krok späť" << endl;
	cout << "\tredo   - krok vpred" << endl;
	cout << "\tnew    - vytvorenie novej hry" << endl;
	cout << "\tchange - zmena hry" << endl;
	cout << "\tend    - ukončenie hry" << endl;
}

void CLI::show() {
	GameManager manager;
	GameManager& gameManager = manager;

	int option, deskSize, countPlayers, alg;
	string enter;
	TPoint point;

	cout << "Hra: Othello \nAutori: xnecas24, xtisov00" << endl;

	while(1) {
		cout << "------------------------------------\n";

		if (!gameManager.isEmpty()) {
			if(manager.isActiveP1())
			cout << "Na rade je hráč 1(Biele kamene): ";
			if (manager.isActiveP2())
			cout << "Na rade je hráč 2(Čierne kamene): ";
		}
		else
			help();

		if (gameManager.livePlayer())
			cin >> enter;

		if (!getCoordinate(enter, &point)) {
			if (enter == "pass") {
				if (manager.moveStone({0,0}, true)) {
					cout << "Pass sa vykonal." << endl;
					renderMap(gameManager);
				} else
					cout << "Pass je možné zadať len, keď nie je možné spraviť iný ťah." << endl;
			} else if (enter == "save") {
				cout << "Zadajte súbor, do ktorého sa uloží hra: ";
				cin >> enter;
				if (manager.saveGame(enter))
					cout << "Herne dáta boli uložene v súbore: " << enter << endl;
				else
					cout << "Problém pri ukladaní herných dát, opakujte uloženie." << endl;
			} else if (enter == "load") {
				cout << "Zadajte názov súboru, s ktorého sa majú načítať dáta: ";
				cin >> enter;
				if (manager.loadGame(enter))
					cout << "Dáta boli načítané." << endl;
				else
					cout << "Problém pri načítaní dát." << endl;
				renderMap(gameManager);
			} else if (enter == "end") {
				cout << "Hra bola ukončená." << endl;
				break;
			} else if (enter == "new") {
				help();
				if (!createNewGame(&deskSize, &countPlayers, &alg))
					cout << "Nebolo možné vytvoriť hru." << endl;
				else {
					manager.newGame(deskSize, countPlayers, alg-1);
					renderMap(gameManager);
				}
			} else if (enter == "change") {
				cout << "Zadajte číslo hry: ";
				cin >> enter;
				if (strToInt(enter, &option) && option >=0 ) {
					if (manager.changeGame(option))
						cout << "Hra úspešne zmenená." << endl;
					else
						cout << "Bola zadaná nesprávna hodnota. " << endl;
					renderMap(gameManager);
				} else
					cout << "Bola zadaná zlá hodnota hry." << endl;

			} else if (enter == "undo") {
				if (!gameManager.undo())
					cout << "Nie je možné ísť späť." << endl;
				renderMap(gameManager);
			} else if (enter == "redo") {
				if (!gameManager.redo())
					cout << "Nie je možné sa vrátiť vpred." << endl;
				renderMap(gameManager);
			} else {
				cout << "Nepodporovaný reťazec, opakujte voľbu." << endl;
			}
		} else {
			if (!gameManager.moveStone(point, false) )
				cout << "Nemožno presunúť kameň, opakujte pokus." << endl;

			renderMap(gameManager);
		}

		//vypis skore, ked je aspon jedna aktivna hra
		if (!gameManager.isEmpty()) {
			cout << "Skóre Hráč 1::" << gameManager.getP1Score() << endl;
			cout << "Skúre hráč 2::" << gameManager.getP2Score() << endl;
			cout << "ID hry: " << gameManager.getGameID() << endl;
		}

		if (manager.endGame())
			cout << "\t\t!!!Koniec hry.!!!" << endl;
	}
}
