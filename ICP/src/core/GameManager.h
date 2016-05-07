/**
 * @file			GameManager.h
 * @author			Klára Nečasová <xnecas24>
 * @author			Peter Tisovčík <xtisov00>
 */

#ifndef GAME_MANAGER_H
#define GAME_MANAGER_H

#include <iostream>
#include <vector>
#include <memory>
#include "GameBoard.h"
#include "GameData.h"
#include "Player.h"

/**
 * Struktura uchovavajuca jednu hru, hra pozostava
 * z hernej dosky, z hernych dat(jednotlvie tahy) a hracov
 */
typedef struct {
	GameBoard board;			/**< Hracia doska */
	GameData data;				/**< Herne data (jednotlive tahy) */
	std::shared_ptr<Player> p1;	/**< Informacie o hraci cislo 1 */
	std::shared_ptr<Player> p2;	/**< Informacie o hraci cislo 2 */
} TGame;

/**
 * Trieda spaja hraciu dosku, informacie o hre a hracov
 * do jednej struktury a ponuka metody, ktorymi je mozne
 * vytvarat upravovat dosku.
 */
class GameManager {
	std::vector <TGame> games;
	int activeGameIndex;
	bool firstGame;
	TGame *game;

public:
	GameManager();

	/**
	 * Vytvori novu hru a priradi jej hraciu dosku, historiu tahov a hracov
	 * @param	deskSize	Velkost hracej dosky
	 * @param	players		Pocet realnych hracov v danej hre
	 * @param	algorithm	Cislo Algoritmu, ktory sa ma pouzit pre PC
	 */
	void newGame(int deskSize, int players, int algorithm);

	/**
	 * Nacita herne data a vytvori novu hru, kde sa zobrazia tieto data
	 * @param	filename	Nazov suboru odkial sa maju nacitat herne data
	 */
	bool loadGame(std::string filename);

	/**
	 * Ulozi aktualne rozohratu hru s potrebnymi udajmi pre znovunacitanie
	 * @param	filename	Subor, kde sa maju ulozit herne data.
	 */
	bool saveGame(std::string filename);

	/**
	 * Zmena aktualnej hry na inu
	 * @param	gameIndex	Index hry, na ktoru sa ma prepnut
	 * @return	True ak dana hra existuje a spravne sa prepla inak false
	 */
	bool changeGame(size_t gameIndex);

	/**
	 * Overi ci je mozne urobit krok spat, vykona krok spat
	 * a prekresli hraciu dosku predchadzajucim tahom
	 * @return	True ak je mozne urobit tah spat, inak false
	 */
	bool undo();

	/**
	 * Overi ci je mozne urobit krok vpred, vykona krok vpred
	 * a prekresli hraciu dosku nasledujucim tahom
	 * @return	True ak je mozne urobit tah vpred, inak false
	 */
	bool redo();

	/**
	 * Dostane bod, kde by sa mal vykonat tak, overi ci je mozne
	 * uskutocnit tah, ak ano prekreslia sa body a zapisu sa
	 * do historie, prepne sa na dalsieho hraca vykreslia
	 * sa nove pomocne body, kde sa da vykonat tah a aktualizuje
	 * sa hracia doska
	 * @param	point		Bod, kde sa ma vykonat tah
	 * @param	isPass		V pripade ze hrac nemoze tahat
	 * @return True ak je to platny, false ak je to neplatny tah
	 */
	bool moveStone(TPoint point, bool isPass);

	/**
	 * Ukonci aktualne rozohranu hru
	 */
	void endGame();

	/**
	 * Zisti ci je aktivny hrac jedna
	 * @return	Ak je aktivny hrac P1 vrati true, inak false
	 */
	bool isActiveP1();

	/**
	 * Zisti ci je aktivny hrac dva
	 * @return	Ak je aktivny hrac P2 vrati true, inak false
	 */
	bool isActiveP2();

	/**
	 * Zisti ci je hrac dva poitac alebo nie
	 * @return	True ak je hrac dva clovek inak false
	 */
	bool livePlayer();

	/**
	 * Zisti ci existuje aspon jedna hra
	 * @return	True ak existuje aspon jedna hra, false ak neexistuje
	 * ani jedna hra
	 */
	bool isEmpty();

	/**
	 * Vrati ID aktualnej hry, pomocou ktoreho sa da identifikovat
	 * konkretna hra a zaroven sa na zaklade ID hry daju prepinat vytvorene hry
	 * @return	Integer, index danej hry, -1 v pripade, ze este nie je vytvorena
	 *			ziadna hra
	 */
	int getGameID();

	/**
	 * Vrati velkost hracej dosky
	 * @return	Integer, velkost hracej dosky, ak nie je nastavena
	 *			ziadna hra vrati sa -1
	 */
	int getDeskSize();

	/**
	 * Zisti sa skore hraca jedna
	 * @return	Integer, skore hraca jedna, -1 v pripade, ze este
	 *			nie je vytvorena ziadna hra
	 */
	int getP1Score();

	/**
	 * Zisti sa skore hraca dva
	 * @return	Integer, skore hraca dva, -1 v pripade, ze este
	 *			nie je vytvorena ziadna hra
	 */
	int getP2Score();

	/**
	 * Vrati hodnotu(farbu) kamena na zadanych suradniciach
	 * @param	point		Suradnice, hladaneho kamena
	 * @return	Integer, hodnota kamena, ktory sa nachadza na danych suradniciach
	 */
	int getStone(TPoint point);

private:
	/**
	 * Vygeneruje pomocne body, ktore znacia mozne tahy daneho hraca
	 * @return	Integer, pocet bodov, ktore sa daju prekreslit
	 */
	int getHint();

	/**
	 * Prepne na dalsieho hraca. Ak je aktivny hrac P1 prepne sa na
	 * hraca P2 a ak je aktivny hrac P2 prepe sa na hraca P1
	 */
	void nextPlayer();

	/**
	 * Aktualizuje skore jednotlivych hracov na zaklade hracovej
	 * farby podla, ktorej sa spocita pocet kamenov danej farby
	 * na hracej doske
	 */
	void updateScore();

	/**
	 * Aktualizuje hraciu dosku, najprv ju inicialuzuje na pociatocny stav a
	 * nasledne vykresli jednotlive tahy ktore dostane
	 * @param	moves		Tahy, ktore sa maju vykreslit
	 */
	void updateBoard(std::vector<TGameMove> moves);
};

#endif
