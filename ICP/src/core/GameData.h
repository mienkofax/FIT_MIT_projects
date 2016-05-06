#ifndef GAME_DATA_H
#define GAME_DATA_H

#include <iostream>
#include <vector>
#include "GameBoard.h"

typedef struct {
	int moveNumber;		//cislo tahu
	std::vector <TPoint> points;
	int color; 			//farba na ktoru sa to ma prekreslit
	bool isActive;
} TGameMove;

class GameData
{
	std::vector <TGameMove> history;

public:
	GameData();

	/**
	 * Prida tah hry so historie
	 * @param	points		Vector bodov, ktore sa maju pridat do historie
	 * @param 	color 		Farba bodov, na ktoru sa prekreslili
	 */
	void addMoveToHistory(std::vector <TPoint> points, int color);

	/**
	 * Ulozi historiu hry
	 * @param	filename	Nazov suboru, do ktoreho sa maju ulozit herne data
	 * @param	deskSize	Velkost hracej dosky
	 * @param 	players		Pocet realnych hracov v hre
	 * @return	True v pripade, ze sa ulozili data spravne inak false
	 */
	bool saveGameData(std::string filename, int deskSize, int players);

	/**
	 * Nacitanie rozohranej hry
	 * @param	filename	Nazov suboru, s ktoreho sa maju nacitat herne data
	 * @return True v pripade, ze sa podarilo nacitat subor spravne inak false.
	 */
	bool loadGameData(std::string filename);

	/**
	 * Vykona jeden krok v historii spat
	 * @return	Vektor hernych dat, ktore sa zmenili, prazdny vektor
	 * 			ak nie je co vratit spat
	 */
	std::vector <TGameMove> undo();

	/**
	 * Urobi jeden krok v historii dopredu
	 * @return  Vektor hernych dat, ktore sa zmenili, prazdny vektor
	 * 			ak nie je co vrati dopredu
	 */
	std::vector <TGameMove> redo();

	/**
	 * Vrati historiu danej hry, pouziva sa pri prepinani hier
	 * return Vektor hernych tahov
	 */
	std::vector <TGameMove> getHistory();

	/**
	 * Vymazanie nepotrebnych dat, po tom co sa pouzije undo a zada sa iny tah
	 */
	void removeInvalidData();

	/**
	 * Vrati pocet platnych bodov pri pouzivani undo,
	 * kontrola, ci sa nezadalo undo aj ked nie je co vratit,
	 * uz nie je mozne ist viacej dozadu
	 * @return	True v pripade, ze je undo platne inak false
	 */
	bool checkUndo();

	/**
	* Vrati pocet platnych bodov pri pouzivani redo,
	* kontrola, ci sa nezadalo redo aj ked nie je co vratit,
	* uz nie je mozne ist viacej dopredu
	* @return	True v pripade, ze je undo platne inak false
	 */
	bool checkRedo();
};

#endif
