/**
 * @file			cli.h
 * @author			Klára Nečasová <xnecas24>
 * @author			Peter Tisovčík <xtisov00>
 */

#include <iostream>
#include <vector>
#include "GameData.h"
#include "GameManager.h"

/**
 * Trieda vytvara CLI rozhranie pre uzivatela,
 * vypisuje informacie uzivatelovi, co moze spravit,
 * co ma zadat, popripadne osetruje zle vstupy
 */
class CLI {

	/**
	 * Vykreslenie kamenov a hracej plochy.
	 * @param	board		Hracia doska, ktora sa ma vykreslit
	 */
	void renderMap(GameManager &board);

	/**
	 * Prevod stringu na cislo
	 * @param	str			String, ktory sa ma previest na cislo
	 * @param	value		Pointer kam sa ma ulozit vysledok
	 * @return	True v pripade, ze prevod prebehol v poriadku inak false
	 */
	bool strToInt(std::string str, int *value);

	/**
	 * Ziska x,y suradnice zo zadaneho stringu
	 * @param	str			String, ktory sa ma previes na suradnice
	 * @param	point		Pointer TPoint kam sa ulozia suradnice
	 * @return	True ak boli zadane suradnice v spravnom formate inak false
	 */
	bool getCoordinate(std::string str, TPoint *point);

	/**
	 * Vytvorenie novej hry na zaklade zadanych udajov od uzivatela.
	 * @param	deskSize		Pointer, kde sa ulozi velkost hracej dosky
	 * @param	countPlayers	Pointer, kde sa ulozi pocet realnych hracov
	 * @param	alg				Pointer, kde sa ulozi cislo Algoritmu,
	 * 							ktory sa ma pouzit pre PC
	 * @return	True v pripade, ze boli zadane vsetky parametre spravne,
	 * 			inak false
	 */
	bool createNewGame(int *deskSize, int *countPlayers, int *alg);

	/**
	 * Vypis zakladnych prikazov hry.
	 */
	void help();

public:
	void show();
};
