#ifndef STRATEGY_H
#define STRATEGY_H

#include <iostream>
#include "GameBoard.h"
#include <vector>
#include <memory>

class Strategy
{
public:
	Strategy();

	/**
	 * Rozhranie, ktore sluzi pre jednotlive algoritmy. V tejto
	 * metode sa vykona vypocet suradnic s urcitou inteligenciou,
	 * ktore kamane ma PC otocit.
	 * @param	point		Suradnice, kam ma PC urobit tah
	 * @param	board		Hracia doska, kde su umiestnene kamene
	 * @return	True v pripade, ze neexistuje tah, inak false
	 */
	virtual bool executeMove(TPoint* point,  GameBoard board) = 0;
};

class Alg1 : public Strategy
{
public:
	/**
	 * Najdenie tahu pre PC, na zaklade prvych suradnic,
	 * ktorymi je mozne tahat.
	 * @param	point		Suradnice, kam ma PC urobit tah
	 * @param	board		Hracia doska, kde su umiestnene kamene
	 * @return	True v pripade, ze neexistuje tah, inak false
	 */
	bool executeMove(TPoint*, GameBoard board);
};

class Alg2 : public Strategy
{
public:
	/**
	 * Najdenie tahu pre PC, na zaklade nahodneho vyberu,
	 * z vsetkych moznych tahov
	 * @param	point		Suradnice, kam ma PC urobit tah
	 * @param	board		Hracia doska, kde su umiestnene kamene
	 * @return	True v pripade, ze neexistuje tah, inak false
	 */
	bool executeMove(TPoint*,  GameBoard board);
};

#endif
