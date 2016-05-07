/**
 * @file			Player.h
 * @author			Klára Nečasová <xnecas24>
 * @author			Peter Tisovčík <xtisov00>
 */

#ifndef PLAYER_H
#define PLAYER_H

#include <iostream>
#include <memory>
#include "GameBoard.h"
#include "Strategy.h"

/**
 * Trieda popisuje jednotlivych hracov, jednotlivy hraci
 * si uchovavaju informacie o farbe, type a score.
 */
class Player {
protected:
	int color;
	int score;
	int typ;
	void setType(int);

public:
	bool active;
	Player();

	/**
	 * Rozhranie pre dalsi tah, pre cloveka to je len vratenie
	 * rovnakych suradnic ale pre PC to je vygenerovanie suradnic
	 * na zaklade algoritmus
	 * @param	point		Ukazatel na suradnice, ktore sa maju zmenit
	 * @param	board		Hracia doska, z ktorej sa pocitaju tahy
	 *						v jednotlivych algoritmoch
	 * @return	True v pripade, ze je tah validny, false v pripade,
	 *			ze nie je kam potiahnut
	 */
	virtual bool getNextMove(TPoint* point, GameBoard board) = 0;

	/**
	 * Vrati farbu daneho hraca
	 * @return	Integer, farba hraca
	 */
	int getColor();

	/**
	 * Nastavenie farby
	 * @param	color		Farba, ktoru bude mat dany hrac
	 */
	void setColor(int color);

	/**
	 * Zistenie score hraca
	 * @retunr Integer, score hraca
	 */
	int getScore();

	/**
	 * Nastavenie score hraca
	 * @param	score		Score hraca
	 */
	void setScore(int score);

	/**
	 * Zistenie ci sa jedna o PC alebo HUMAN hraca
	 * @return Integer, typ hraca
	 */
	int getType();
};

class Human : public Player
{
public:
	Human();
	bool getNextMove(TPoint*, GameBoard board) ;
};

class PC : public Player
{
	std::shared_ptr<Strategy> algo;

public:
	PC(std::shared_ptr<Strategy>);
	bool getNextMove(TPoint*, GameBoard board);
};

#endif
