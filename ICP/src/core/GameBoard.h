/**
 * Game Board
 *
 * @author			Klára Nečasová <xnecas24>
 * @author			Peter Tisovčík <xtisov00>
 */

#ifndef GAME_BOARD_H
#define GAME_BOARD_H

#include <iostream>
#include <vector>

typedef struct {
	int x;
	int y;
} TPoint;

//const int white = 1;

#define WHITE 1
#define BLACK 0
#define NON_DEFINE -1
#define HINT 8

class GameBoard
{
protected:
	int deskSize;
public:
	std::vector <int> gameDesk;

	/**
	* Test ci sa jedna o validne suradnice
	* @param	point		Suradnice, ktore sa maju overit
	* @return 	True ak je suradnica platna inak false
	*/
	bool checkCoordinates(TPoint point);

	/**
	 * Test ci je na danej pozicie umiestneny uz nejaky kamen
	 * @param	point		Suradnice, ktore sa maju overit
	 * @return 	True ak je na danych suradniciach uz nejaky kamen inak false
	 */
	bool isFieldEmpty(TPoint point);

	/**
	 * Prevedie x,y suradnice na odpovedajuci index vo vektore cisiel
	 * @param	point		Suradnice, ktore sa maju previest
	 * @return 	Index prvku vo vektore
	 */
	int getPointIndex(TPoint point);

public:
	/**
	 * Algoritmus, ktory prehladava okolie a zistuje ci je dany tah validny,
	 * ci je mozne otocit aspon jeden kamen a ci nie je volne policko medzi nimi
	 * otacanymi kamenmi
	 * @param	point		Suradnice, kde sa ma umiestnit novy kamen
	 * @param 	color		Farba kamena, ktory sa ma umiestnit
	 * @return 	Vektor bodov, ktore je mozne prekreslit danym tahom
	 */
	std::vector <TPoint> findingMoves(TPoint point, int color);

	/**
	 * Iniclializacia hracej dosky na zaciatocne 4 kamene
	 * @param	deskSize	Velkost hracej dosky
	 */
	GameBoard(int deskSize);

	/**
	 * Nastavi zakladne 4 hracie kamane na spravne pozicie podla velkosti dosky
	 */
	void initDesk();

	/**
	 * Nastavenie farby kamena na zaklade zadanych suradnic a farby
	 * @param	point		Bod, na ktorom sa ma dana farba zmenit
	 * @param	color		Farba kamena, na ktoru sa ma otocit
	 */
	void setStone(TPoint points, int color);

	/**
	* Zistenie farby kamena na danych suradniciach
	* @param	point		Suradnice, pozadovaneho bodu
	* @return	Farba daneho kamena na zadanych suradniciach
	*/
	int getStone(TPoint point);

	/**
	* Velkost hracej dosky
	* @return	Velkost hracej dosky ako cele cislo
	*/
	int getDeskSize();

	/**
	 * Aktualizuje hracie kamene na danu farbu
	 * @param	points		Body, na ktorych sa ma dana farba zmenit
	 * @param	color		Farba kamena, na ktoru sa ma otocit
	 */
	void updateBoard(std::vector <TPoint> points, int color);

	/**
	 * Pocet kamenov danej farby
	 * @param	color		Farba kamena
	 * @return Pocet kamenov danej farby na hracej doske
	 */
	int getCountStone(int color);

	/**
	 * Presunie kamen danej farby na zadane suradnice a zaplni prazdne policka
	 * ak sa jedna o neplatny tah vrati naspat vykonane zmeny
	 * @param	point		Suradnice, kde sa ma vykonat tah, prekreslenie
	 * @param	color		Farba kamena, na ktoru sa maju prekreslit ostatne kamene
	 * @return 	Vekto bodov, ktore sa maju prekreslit, ak sa nema co prekreslit
	 *			vrati sa prazdny vektor - tah je neplatny
	 */
	std::vector <TPoint> moveStone(TPoint point, int color);

	/**
	 * Oznaci suradnice kamenov, ktore je mozne v nasledujucom tahu prevratit
	 * @param	point		Suradnice kamena, ktore sa maju overit ci to je platny tah
	 * @param	color		Farba zadaneho kamena, ktory sa bude overovat
	 */
	int moveHint(TPoint point, int color);
};

#endif
