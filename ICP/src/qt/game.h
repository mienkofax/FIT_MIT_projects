/**
 * @file			game.h
 * @author			Klára Nečasová <xnecas24>
 * @author			Peter Tisovčík <xtisov00>
 */

#ifndef GAME
#define GAME

#include <QList>
#include <QGraphicsScene>
#include <QGraphicsSceneMouseEvent>
#include "square.h"
#include "GameManager.h"

/**
 * Trieda obsahujuca objekty hernej sceny.
 */
class Game : public QObject {
	Q_OBJECT

	QList <Square*> squares;
	QList <QGraphicsEllipseItem *> stones;
	QList <QGraphicsEllipseItem *> hints;
	QGraphicsEllipseItem *lastElipseItem;
	int windowSize;

public:
	QGraphicsScene* scene;

	/**
	 * Constructor hry, ktory vytvori hraciu scenu
	 * @param	windowSize	Velkos hracieho okna
	 */
	Game(int windowSize);

	/**
	 * Vytvorenie hracich policok na zaklade hracej dosky
	 * @param	deskSize	Velkost hracej dosky
	 */
	void placeSquare(int deskSize);

	/**
	 * Vytvorenie kamena a umiestnenie na zadane index hracej dosky na
	 * zaklade velkosti hracej dosky, na zaklade, ktorej sa vypocita
	 * velkost kamena
	 * @param	xPos		X index hracej dosky, kde sa ma umiestnit kamen
	 * @param	yPos		Y index hracej dosky, kde sa ma umiestnit kamen
	 * @param	color		Farba kamena
	 * @param	deskSize	Velkost hracej dosky
	 */
	void createStone(int xPos, int yPos, Qt::GlobalColor  color , int deskSize);

	/**
	 * Vytvorenie kamena a umiestnenie na zadane x, y suradni hracej plochy
	 * @param	x			X suradnica hracej plochy, kde sa ma umiestnit kamen
	 * @param	y			Y suradnica hracej plochy, kde sa ma umiestnit kamen
	 * @param	color		Farba kamena
	 */
	void createStone(int x, int y, Qt::GlobalColor  color);

	/**
	 * Vykreslenie kamenov na hraciu dosku
	 * @param	board		Hracia doska
	 */
	void drawStone(std::shared_ptr<GameManager> board);

	/**
	 * Vykreslenie kamena, ktory znaci farbu aktualneho hraca
	 * @param	x			X suradnica hracej plochy, kde sa ma umiestnit kamen
	 * @param	y			Y suradnica hracej plochy, kde sa ma umiestnit kamen
	 * @param	color		Farba kamena
	 */
	void setActivePlayer(int x, int y, Qt::GlobalColor color);

public slots:
	/**
	 * Slot na, ktory sa napoji signal po kliknuti na dany stvorec
	 * @param	x			X index hracej dosky
	 * @oaram	Y			Y index hracej dosky
	 */
	void getPosition(int x, int y);

signals:
	/**
	 * Signal pre stlacene suradnice s triedy Square
	 * @param	x			X index hracej dosky
	 * @oaram	Y			Y index hracej dosky
	 */
	void moveToPosition(int x, int y);
};

#endif // GAME
