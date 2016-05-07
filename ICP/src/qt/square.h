#ifndef SQUARE
#define SQUARE

#include <QGraphicsPolygonItem>
#include <QGraphicsSceneMouseEvent>
#include <QMessageBox>
#include <QObject>


class Square: public QObject, public QGraphicsPolygonItem {
	Q_OBJECT
	QString owner;
	int size;

public:
	/**
	 * Constructor stvorca, ktory predstavuje jedno hracie policko
	 * @param	param		Rodic objektu
	 * @param	size		Velkost stvorca
	 */
	Square(QGraphicsItem* parent=NULL, int size = 50);

	/**
	 * Event pri stlaceni na dany stvorec
	 * @param	event		Pointer na event, ktory vyvolal stlacenie
	 */
	void mousePressEvent(QGraphicsSceneMouseEvent *event);

signals:
	/**
	 * Signal pre poslanie suradnic stvorca, kde sa kliklo
	 * @param	x			X suradnica stvorca, kde sa kliklo
	 * @param	y			Y suradnica stvorca, kde sa kliklo
	 */
	void getClickPosition(int x, int y);
};

#endif // SQUARE
