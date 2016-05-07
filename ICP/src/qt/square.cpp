/**
 * @file			square.cpp
 * @author			Klára Nečasová <xnecas24>
 * @author			Peter Tisovčík <xtisov00>
 */

#include <square.h>
#include <QBrush>

Square::Square(QGraphicsItem *parent, int size)
{
	QVector<QPointF> squarePoints;
	squarePoints << QPointF(0,0) << QPointF(1,0)  << QPoint(1,1) << QPointF(0,1);

	for (int i = 0; i < squarePoints.size(); i++)
		squarePoints[i] *= size;

	//vytvorenie a vykreslenie obdlznika
	QPolygonF square(squarePoints);
	setPolygon(square);

	//nastavenie stetca
	QBrush brush;
	brush.setStyle(Qt::SolidPattern);
	brush.setColor(Qt::green);
	setBrush(brush);
}

void Square::mousePressEvent(QGraphicsSceneMouseEvent *event)
{
	QPointF position = event->scenePos();

	//signal s poziciou kde sa kliklo, pozicia = index stvorca
	emit getClickPosition(int(position.rx()/size), int(position.ry()/size));
}
