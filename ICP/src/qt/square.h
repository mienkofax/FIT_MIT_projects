#ifndef SQUARE
#define SQUARE

#include <QGraphicsPolygonItem>
#include <QGraphicsSceneMouseEvent>
#include <QMessageBox>
#include <QObject>

class Square: public QObject, public QGraphicsPolygonItem {
    Q_OBJECT
public:
    Square(QGraphicsItem* parent=NULL, int size=50);

    void mousePressEvent(QGraphicsSceneMouseEvent *event);

    void setOwner();

signals:
    void getClickPosition(int x, int y);
private:
    QString owner;
    int size;
};

#endif // SQUARE

