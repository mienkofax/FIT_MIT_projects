#ifndef GAME
#define GAME

#include <QList>
#include "square.h"
#include <QGraphicsScene>
#include <QGraphicsSceneMouseEvent>
#include "GameManager.h"

class Game : public QObject {
    Q_OBJECT
public:
    Game(int windowsSize);

    void placeSquare(int deskSize);

    QGraphicsScene* scene;
    void createStone(int xPos, int yPos, Qt::GlobalColor  color , int deskSize);
    void createStone(int xPos, int yPos, Qt::GlobalColor  color);
    void drawStone(std::shared_ptr<GameManager> board);
    void setActivePlayer(int x, int y, Qt::GlobalColor color);
    QList <QGraphicsEllipseItem *> hints;

public slots:
    void getPosition(int x, int y);

signals:
    void moveToPosition(int x, int y);

private:
    void createHexColumn(int x, int y, int numHexes);
    QList <Square*> squares;

    QList <QGraphicsEllipseItem *> stones;
    QGraphicsEllipseItem * lastElipseItem;
    int windowSize;
};

#endif // GAME

