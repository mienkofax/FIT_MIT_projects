#include "game.h"
#include <QMessageBox>
#include <QObject>
#include <QGraphicsDropShadowEffect>


Game::Game(int windowSize)
{
    scene = new QGraphicsScene();
    this->windowSize = windowSize;
}

void Game::getPosition(int x, int y) {
    emit moveToPosition(x, y);
}

void Game::placeSquare(int deskSize)
{
    //vypocet velkosti stvorceka
    float size = windowSize/deskSize;

    //vytvorenie hraciu dosku zo stvorcov
    for (int j = 0; j < deskSize; j++)
    {
        for (int i = 0; i < deskSize; i++) {
            Square* square = new Square(__null, size);
            square->setPos(size*j,size*i);
            squares.append(square);
            scene->addItem(square);
            square->setOwner();
            connect(square, &Square::getClickPosition, this, &Game::getPosition);
        }
    }
}

void Game::createStone(int xPos, int yPos, Qt::GlobalColor  color, int deskSize)
{
    //vypocet velkosti
    float size = this->windowSize/deskSize;

    QGraphicsDropShadowEffect * effect = new QGraphicsDropShadowEffect();
    effect->setBlurRadius(size/2);

    //pozicia kamena
    float x = 3 + size * xPos;
    float y = 3 + size * yPos;
    QGraphicsEllipseItem *ell = new QGraphicsEllipseItem(x, y, size-6,size-6);

    //ulozenie kamena, pre neskorsie odstranenie
    if (color == Qt::darkCyan) {
        hints.append(ell);
    } else
        stones.append(ell);

    QBrush br(color);
    ell->setGraphicsEffect(effect);
    ell->setBrush(br);

    scene->addItem(ell);
}

void Game::createStone(int x, int y, Qt::GlobalColor  color)
{
    QGraphicsDropShadowEffect * effect = new QGraphicsDropShadowEffect();
    effect->setBlurRadius(45/2);

    QGraphicsEllipseItem *ell = new QGraphicsEllipseItem(x, y, 45,45);

    if (color == Qt::darkCyan) {
        hints.append(ell);
    }

    QBrush br(color);
    ell->setBrush(br);
    ell->setGraphicsEffect(effect);

    scene->addItem(ell);

    lastElipseItem = ell;
}

void Game::drawStone(std::shared_ptr <GameManager> board)
{
    int deskSize = board->getDeskSize();

    //odstranenie starych kamenov
    for (QGraphicsEllipseItem *item : hints)
           scene->removeItem(item);

    for (QGraphicsEllipseItem *item : stones)
           scene->removeItem(item);

    hints.clear();
    stones.clear();

    //vykreslenie novych kamenov
    for (int i = 0; i < deskSize; i++) {
        for (int j = 0; j < deskSize; j++) {
            if (board->getStone({i, j}) == WHITE)
                createStone(i,j, Qt::white, deskSize);
            else if(board->getStone({i, j}) == BLACK)
                createStone(i,j, Qt::black, deskSize);
            else if (board->getStone({i, j}) == 8)
                createStone(i,j, Qt::darkCyan, deskSize);
        }
    }
}

void Game::setActivePlayer(int x, int y, Qt::GlobalColor color)
{
    delete lastElipseItem;
    createStone(x, y, color);
}



