#include "gui.h"
#include "ui_gui.h"
#include <QFileDialog>
#include <QString>
#include "game.h"
#include <QThread>

#include <QListView>


gui::gui(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::gui)
{
    ui->setupUi(this);

    this->setFixedSize(320,320);

    //nastavenie na prvy tab a inicializacia dat pre vytvorenie hry
    ui->stackedWidget->setCurrentIndex(0);
    setComboBoxNewGameData();

    manager = std::shared_ptr <GameManager>(new GameManager());
    game = new Game(480);

    //vytvorenie kamenov pre aktualne skore
    game->createStone(500, 245,Qt::white);
    game->createStone(500, 300,Qt::black);

    //zaciatocny kamen, kto je na tahu
    game->createStone(540, 140,Qt::white);
}

void gui::setComboBoxNewGameData()
{
    QStringList list;

    //velkosti hracej dosky
    list = (QStringList() << "6" << "8" << "10" << "12");
    ui->comboBoxDeskSize->addItems(list);

    list.clear();

    //1. hrac
    list = (QStringList() << "Clovek");
    ui->comboBoxPlayer1->addItems(list);


    list.clear();

    //2.hrac
    list = (QStringList() << "Clovek" << "PC");
    ui->comboBoxPlayer2->addItems(list);

    list.clear();

    //algoritmy
    list = (QStringList() << "Alg1" << "Alg2");
    ui->comboBoxAlgorithm->addItems(list);
}

void gui::initComboBoxNewGameData()
{
    //nastavenie obrazovky 1
     ui->stackedWidget->setCurrentIndex(1);

     //nastavenie velkosti hracej dosky, 8 ako predvolena hodnota
     ui->comboBoxDeskSize->setCurrentIndex(1);

     //nastavenie prveho hraca
     ui->comboBoxPlayer1->setEnabled(false);
     ui->comboBoxPlayer1->setCurrentIndex(0);

     //nastavenie hracov
     ui->comboBoxPlayer2->setCurrentIndex(0);

     //nastavenoie algoritmu
     ui->comboBoxAlgorithm->setCurrentIndex(1);
}

void gui::resizeToGame()
{
    //velkost okna
    this->setFixedSize(660,550);

    //velkost hracej dosky
    ui->graphicsView->setFixedSize(640,480+3);
}

void gui::updateGameData()
{
    ui->labelPlayer1Score->setText(QString::number(manager->getP1Score()));
    ui->labelPlayer2Score->setText(QString::number(manager->getP2Score()));

    //vykreslenie farby kamena, ktory je na tahu
    if (manager->isActiveP1())
        game->setActivePlayer(540, 140,Qt::white);

    if (manager->isActiveP2())
        game->setActivePlayer(540, 140,Qt::black);
}

void gui::status(QString text, bool isOk)
{
    if (isOk)
        ui->labelStatus->setStyleSheet("QLabel {color : green;}");
    else
        ui->labelStatus->setStyleSheet("QLabel {color : red;}");

    ui->labelStatus->setText(text);
}

void gui::createComboBoxString(QString deskSize)
{
    //vytvorenie stringu, ktory identifikuje hru a zobrazi sa v comboBoxe
    QString gameString = QString::number(ui->comboBoxGame->count()) + ": Game: " +
            deskSize + "x" + deskSize;
    ui->comboBoxGame->addItem(gameString);

    //nastavenie poslednej pridanej polozky ako aktivnej
    ui->comboBoxGame->setCurrentIndex(ui->comboBoxGame->count()-1);
}

void gui::moveToPosition(int x, int y)
{
    //spracovanie tahu
    if (manager->moveStone({x, y}, false))
       status("Uspesny tah.", true);
    else
       status("Nesuspesny tah.", false);

    //tah pocitaca a pockanie 0.5s
    if (!manager->livePlayer()) {
        QThread::msleep(500);
        manager->moveStone({-1,-1}, false);
    }

    //prekreslenie kamenov a aktualizacia skore
    game->drawStone(manager);
    updateGameData();
}

void gui::widgetCreateNewGame()
{
    //okno pre vytvorenie novej hry
    ui->stackedWidget->setCurrentIndex(1);

    //nastavenie predvolenych hodnot
    initComboBoxNewGameData();
    this->setFixedSize(320,360);
}

void gui::widgetLoadNewGame()
{
    QString filename = QFileDialog::getOpenFileName(
                this, tr("Open game data file"), "", "All files(*)");

    //ak sa nacitali udaje spravne vytvori sa hra
    if(manager->loadGame(filename.toStdString())) {

        //vytvorenie hracej dosky, vykreslenie kamenov
        game->placeSquare(manager->getDeskSize());
        game->drawStone(manager);
        updateGameData();

        //aktualizacia score
        ui->graphicsView->setScene(game->scene);

        ui->stackedWidget->setCurrentIndex(2);
        resizeToGame();

        //pridanie hry do zoznamu aktivnych hier
        createComboBoxString(QString::number(manager->getDeskSize()));

        connect(game, &Game::moveToPosition, this, &gui::moveToPosition);
        status("Hra uspesne nacitana.", true);
   } else
        status("Hru sa nepodarilo nacitat.", false);
}

gui::~gui()
{
    delete game;
    delete ui;
}

void gui::on_buttonNewGame_clicked()
{
    widgetCreateNewGame();
}

void gui::on_buttonNewGame2_clicked()
{
    widgetCreateNewGame();
}

void gui::on_buttonBack_clicked()
{
    ui->stackedWidget->setCurrentIndex(0);
}

void gui::on_buttonLoadGame_clicked()
{
   widgetLoadNewGame();
}

void gui::on_comboBoxPlayer2_currentIndexChanged(int index)
{
    if (index == 0) {
        ui->label_4->setEnabled(false);
        ui->comboBoxAlgorithm->setEnabled(false);

    } else {
        ui->label_4->setEnabled(true);
        ui->comboBoxAlgorithm->setEnabled(true);
    }
}

void gui::on_pushButton_12_clicked()
{
    int players = 1;

    if (ui->comboBoxPlayer2->currentIndex() == 0)
        players = 2;

    //create new game
    manager->newGame(ui->comboBoxDeskSize->currentText().toInt(),
                     players, ui->comboBoxAlgorithm->currentIndex()+1);

    ui->stackedWidget->setCurrentIndex(2);

    //vytvorenie hracej dosky, vykreslenie kamenov a zmena velkosti okna
    game->placeSquare(ui->comboBoxDeskSize->currentText().toInt());
    game->drawStone(manager);
    resizeToGame();

    //pridanie hry do zoznamu aktivnych hier
    createComboBoxString(ui->comboBoxDeskSize->currentText());

    ui->graphicsView->setScene(game->scene);
    connect(game, &Game::moveToPosition, this, &gui::moveToPosition);
    status("Hra bola uspesne vytvorena.", true);
}

void gui::on_buttonSave_clicked()
{
    QString filename = QFileDialog::getSaveFileName(
                this, tr("Save game data file"), "", "All files (*.*)");

    if (manager->saveGame(filename.toStdString()))
        status("Hra bola uspesne ulozena.", true);
    else
        status("Hru sa nepodarilo ulozit.", false);
}

void gui::on_buttonLoad2_clicked()
{
    widgetLoadNewGame();
}

void gui::on_buttonUndo_clicked()
{
    if (manager->undo())

         status("Krok spat uspesny.", true);
    else
         status("Krok spat neuspesny, nie je kam sa vratit.", false);

    game->drawStone(manager);
    updateGameData();
}

void gui::on_buttonRedo_clicked()
{
    if (manager->redo())
        status("Krok vpred uspesny.", true);
    else
        status("Krok vpred neuspesny, nie je kam ist.", false);

    game->drawStone(manager);
    updateGameData();
}

void gui::on_buttonBackToGame_clicked()
{
    ui->stackedWidget->setCurrentIndex(2);
    resizeToGame();
}

void gui::on_buttonChangeGame_clicked()
{
    if (manager->changeGame(ui->comboBoxGame->currentIndex())) {

        //vykresenie hracej plochy a nasledne kamenov
        game->placeSquare(manager->getDeskSize());
        game->drawStone(manager);
        updateGameData();

        ui->graphicsView->setScene(game->scene);
        status("Hra bola uspesne zmenena.", true);
    } else
        status("Hru sa nepodarilo zmenit", false);
}

void gui::on_buttonDemoGame_clicked()
{
    QMessageBox msgBox;
    msgBox.setText("Help");
    msgBox.exec();
}

void gui::on_buttonPass_clicked()
{
    if (manager->moveStone({-1,-1}, true))
        status("Pass.", true);
    else
        status("Nie je mozne urobit pass", false);
}
