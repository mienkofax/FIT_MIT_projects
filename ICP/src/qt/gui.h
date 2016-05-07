#ifndef GUI_H
#define GUI_H

#include <QMainWindow>
#include "game.h"
#include "ui_gui.h"

namespace Ui {
class gui;
}

class gui : public QMainWindow
{
    Q_OBJECT

public:
    explicit gui(QWidget *parent = 0);
    ~gui();

private slots:
    void on_buttonNewGame_clicked();

    void on_buttonBack_clicked();

    void on_buttonLoadGame_clicked();

    void on_comboBoxPlayer2_currentIndexChanged(int index);

    void on_pushButton_12_clicked();

    void moveToPosition(int x, int y);

    void on_buttonNewGame2_clicked();

    void on_buttonLoad2_clicked();

    void on_buttonUndo_clicked();

    void on_buttonRedo_clicked();

    void on_buttonSave_clicked();

    void on_buttonBackToGame_clicked();

    void on_buttonChangeGame_clicked();

    void on_buttonDemoGame_clicked();

    void on_buttonPass_clicked();

private:
    Ui::gui *ui;
    std::shared_ptr<GameManager> manager;
    void widgetCreateNewGame();
    void widgetLoadNewGame();

    void setComboBoxNewGameData();
    void initComboBoxNewGameData();
    void resizeToGame();
    void updateGameData();
    void status(QString text, bool isOk);
    void createComboBoxString(QString deskSize);
    void initNewGame();
    void setGameTitle();
     Game *game;

};

#endif // GUI_H
