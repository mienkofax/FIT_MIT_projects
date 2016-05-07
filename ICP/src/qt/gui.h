/**
 * @file			gui.h
 * @author			Klára Nečasová <xnecas24>
 * @author			Peter Tisovčík <xtisov00>
 */

#ifndef GUI_H
#define GUI_H

#include <QMainWindow>
#include "game.h"
#include "ui_gui.h"

namespace Ui {
class gui;
}

/**
 * Trieda obsahujuca hru.
 */
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
	Game *game;
	std::shared_ptr<GameManager> manager;

	/**
	 * Okno pre vytvorenie novej hry
	 */
	void widgetCreateNewGame();

	/**
	 * Okno pre nacitanie novej hry.
	 */
	void widgetLoadNewGame();

	/**
	 * Vlozenie hodnot pre vytvorenie novej hry.
	 */
	void setComboBoxNewGameData();

	/**
	 * Inicializacia hodnot pre vytvorenie novej hry na predvoelene hodnoty.
	 */
	void initComboBoxNewGameData();

	/**
	 * Zmena velkosti okna na velkost pre hru.
	 */
	void resizeToGame();

	/**
	 * Zmena velkosti okna pre hru.
	 */
	void updateGameData();

	/**
	 * Vypis spravy pre informacie o tom ci bola akcia uspesna alebo nie.
	 * @param	text		Text, ktory sa ma vypisat
	 * @param	isOk		True ak sa ma vypisat spravne vykonana
	 						akcia(zelenou) a false ak cervenou
	 */
	void status(QString text, bool isOk);

	/**
	 * Vytvorenie retazca pre pridanie do combo boxu, kvoli zmene hry.
	 * @param	deskSize	Velkot hracej dosky
	 */
	void createComboBoxString(QString deskSize);

	/**
	 * Nastvenie velkosti okna a vlozenie kamenov pre score a aktualneho hraca.
	 */
	void initNewGame();

	/**
	 * Nastavenie nazvu okna
	 */
	void setGameTitle();

};

#endif // GUI_H
