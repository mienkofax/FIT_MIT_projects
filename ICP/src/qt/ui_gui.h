/********************************************************************************
** Form generated from reading UI file 'gui.ui'
**
** Created by: Qt User Interface Compiler version 5.5.1
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_GUI_H
#define UI_GUI_H

#include <QtCore/QVariant>
#include <QtWidgets/QAction>
#include <QtWidgets/QApplication>
#include <QtWidgets/QButtonGroup>
#include <QtWidgets/QComboBox>
#include <QtWidgets/QGraphicsView>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QLabel>
#include <QtWidgets/QMainWindow>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QStackedWidget>
#include <QtWidgets/QToolButton>
#include <QtWidgets/QWidget>

QT_BEGIN_NAMESPACE

class Ui_gui
{
public:
    QWidget *centralWidget;
    QStackedWidget *stackedWidget;
    QWidget *page;
    QPushButton *buttonDemoGame;
    QPushButton *buttonNewGame;
    QPushButton *buttonLoadGame;
    QWidget *page_2;
    QComboBox *comboBoxDeskSize;
    QComboBox *comboBoxPlayer1;
    QPushButton *pushButton_12;
    QComboBox *comboBoxAlgorithm;
    QPushButton *buttonBack;
    QLabel *label_2;
    QLabel *label_3;
    QComboBox *comboBoxPlayer2;
    QLabel *label_4;
    QLabel *label_5;
    QPushButton *buttonBackToGame;
    QWidget *page_3;
    QGraphicsView *graphicsView;
    QPushButton *buttonNewGame2;
    QLabel *labelMove;
    QPushButton *buttonLoad2;
    QPushButton *buttonSave;
    QPushButton *buttonChangeGame;
    QLabel *labelPlayer1Score;
    QLabel *labelPlayer2Score;
    QPushButton *buttonPass;
    QComboBox *comboBoxGame;
    QToolButton *buttonRedo;
    QToolButton *buttonUndo;
    QLabel *labelStatus;
    QLabel *labelMove_2;

    void setupUi(QMainWindow *gui)
    {
        if (gui->objectName().isEmpty())
            gui->setObjectName(QStringLiteral("gui"));
        gui->resize(854, 622);
        centralWidget = new QWidget(gui);
        centralWidget->setObjectName(QStringLiteral("centralWidget"));
        stackedWidget = new QStackedWidget(centralWidget);
        stackedWidget->setObjectName(QStringLiteral("stackedWidget"));
        stackedWidget->setGeometry(QRect(10, 10, 650, 660));
        page = new QWidget();
        page->setObjectName(QStringLiteral("page"));
        buttonDemoGame = new QPushButton(page);
        buttonDemoGame->setObjectName(QStringLiteral("buttonDemoGame"));
        buttonDemoGame->setGeometry(QRect(90, 180, 121, 30));
        buttonNewGame = new QPushButton(page);
        buttonNewGame->setObjectName(QStringLiteral("buttonNewGame"));
        buttonNewGame->setGeometry(QRect(90, 120, 121, 30));
        buttonNewGame->setFlat(false);
        buttonLoadGame = new QPushButton(page);
        buttonLoadGame->setObjectName(QStringLiteral("buttonLoadGame"));
        buttonLoadGame->setGeometry(QRect(90, 80, 121, 30));
        stackedWidget->addWidget(page);
        page_2 = new QWidget();
        page_2->setObjectName(QStringLiteral("page_2"));
        comboBoxDeskSize = new QComboBox(page_2);
        comboBoxDeskSize->setObjectName(QStringLiteral("comboBoxDeskSize"));
        comboBoxDeskSize->setGeometry(QRect(80, 30, 131, 28));
        comboBoxPlayer1 = new QComboBox(page_2);
        comboBoxPlayer1->setObjectName(QStringLiteral("comboBoxPlayer1"));
        comboBoxPlayer1->setGeometry(QRect(80, 90, 131, 28));
        pushButton_12 = new QPushButton(page_2);
        pushButton_12->setObjectName(QStringLiteral("pushButton_12"));
        pushButton_12->setGeometry(QRect(160, 300, 111, 30));
        comboBoxAlgorithm = new QComboBox(page_2);
        comboBoxAlgorithm->setObjectName(QStringLiteral("comboBoxAlgorithm"));
        comboBoxAlgorithm->setGeometry(QRect(80, 210, 131, 28));
        buttonBack = new QPushButton(page_2);
        buttonBack->setObjectName(QStringLiteral("buttonBack"));
        buttonBack->setGeometry(QRect(20, 300, 111, 30));
        label_2 = new QLabel(page_2);
        label_2->setObjectName(QStringLiteral("label_2"));
        label_2->setGeometry(QRect(80, 10, 151, 20));
        label_3 = new QLabel(page_2);
        label_3->setObjectName(QStringLiteral("label_3"));
        label_3->setGeometry(QRect(120, 130, 66, 20));
        comboBoxPlayer2 = new QComboBox(page_2);
        comboBoxPlayer2->setObjectName(QStringLiteral("comboBoxPlayer2"));
        comboBoxPlayer2->setGeometry(QRect(80, 150, 131, 28));
        label_4 = new QLabel(page_2);
        label_4->setObjectName(QStringLiteral("label_4"));
        label_4->setEnabled(true);
        label_4->setGeometry(QRect(110, 190, 81, 20));
        label_5 = new QLabel(page_2);
        label_5->setObjectName(QStringLiteral("label_5"));
        label_5->setGeometry(QRect(120, 70, 66, 20));
        buttonBackToGame = new QPushButton(page_2);
        buttonBackToGame->setObjectName(QStringLiteral("buttonBackToGame"));
        buttonBackToGame->setGeometry(QRect(20, 260, 111, 30));
        stackedWidget->addWidget(page_2);
        page_3 = new QWidget();
        page_3->setObjectName(QStringLiteral("page_3"));
        graphicsView = new QGraphicsView(page_3);
        graphicsView->setObjectName(QStringLiteral("graphicsView"));
        graphicsView->setGeometry(QRect(0, 50, 640, 550));
        graphicsView->setAlignment(Qt::AlignLeading|Qt::AlignLeft|Qt::AlignTop);
        buttonNewGame2 = new QPushButton(page_3);
        buttonNewGame2->setObjectName(QStringLiteral("buttonNewGame2"));
        buttonNewGame2->setGeometry(QRect(20, 10, 111, 30));
        labelMove = new QLabel(page_3);
        labelMove->setObjectName(QStringLiteral("labelMove"));
        labelMove->setGeometry(QRect(500, 160, 121, 20));
        QFont font;
        font.setPointSize(16);
        font.setBold(true);
        font.setWeight(75);
        labelMove->setFont(font);
        labelMove->setAlignment(Qt::AlignCenter);
        buttonLoad2 = new QPushButton(page_3);
        buttonLoad2->setObjectName(QStringLiteral("buttonLoad2"));
        buttonLoad2->setGeometry(QRect(550, 10, 81, 30));
        buttonSave = new QPushButton(page_3);
        buttonSave->setObjectName(QStringLiteral("buttonSave"));
        buttonSave->setGeometry(QRect(460, 10, 81, 30));
        buttonChangeGame = new QPushButton(page_3);
        buttonChangeGame->setObjectName(QStringLiteral("buttonChangeGame"));
        buttonChangeGame->setGeometry(QRect(500, 480, 121, 30));
        labelPlayer1Score = new QLabel(page_3);
        labelPlayer1Score->setObjectName(QStringLiteral("labelPlayer1Score"));
        labelPlayer1Score->setGeometry(QRect(560, 310, 31, 20));
        labelPlayer1Score->setFont(font);
        labelPlayer2Score = new QLabel(page_3);
        labelPlayer2Score->setObjectName(QStringLiteral("labelPlayer2Score"));
        labelPlayer2Score->setGeometry(QRect(560, 370, 31, 20));
        labelPlayer2Score->setFont(font);
        buttonPass = new QPushButton(page_3);
        buttonPass->setObjectName(QStringLiteral("buttonPass"));
        buttonPass->setGeometry(QRect(500, 110, 121, 30));
        comboBoxGame = new QComboBox(page_3);
        comboBoxGame->setObjectName(QStringLiteral("comboBoxGame"));
        comboBoxGame->setGeometry(QRect(500, 440, 121, 28));
        buttonRedo = new QToolButton(page_3);
        buttonRedo->setObjectName(QStringLiteral("buttonRedo"));
        buttonRedo->setGeometry(QRect(565, 70, 55, 28));
        buttonRedo->setArrowType(Qt::RightArrow);
        buttonUndo = new QToolButton(page_3);
        buttonUndo->setObjectName(QStringLiteral("buttonUndo"));
        buttonUndo->setGeometry(QRect(500, 70, 55, 28));
        QFont font1;
        font1.setPointSize(15);
        font1.setBold(false);
        font1.setWeight(50);
        buttonUndo->setFont(font1);
        buttonUndo->setIconSize(QSize(32, 32));
        buttonUndo->setCheckable(false);
        buttonUndo->setAutoRaise(false);
        buttonUndo->setArrowType(Qt::LeftArrow);
        labelStatus = new QLabel(page_3);
        labelStatus->setObjectName(QStringLiteral("labelStatus"));
        labelStatus->setGeometry(QRect(140, 9, 311, 31));
        QFont font2;
        font2.setPointSize(12);
        labelStatus->setFont(font2);
        labelStatus->setAlignment(Qt::AlignCenter);
        labelMove_2 = new QLabel(page_3);
        labelMove_2->setObjectName(QStringLiteral("labelMove_2"));
        labelMove_2->setGeometry(QRect(500, 260, 121, 20));
        labelMove_2->setFont(font);
        labelMove_2->setAlignment(Qt::AlignCenter);
        stackedWidget->addWidget(page_3);
        gui->setCentralWidget(centralWidget);

        retranslateUi(gui);

        stackedWidget->setCurrentIndex(2);
        buttonNewGame->setDefault(false);


        QMetaObject::connectSlotsByName(gui);
    } // setupUi

    void retranslateUi(QMainWindow *gui)
    {
        gui->setWindowTitle(QApplication::translate("gui", "Othello Game", 0));
        buttonDemoGame->setText(QApplication::translate("gui", "Help", 0));
        buttonNewGame->setText(QApplication::translate("gui", "New Game", 0));
        buttonLoadGame->setText(QApplication::translate("gui", "Load Game", 0));
        pushButton_12->setText(QApplication::translate("gui", "Create game", 0));
        buttonBack->setText(QApplication::translate("gui", "Back to menu", 0));
        label_2->setText(QApplication::translate("gui", "Velkost hracej dosky", 0));
        label_3->setText(QApplication::translate("gui", "Hrac 2:", 0));
        label_4->setText(QApplication::translate("gui", "Algoritmus", 0));
        label_5->setText(QApplication::translate("gui", "Hrac 1:", 0));
        buttonBackToGame->setText(QApplication::translate("gui", "Back to game", 0));
        buttonNewGame2->setText(QApplication::translate("gui", "New Game", 0));
        labelMove->setText(QApplication::translate("gui", "Na \305\245ahu:", 0));
        buttonLoad2->setText(QApplication::translate("gui", "Load", 0));
        buttonSave->setText(QApplication::translate("gui", "Save", 0));
        buttonChangeGame->setText(QApplication::translate("gui", "Change Game", 0));
        labelPlayer1Score->setText(QApplication::translate("gui", "2", 0));
        labelPlayer2Score->setText(QApplication::translate("gui", "2", 0));
        buttonPass->setText(QApplication::translate("gui", "Pass", 0));
        buttonRedo->setText(QApplication::translate("gui", "...", 0));
        buttonUndo->setText(QApplication::translate("gui", "...", 0));
        buttonUndo->setShortcut(QString());
        labelStatus->setText(QApplication::translate("gui", "...", 0));
        labelMove_2->setText(QApplication::translate("gui", "Score:", 0));
    } // retranslateUi

};

namespace Ui {
    class gui: public Ui_gui {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_GUI_H
