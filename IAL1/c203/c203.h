
/* ******************************* c203.h *********************************** */
/*  Předmět: Algoritmy (IAL) - FIT VUT v Brně                                 */
/*  Hlavičkový soubor pro c203.c (Fronta znaků v poli)                        */
/*  Úkol: c203 - Fronta znaků v poli                                          */
/*  Vytvořil: Václav Topinka, září 2005                                       */
/*  Úpravy: Bohuslav Křena, říjen 2009                                        */
/* ************************************************************************** */

/* TENTO SOUBOR, PROSÍME, NEUPRAVUJTE! */

#ifndef _QUEUE_H_
#define _QUEUE_H_

#include <stdio.h>

#define MAX_QUEUE 50
extern int QUEUE_SIZE;
/*
 * Hodnota MAX_QUEUE udvává skutečnou velikost statického pole pro uloľení
 * hodnot fronty. Při implementaci operací nad ADT fronta však předpokládejte,
 * ľe velikost tohoto pole je pouze QUEUE_SIZE. Umoľní nám to jednoduše
 * měnit velikost fronty v průběhu testování. Při implementaci ADT fronta byste
 * tedy hodnotu MAX_QUEUE neměli vůbec pouľít. Pamatujte, ľe do fronty se vejde
 * maximálně (QUEUE_SIZE - 1) prvků. Jedna pozice ve frontě bude vľdy nevyuľitá,
 * aby bylo moľné odlišit prázdnou frontu od plné.
 */

extern int solved;                      /* Indikuje, zda byla operace řešena. */
extern int err_flag;                   /* Indikuje, zda operace volala chybu. */

                                        /* Chybové kódy pro funkci queueError */
#define MAX_QERR    5                                   /* počet moľných chyb */
#define QERR_UP     1                                   /* chyba při stackTop */
#define QERR_FRONT  2                                   /* chyba při stackPop */
#define QERR_REMOVE 3                                  /* chyba při stackPush */
#define QERR_GET    4                                  /* chyba při stackPush */
#define QERR_INIT   5                                     /* chyba při malloc */

                               /* ADT fronta implementovaný ve statickém poli */
typedef struct {
	char arr[MAX_QUEUE];                           /* pole pro uloľení hodnot */
	int f_index;                                       /* index prvního prvku */
	int b_index;                                  /* index první volné pozice */
} tQueue;
                                      /* Hlavičky funkcí pro práci s frontou. */
void queueError ( int error_code );
void queueInit ( tQueue* q );
int queueEmpty ( const tQueue* q );
int queueFull ( const tQueue* q );
void queueFront ( const tQueue* q, char* c);
void queueRemove ( tQueue* q );
void queueGet ( tQueue* q, char* c );
void queueUp ( tQueue* q, char c );

#endif

/* Konec hlavičkového souboru c203.h */
