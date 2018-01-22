
/* Předmět: Algoritmy (IAL) - FIT VUT v Brně
 * Hlavičkový soubor pro c201.c (Jednosměrně vázaný lineární seznam)
 * Vytvořil: Martin Tuček, září 2005
 * Upravil: Bohuslav Křena, říjen 2009
 *
 *
 * Tento soubor, prosíme, neupravujte!
 * Plese, do not change this file!
 */

#include<stdio.h>
#include<stdlib.h>
#include<stdarg.h>

#define TRUE 1
#define FALSE 0
                                           /* Indikace, zda byl příklad řešen */
                                   /* Detects whether the task is implemented */
extern int solved;
                                                            /* Indikace chyby */
                                                                /* Error flag */
extern int errflg;
                                                             /* Prvek seznamu */
                                                /* Definition of list element */
typedef struct tElem {
    struct tElem *ptr;
    int data;
} *tElemPtr;
                                                            /* Vlastní seznam */
                                                        /* Definition of list */
typedef struct {
    tElemPtr Act;
    tElemPtr First;
} tList;
                                                   /* Funkce pro implementaci */
                                             /* Funkctionst to be implemented */
void InitList (tList *);
void DisposeList (tList *);
void InsertFirst (tList *, int);
void First (tList *);
void CopyFirst (tList *, int *);
void DeleteFirst (tList *);
void PostDelete (tList *);
void PostInsert (tList *, int);
void Succ (tList *);
void Copy (tList *, int *);
void Actualize (tList *, int );
int  Active (tList *);
