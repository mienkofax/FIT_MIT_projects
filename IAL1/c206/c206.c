/* c206.c **********************************************************}
{* Téma: Dvousměrně vázaný lineární seznam
**
**                   Návrh a referenční implementace: Bohuslav Křena, říjen 2001
**                            Přepracované do jazyka C: Martin Tuček, říjen 2004
**                                            Úpravy: Bohuslav Křena, říjen 2015
**
** Implementujte abstraktní datový typ dvousměrně vázaný lineární seznam.
** Uľitečným obsahem prvku seznamu je hodnota typu int.
** Seznam bude jako datová abstrakce reprezentován proměnnou
** typu tDLList (DL znamená Double-Linked a slouľí pro odlišení
** jmen konstant, typů a funkcí od jmen u jednosměrně vázaného lineárního
** seznamu). Definici konstant a typů naleznete v hlavičkovém souboru c206.h.
**
** Vaším úkolem je implementovat následující operace, které spolu
** s výše uvedenou datovou částí abstrakce tvoří abstraktní datový typ
** obousměrně vázaný lineární seznam:
**
**      DLInitList ...... inicializace seznamu před prvním pouľitím,
**      DLDisposeList ... zrušení všech prvků seznamu,
**      DLInsertFirst ... vloľení prvku na začátek seznamu,
**      DLInsertLast .... vloľení prvku na konec seznamu,
**      DLFirst ......... nastavení aktivity na první prvek,
**      DLLast .......... nastavení aktivity na poslední prvek,
**      DLCopyFirst ..... vrací hodnotu prvního prvku,
**      DLCopyLast ...... vrací hodnotu posledního prvku,
**      DLDeleteFirst ... zruší první prvek seznamu,
**      DLDeleteLast .... zruší poslední prvek seznamu,
**      DLPostDelete .... ruší prvek za aktivním prvkem,
**      DLPreDelete ..... ruší prvek před aktivním prvkem,
**      DLPostInsert .... vloľí nový prvek za aktivní prvek seznamu,
**      DLPreInsert ..... vloľí nový prvek před aktivní prvek seznamu,
**      DLCopy .......... vrací hodnotu aktivního prvku,
**      DLActualize ..... přepíše obsah aktivního prvku novou hodnotou,
**      DLSucc .......... posune aktivitu na další prvek seznamu,
**      DLPred .......... posune aktivitu na předchozí prvek seznamu,
**      DLActive ........ zjiš»uje aktivitu seznamu.
**
** Při implementaci jednotlivých funkcí nevolejte ľádnou z funkcí
** implementovaných v rámci tohoto příkladu, není-li u funkce
** explicitně uvedeno něco jiného.
**
** Nemusíte ošetřovat situaci, kdy místo legálního ukazatele na seznam
** předá někdo jako parametr hodnotu NULL.
**
** Svou implementaci vhodně komentujte!
**
** Terminologická poznámka: Jazyk C nepouľívá pojem procedura.
** Proto zde pouľíváme pojem funkce i pro operace, které by byly
** v algoritmickém jazyce Pascalovského typu implemenovány jako
** procedury (v jazyce C procedurám odpovídají funkce vracející typ void).
**/

#include "c206.h"

int solved;
int errflg;

void DLError() {
/*
** Vytiskne upozornění na to, ľe došlo k chybě.
** Tato funkce bude volána z některých dále implementovaných operací.
**/
    printf ("*ERROR* The program has performed an illegal operation.\n");
    errflg = TRUE;             /* globální proměnná -- příznak ošetření chyby */
    return;
}

void DLInitList (tDLList *L) {
/*
** Provede inicializaci seznamu L před jeho prvním pouľitím (tzn. ľádná
** z následujících funkcí nebude volána nad neinicializovaným seznamem).
** Tato inicializace se nikdy nebude provádět nad jiľ inicializovaným
** seznamem, a proto tuto moľnost neošetřujte. Vľdy předpokládejte,
** ľe neinicializované proměnné mají nedefinovanou hodnotu.
**/
	L->First = NULL;
	L->Act = NULL;
	L->Last = NULL;
}

void DLDisposeList (tDLList *L) {
/*
** Zruší všechny prvky seznamu L a uvede seznam do stavu, v jakém
** se nacházel po inicializaci. Rušené prvky seznamu budou korektně
** uvolněny voláním operace free.
**/
	for (tDLElemPtr item = L->First; L->First != NULL; item = L->First ) {
		L->First = item->rptr;

		free(item);
	}

	L->First = NULL;
	L->Act = NULL;
	L->Last = NULL;
}

void DLInsertFirst (tDLList *L, int val) {
/*
** Vloľí nový prvek na začátek seznamu L.
** V případě, ľe není dostatek paměti pro nový prvek při operaci malloc,
** volá funkci DLError().
**/
	tDLElemPtr item = malloc(sizeof(struct tDLElem));
	if (item == NULL)
		DLError();
	else {
		item->data = val;

		if (L->First == NULL) {
			item->rptr = NULL;
			item->lptr = NULL;
			L->Last = item;
		} else {
			item->lptr = NULL;
			item->rptr = L->First;
			L->First->lptr = item;
		}

		L->First = item;
	}

}

void DLInsertLast(tDLList *L, int val) {
/*
** Vloľí nový prvek na konec seznamu L (symetrická operace k DLInsertFirst).
** V případě, ľe není dostatek paměti pro nový prvek při operaci malloc,
** volá funkci DLError().
**/
	tDLElemPtr item = malloc(sizeof(struct tDLElem));

	if (item == NULL)
		DLError();
	else {
		item->data = val;

		if (L->Last == NULL) {
			item->rptr = NULL;
			item->lptr = NULL;
			L->First = item;
		} else {
			item->rptr = NULL;
			item->lptr = L->Last;
			L->Last->rptr = item;
		}

		L->Last= item;
	}
}

void DLFirst (tDLList *L) {
/*
** Nastaví aktivitu na první prvek seznamu L.
** Funkci implementujte jako jediný příkaz (nepočítáme-li return),
** aniľ byste testovali, zda je seznam L prázdný.
**/
	L->Act = L->First;
}

void DLLast (tDLList *L) {
/*
** Nastaví aktivitu na poslední prvek seznamu L.
** Funkci implementujte jako jediný příkaz (nepočítáme-li return),
** aniľ byste testovali, zda je seznam L prázdný.
**/
	L->Act = L->Last;
}

void DLCopyFirst (tDLList *L, int *val) {
/*
** Prostřednictvím parametru val vrátí hodnotu prvního prvku seznamu L.
** Pokud je seznam L prázdný, volá funkci DLError().
**/
	if (L->First == NULL)
		DLError();
	else {
		*val = L->First->data;
	}
}

void DLCopyLast (tDLList *L, int *val) {
/*
** Prostřednictvím parametru val vrátí hodnotu posledního prvku seznamu L.
** Pokud je seznam L prázdný, volá funkci DLError().
**/
	if (L->Last == NULL)
		DLError();
	else {
		*val = L->Last->data;
	}
}

void DLDeleteFirst (tDLList *L) {
/*
** Zruší první prvek seznamu L. Pokud byl první prvek aktivní, aktivita
** se ztrácí. Pokud byl seznam L prázdný, nic se neděje.
**/
	if (L->First != NULL) {
		tDLElemPtr item = L->First;

		if (L->First->rptr == NULL) {
			L->Last = NULL;
			L->First = NULL;
			L->Act = NULL;
		} else {
			L->First = item->rptr;
			L->First->lptr = NULL;

			if (L->Act == item)
				L->Act = NULL;
		}

		free(item);
	}
}

void DLDeleteLast (tDLList *L) {
/*
** Zruší poslední prvek seznamu L. Pokud byl poslední prvek aktivní,
** aktivita seznamu se ztrácí. Pokud byl seznam L prázdný, nic se neděje.
**/
	if (L->Last != NULL) {
		tDLElemPtr item = L->Last;

		if (L->Last->lptr == NULL) {
			L->Last = NULL;
			L->First = NULL;
			L->Act = NULL;
		} else {
			L->Last = item->lptr;
			L->Last->rptr = NULL;

			if (L->Act == item)
				L->Act = NULL;

		}

		free(item);
	}
}

void DLPostDelete (tDLList *L) {
/*
** Zruší prvek seznamu L za aktivním prvkem.
** Pokud je seznam L neaktivní nebo pokud je aktivní prvek
** posledním prvkem seznamu, nic se neděje.
**/
	if (L->Act != NULL && L->Act !=L->Last) {

		tDLElemPtr item = L->Act->rptr;
		if (item->rptr == NULL) {
			item->lptr->rptr = NULL;
			L->Last = L->Act;
		} else {
			item->lptr->rptr = item->rptr;
			item->rptr->lptr = item->lptr;
		}

		free(item);
	}
}

void DLPreDelete (tDLList *L) {
/*
** Zruší prvek před aktivním prvkem seznamu L .
** Pokud je seznam L neaktivní nebo pokud je aktivní prvek
** prvním prvkem seznamu, nic se neděje.
**/
	if (L->Act != NULL && L->Act !=L->First) {

		tDLElemPtr item = L->Act->lptr;
		if (item->lptr == NULL) {
			item->rptr->lptr = NULL;
			L->First = L->Act;
		} else {
			item->lptr->rptr = item->rptr;
			item->rptr->lptr = item->lptr;
		}
		free(item);
	}
}

void DLPostInsert (tDLList *L, int val) {
/*
** Vloľí prvek za aktivní prvek seznamu L.
** Pokud nebyl seznam L aktivní, nic se neděje.
** V případě, ľe není dostatek paměti pro nový prvek při operaci malloc,
** volá funkci DLError().
**/
	if (L->Act != NULL) {
		tDLElemPtr item = malloc(sizeof(struct tDLElem));

		if (item == NULL)
			DLError();
		else {
			item->data = val;

			if (L->Act->rptr == NULL) {
				item->lptr = L->Act;
				item->rptr = NULL;
				L->Act->rptr = item;
				L->Last = item;

			} else {
				item->lptr = L->Act;
				item->rptr = L->Act->rptr;
				L->Act->rptr = item;
				item->rptr->lptr = item;
			}


		}
	}
}

void DLPreInsert (tDLList *L, int val) {
/*
** Vloľí prvek před aktivní prvek seznamu L.
** Pokud nebyl seznam L aktivní, nic se neděje.
** V případě, ľe není dostatek paměti pro nový prvek při operaci malloc,
** volá funkci DLError().
**/
	if (L->Act != NULL) {
		tDLElemPtr item = malloc(sizeof(struct tDLElem));

		if (item == NULL)
			DLError();
		else {
			item->data = val;

			if (L->Act->lptr == NULL) {
				item->rptr = L->Act;
				item->lptr = NULL;
				L->Act->lptr = item;
				L->First = item;
			} else {
				item->rptr = L->Act;
				item->lptr = L->Act->lptr;
				L->Act->lptr = item;
				item->lptr->rptr = item;
			}
		}
	}
}

void DLCopy (tDLList *L, int *val) {
/*
** Prostřednictvím parametru val vrátí hodnotu aktivního prvku seznamu L.
** Pokud seznam L není aktivní, volá funkci DLError ().
**/
	if (L->Act == NULL)
		DLError();
	else {
		*val = L->Act->data;
	}
}

void DLActualize (tDLList *L, int val) {
/*
** Přepíše obsah aktivního prvku seznamu L.
** Pokud seznam L není aktivní, nedělá nic.
**/
	if (L->Act != NULL)
		L->Act->data = val;
}

void DLSucc (tDLList *L) {
/*
** Posune aktivitu na následující prvek seznamu L.
** Není-li seznam aktivní, nedělá nic.
** Všimněte si, ľe při aktivitě na posledním prvku se seznam stane neaktivním.
**/
	if (L->Act != NULL)
		L->Act = L->Act->rptr;
}


void DLPred (tDLList *L) {
/*
** Posune aktivitu na předchozí prvek seznamu L.
** Není-li seznam aktivní, nedělá nic.
** Všimněte si, ľe při aktivitě na prvním prvku se seznam stane neaktivním.
**/
	if (L->Act != NULL)
		L->Act = L->Act->lptr;
}

int DLActive (tDLList *L) {
/*
** Je-li seznam L aktivní, vrací nenulovou hodnotu, jinak vrací 0.
** Funkci je vhodné implementovat jedním příkazem return.
**/
	return (L->Act == NULL)  ? 0 : 1;
}

/* Konec c206.c*/
