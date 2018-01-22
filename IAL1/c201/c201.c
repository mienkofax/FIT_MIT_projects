
/* c201.c *********************************************************************}
{* Téma: Jednosměrný lineární seznam
**
**                     Návrh a referenční implementace: Petr Přikryl, říjen 1994
**                                          Úpravy: Andrea Němcová listopad 1996
**                                                   Petr Přikryl, listopad 1997
**                                Přepracované zadání: Petr Přikryl, březen 1998
**                                  Přepis do jazyka C: Martin Tuček, říjen 2004
**	                                      Úpravy: Bohuslav Křena, říjen 2015
**
** Implementujte abstraktní datový typ jednosměrný lineární seznam.
** Uľitečným obsahem prvku seznamu je celé číslo typu int.
** Seznam bude jako datová abstrakce reprezentován proměnnou typu tList.
** Definici konstant a typů naleznete v hlavičkovém souboru c201.h.
**
** Vaším úkolem je implementovat následující operace, které spolu s výše
** uvedenou datovou částí abstrakce tvoří abstraktní datový typ tList:
**
**      InitList ...... inicializace seznamu před prvním pouľitím,
**      DisposeList ... zrušení všech prvků seznamu,
**      InsertFirst ... vloľení prvku na začátek seznamu,
**      First ......... nastavení aktivity na první prvek,
**      CopyFirst ..... vrací hodnotu prvního prvku,
**      DeleteFirst ... zruší první prvek seznamu,
**      PostDelete .... ruší prvek za aktivním prvkem,
**      PostInsert .... vloľí nový prvek za aktivní prvek seznamu,
**      Copy .......... vrací hodnotu aktivního prvku,
**      Actualize ..... přepíše obsah aktivního prvku novou hodnotou,
**      Succ .......... posune aktivitu na další prvek seznamu,
**      Active ........ zjiš»uje aktivitu seznamu.
**
** Při implementaci funkcí nevolejte ľádnou z funkcí implementovaných v rámci
** tohoto příkladu, není-li u dané funkce explicitně uvedeno něco jiného.
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

#include "c201.h"

int solved;
int errflg;

void Error() {
/*
** Vytiskne upozornění na to, ľe došlo k chybě.
** Tato funkce bude volána z některých dále implementovaných operací.
**/
    printf ("*ERROR* The program has performed an illegal operation.\n");
    errflg = TRUE;                      /* globální proměnná -- příznak chyby */
}

void InitList (tList *L) {
/*
** Provede inicializaci seznamu L před jeho prvním pouľitím (tzn. ľádná
** z následujících funkcí nebude volána nad neinicializovaným seznamem).
** Tato inicializace se nikdy nebude provádět nad jiľ inicializovaným
** seznamem, a proto tuto moľnost neošetřujte. Vľdy předpokládejte,
** ľe neinicializované proměnné mají nedefinovanou hodnotu.
**/
	L->Act = NULL;
	L->First = NULL;
}

void DisposeList (tList *L) {
/*
** Zruší všechny prvky seznamu L a uvede seznam L do stavu, v jakém se nacházel
** po inicializaci. Veškerá pamě» pouľívaná prvky seznamu L bude korektně
** uvolněna voláním operace free.
***/
	for (tElemPtr item = L->First; L->First != NULL; item = L->First ) {
		L->First = item->ptr;
		free(item);
	}

	for (tElemPtr item = L->Act; L->First != NULL; item = L->Act ) {
		L->Act= item->ptr;
		free(item);
	}

	L->Act = NULL;
	L->First = NULL;
}

void InsertFirst (tList *L, int val) {
/*
** Vloľí prvek s hodnotou val na začátek seznamu L.
** V případě, ľe není dostatek paměti pro nový prvek při operaci malloc,
** volá funkci Error().
**/
	tElemPtr item = malloc(sizeof(struct tElem));

	if (item == NULL)
		Error();
	else {
		item->ptr = L->First;
		item->data = val;
		L->First = item;
	}
}

void First (tList *L) {
/*
** Nastaví aktivitu seznamu L na jeho první prvek.
** Funkci implementujte jako jediný příkaz, aniľ byste testovali,
** zda je seznam L prázdný.
**/
	L->Act = L->First;
}

void CopyFirst (tList *L, int *val) {
/*
** Prostřednictvím parametru val vrátí hodnotu prvního prvku seznamu L.
** Pokud je seznam L prázdný, volá funkci Error().
**/

	if (L->First == NULL)
		Error();
	else {
		*val = L->First->data;
	}
}

void DeleteFirst (tList *L) {
/*
** Zruší první prvek seznamu L a uvolní jím pouľívanou pamě».
** Pokud byl rušený prvek aktivní, aktivita seznamu se ztrácí.
** Pokud byl seznam L prázdný, nic se neděje.
**/
	if (L->First != NULL) {
		tElemPtr item;

		item = L->First;
		L->First = item->ptr;

		if (item == L->Act) {
			L->Act = NULL;
		}
		free(item);
	}
}

void PostDelete (tList *L) {
/*
** Zruší prvek seznamu L za aktivním prvkem a uvolní jím pouľívanou pamě».
** Pokud není seznam L aktivní nebo pokud je aktivní poslední prvek seznamu L,
** nic se neděje.
**/
	//if (L->Act != NULL && L->Act->ptr != NULL) {
	if (L->Act != NULL && L->Act->ptr != NULL) {
		tElemPtr item;

		item = L->Act->ptr;
		L->Act->ptr = item->ptr;

		free(item);
	}
}

void PostInsert (tList *L, int val) {
/*
** Vloľí prvek s hodnotou val za aktivní prvek seznamu L.
** Pokud nebyl seznam L aktivní, nic se neděje!
** V případě, ľe není dostatek paměti pro nový prvek při operaci malloc,
** zavolá funkci Error().
**/
	if(L->Act != NULL) {
		tElemPtr item = malloc(sizeof(struct tElem));

		if (item == NULL)
			Error();
		else {
			item->ptr = L->Act->ptr;
			item->data = val;
			L->Act->ptr = item;
		}
	}
}

void Copy (tList *L, int *val) {
/*
** Prostřednictvím parametru val vrátí hodnotu aktivního prvku seznamu L.
** Pokud seznam není aktivní, zavolá funkci Error().
**/
	if (L->Act == NULL)
		Error();
	else
		*val = L->Act->data;
}

void Actualize (tList *L, int val) {
/*
** Přepíše data aktivního prvku seznamu L hodnotou val.
** Pokud seznam L není aktivní, nedělá nic!
**/
	if (L->Act != NULL) {
		L->Act->data = val;
	}
}

void Succ (tList *L) {
/*
** Posune aktivitu na následující prvek seznamu L.
** Všimněte si, ľe touto operací se můľe aktivní seznam stát neaktivním.
** Pokud není předaný seznam L aktivní, nedělá funkce nic.
**/
	if (L->Act != NULL)
		L->Act = L->Act->ptr;
}

int Active (tList *L) {
/*
** Je-li seznam L aktivní, vrací nenulovou hodnotu, jinak vrací 0.
** Tuto funkci je vhodné implementovat jedním příkazem return.
**/
	return (L->Act == NULL)  ? 0 : 1;
}

/* Konec c201.c */
