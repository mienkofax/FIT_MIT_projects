
/* ******************************* c203.c *********************************** */
/*  Předmět: Algoritmy (IAL) - FIT VUT v Brně                                 */
/*  Úkol: c203 - Fronta znaků v poli                                          */
/*  Referenční implementace: Petr Přikryl, 1994                               */
/*  Přepis do jazyka C: Václav Topinka, září 2005                             */
/*  Úpravy: Bohuslav Křena, říjen 2015                                        */
/* ************************************************************************** */
/*
** Implementujte frontu znaků v poli. Přesnou definici typů naleznete
** v hlavičkovém souboru c203.h (ADT fronta je reprezentována strukturou tQueue,
** která obsahuje pole 'arr' pro uloľení hodnot ve frontě a indexy f_index
** a b_index. Všechny implementované funkce musí předpokládat velikost pole
** QUEUE_SIZE, i kdyľ ve skutečnosti jsou rozměry statického pole definovány
** MAX_QUEUE. Hodnota QUEUE_SIZE slouľí k simulaci fronty v různě velkém poli
** a nastavuje se v testovacím skriptu c203-test.c před testováním
** implementovaných funkcí. Hodnota QUEUE_SIZE můľe nabývat hodnot v rozsahu
** 1 aľ MAX_QUEUE.
**
** Index f_index ukazuje vľdy na první prvek ve frontě. Index b_index
** ukazuje na první volný prvek ve frontě. Pokud je fronta prázdná, ukazují
** oba indexy na stejné místo. Po inicializaci ukazují na první prvek pole,
** obsahují tedy hodnotu 0. Z uvedených pravidel vyplývá, ľe v poli je vľdy
** minimálně jeden prvek nevyuľitý.
**
** Při libovolné operaci se ľádný z indexů (f_index i b_index) nesniľuje
** vyjma případu, kdy index přesáhne hranici QUEUE_SIZE. V tom případě
** se "posunuje" znovu na začátek pole. Za tímto účelem budete deklarovat
** pomocnou funkci NextIndex, která pro kruhový pohyb přes indexy pole
** vyuľívá operaci "modulo".
**
** Implementujte následující funkce:
**
**    nextIndex ..... pomocná funkce - viz popis výše
**    queueInit ..... inicializace fronty
**    queueEmpty .... test na prázdnost fronty
**    queueFull ..... test, zda je fronta zaplněna (vyčerpána kapacita)
**    queueFront .... přečte hodnoty prvního prvku z fronty
**    queueRemove ... odstraní první prvek fronty
**    queueGet ...... přečte a odstraní první prvek fronty
**    queueUp ....... zařazení prvku na konec fronty
**
** Své řešení účelně komentujte!
**
** Terminologická poznámka: Jazyk C nepouľívá pojem procedura.
** Proto zde pouľíváme pojem funkce i pro operace, které by byly
** v algoritmickém jazyce Pascalovského typu implemenovány jako
** procedury (v jazyce C procedurám odpovídají funkce vracející typ void).
**
**/

#include "c203.h"

void queueError (int error_code) {
/*
** Vytiskne upozornění na to, ľe došlo k chybě.
** Tato funkce bude volána z některých dále implementovaných operací.
**
** TUTO FUNKCI, PROSÍME, NEUPRAVUJTE!
*/
	static const char* QERR_STRINGS[MAX_QERR+1] = {"Unknown error","Queue error: UP","Queue error: FRONT","Queue error: REMOVE","Queue error: GET","Queue error: INIT"};
	if ( error_code <= 0 || error_code > MAX_QERR )
		error_code = 0;
	printf ( "%s\n", QERR_STRINGS[error_code] );
	err_flag = 1;
}

void queueInit (tQueue* q) {
/*
** Inicializujte frontu následujícím způsobem:
** - všechny hodnoty v poli q->arr nastavte na '*',
** - index na začátek fronty nastavte na 0,
** - index prvního volného místa nastavte také na 0.
**
** V případě, ľe funkce dostane jako parametr q == NULL, volejte funkci
** queueError(QERR_INIT).
*/
	if (q == NULL)
		queueError(QERR_INIT);
	else {
		for (int i = 0; i < QUEUE_SIZE; i ++)
			q->arr[i] = '*';

		q->f_index = 0;
		q->b_index = 0;
	}
}

int nextIndex (int index) {
/*
** Pomocná funkce, která vrací index následujícího prvku v poli.
** Funkci implementujte jako jediný prikaz vyuľívající operace '%'.
** Funkci nextIndex budete vyuľívat v dalších implementovaných funkcích.
*/
	return (index +1) % QUEUE_SIZE;
}

int queueEmpty (const tQueue* q) {
/*
** Vrací nenulovou hodnotu, pokud je frona prázdná, jinak vrací hodnotu 0.
** Funkci je vhodné implementovat jedním příkazem return.
*/
	return (q->f_index == q->b_index) ? 1 : 0;
}

int queueFull (const tQueue* q) {
/*
** Vrací nenulovou hodnotu, je-li fronta plná, jinak vrací hodnotu 0.
** Funkci je vhodné implementovat jedním příkazem return
** s vyuľitím pomocné funkce nextIndex.
*/
	return (nextIndex(q->b_index) == q->f_index) ? 1 : 0;
}

void queueFront (const tQueue* q, char* c) {
/*
** Prostřednictvím parametru c vrátí znak ze začátku fronty q.
** Pokud je fronta prázdná, ošetřete to voláním funkce queueError(QERR_FRONT).
** Volání této funkce při prázdné frontě je vľdy nutné povaľovat za nekorektní.
** Bývá to totiľ důsledek špatného návrhu algoritmu, ve kterém je fronta
** pouľita. O takové situaci se proto musí programátor-vývojář dozvědět.
** V opačném případě je ladění programů obtíľnější!
**
** Při implementaci vyuľijte dříve definované funkce queueEmpty.
*/
	if (queueEmpty(q))
		queueError(QERR_FRONT);
	else {
		*c = q->arr[q->f_index];
	}
}

void queueRemove (tQueue* q) {
/*
** Odstraní znak ze začátku fronty q. Pokud je fronta prázdná, ošetřete
** vzniklou chybu voláním funkce queueError(QERR_REMOVE).
** Hodnotu na uvolněné pozici ve frontě nijak neošetřujte (nepřepisujte).
** Při implementaci vyuľijte dříve definované funkce queueEmpty a nextIndex.
*/
	if (queueEmpty(q))
		queueError(QERR_REMOVE);
	else
		q->f_index = nextIndex(q->f_index);
}

void queueGet (tQueue* q, char* c) {
/*
** Odstraní znak ze začátku fronty a vrátí ho prostřednictvím parametru c.
** Pokud je fronta prázdná, ošetřete to voláním funkce queueError(QERR_GET).
**
** Při implementaci vyuľijte dříve definovaných funkcí queueEmpty,
** queueFront a queueRemove.
*/
	if (queueEmpty(q))
		queueError(QERR_GET);
	else {
		queueFront(q, c);
		queueRemove(q);
	}
}

void queueUp (tQueue* q, char c) {
/*
** Vloľí znak c do fronty. Pokud je fronta plná, ošetřete chybu voláním
** funkce queueError(QERR_UP). Vkládání do plné fronty se povaľuje za
** nekorektní operaci. Situace by mohla být řešena i tak, ľe by operace
** neprováděla nic, ale v případě pouľití takto definované abstrakce by se
** obtíľně odhalovaly chyby v algoritmech, které by abstrakci vyuľívaly.
**
** Při implementaci vyuľijte dříve definovaných funkcí queueFull a nextIndex.
*/
	if (queueFull(q))
		queueError(QERR_UP);
	else {
		q->arr[q->b_index] = c;
		q->b_index = nextIndex(q->b_index);
	}
}
/* Konec příkladu c203.c */
