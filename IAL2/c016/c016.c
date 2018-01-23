
/* c016.c: **********************************************************}
{* Téma:  Tabulka s Rozptýlenými Položkami
**                      První implementace: Petr Přikryl, prosinec 1994
**                      Do jazyka C prepsal a upravil: Vaclav Topinka, 2005
**                      Úpravy: Karel Masařík, říjen 2014
**                      Úpravy: Radek Hranický, říjen 2014
**                      Úpravy: Radek Hranický, listopad 2015
**
** Vytvořete abstraktní datový typ
** TRP (Tabulka s Rozptýlenými Položkami = Hash table)
** s explicitně řetězenými synonymy. Tabulka je implementována polem
** lineárních seznamů synonym.
**
** Implementujte následující procedury a funkce.
**
**  HTInit ....... inicializuje tabulku před prvním použitím
**  HTInsert ..... vložení prvku
**  HTSearch ..... zjištění přítomnosti prvku v tabulce
**  HTDelete ..... zrušení prvku
**  HTRead ....... přečtení hodnoty prvku
**  HTClearAll ... zrušení obsahu celé tabulky (inicializace tabulky
**                 poté, co již byla použita)
**
** Definici typů naleznete v souboru c016.h.
**
** Tabulka je reprezentována datovou strukturou typu tHTable,
** která se skládá z ukazatelů na položky, jež obsahují složky
** klíče 'key', obsahu 'data' (pro jednoduchost typu float), a
** ukazatele na další synonymum 'ptrnext'. Při implementaci funkcí
** uvažujte maximální rozměr pole HTSIZE.
**
** U všech procedur využívejte rozptylovou funkci hashCode.  Povšimněte si
** způsobu předávání parametrů a zamyslete se nad tím, zda je možné parametry
** předávat jiným způsobem (hodnotou/odkazem) a v případě, že jsou obě
** možnosti funkčně přípustné, jaké jsou výhody či nevýhody toho či onoho
** způsobu.
**
** V příkladech jsou použity položky, kde klíčem je řetězec, ke kterému
** je přidán obsah - reálné číslo.
*/

#include "c016.h"

int HTSIZE = MAX_HTSIZE;
int solved;

/*          -------
** Rozptylovací funkce - jejím úkolem je zpracovat zadaný klíč a přidělit
** mu index v rozmezí 0..HTSize-1.  V ideálním případě by mělo dojít
** k rovnoměrnému rozptýlení těchto klíčů po celé tabulce.  V rámci
** pokusů se můžete zamyslet nad kvalitou této funkce.  (Funkce nebyla
** volena s ohledem na maximální kvalitu výsledku). }
*/

int hashCode ( tKey key ) {
	int retval = 1;
	int keylen = strlen(key);
	for ( int i=0; i<keylen; i++ )
		retval += key[i];
	return ( retval % HTSIZE );
}

/*
** Inicializace tabulky s explicitně zřetězenými synonymy.  Tato procedura
** se volá pouze před prvním použitím tabulky.
*/

void htInit ( tHTable* ptrht ) {
	
	//ak uz tabulka bola inicializovana tak ukoncime volanie funkcie
	if (ptrht == NULL)
		return;
	
	//inicializacia poloziek v poli
	for (int k = 0; k < HTSIZE; k++)
		(*ptrht)[k] = NULL;
		
	return;
}

/* TRP s explicitně zřetězenými synonymy.
** Vyhledání prvku v TRP ptrht podle zadaného klíče key.  Pokud je
** daný prvek nalezen, vrací se ukazatel na daný prvek. Pokud prvek nalezen není, 
** vrací se hodnota NULL.
**
*/

tHTItem* htSearch ( tHTable* ptrht, tKey key ) {
	//ak je prazdna tabulka, ukoncime volanie
	if (ptrht == NULL)
		return NULL;
	
	int k = hashCode(key); //zistime index, kde sa ma hladat
	tHTItem *item = (*ptrht)[k];
	
	//prechod tabulkou a zistenie ci sme nasli kluc
	//alebo ci nie sme na konci tabulky 
	while ((item != NULL) && (item->key != key))
		item = item->ptrnext; //nacitanie dalsej polozky
		
	//vratenie vysledku alebo NULL
	return item;
}

/* 
** TRP s explicitně zřetězenými synonymy.
** Tato procedura vkládá do tabulky ptrht položku s klíčem key a s daty
** data.  Protože jde o vyhledávací tabulku, nemůže být prvek se stejným
** klíčem uložen v tabulce více než jedenkrát.  Pokud se vkládá prvek,
** jehož klíč se již v tabulce nachází, aktualizujte jeho datovou část.
**
** Využijte dříve vytvořenou funkci htSearch.  Při vkládání nového
** prvku do seznamu synonym použijte co nejefektivnější způsob,
** tedy proveďte.vložení prvku na začátek seznamu.
**/

void htInsert ( tHTable* ptrht, tKey key, tData data ) {
	//ukoncenie volanie funkcie v pripade, ze je tabulka prazdna 
	if (ptrht == NULL)
		return;
	
	tHTItem *search_item = htSearch(ptrht, key); //hladame polozku s danym klucom
	tHTItem *new_item = NULL;
	
	//ak sa kluc nenasiel v tabulke 
	if (search_item == NULL) {
		new_item = malloc(sizeof(tHTItem));
		
		if (new_item == NULL)
			return;
		
		new_item->data = data; //pridame nove data
		new_item->key = key; //pridame novy kluc
		new_item->ptrnext = (*ptrht)[hashCode(key)]; //namapovanie
		(*ptrht)[hashCode(key)] = new_item;
	} else 
		search_item->data = data; //aktualizujeme data na zadanom kluci
		
	return;
}

/*
** TRP s explicitně zřetězenými synonymy.
** Tato funkce zjišťuje hodnotu datové části položky zadané klíčem.
** Pokud je položka nalezena, vrací funkce ukazatel na položku
** Pokud položka nalezena nebyla, vrací se funkční hodnota NULL
**
** Využijte dříve vytvořenou funkci HTSearch.
*/

tData* htRead ( tHTable* ptrht, tKey key ) {
	
	if (ptrht == NULL)
		return NULL;
		
	tHTItem *search_item = htSearch(ptrht, key); //hladame polozku s danym klucom
	
	//ak sme nenasli polozku vrati NULL inak ukazatel na tuto polozku
	if (search_item == NULL)
		return NULL;
	else 
		return &(search_item->data);
}

/*
** TRP s explicitně zřetězenými synonymy.
** Tato procedura vyjme položku s klíčem key z tabulky
** ptrht.  Uvolněnou položku korektně zrušte.  Pokud položka s uvedeným
** klíčem neexistuje, dělejte, jako kdyby se nic nestalo (tj. nedělejte
** nic).
**
** V tomto případě NEVYUŽÍVEJTE dříve vytvořenou funkci HTSearch.
*/

void htDelete ( tHTable* ptrht, tKey key ) {
	if (ptrht == NULL)
		return;
	
	int k = hashCode(key);
	tHTItem *del_item = (*ptrht)[k];
	tHTItem *tmp_item = (*ptrht)[k];
	
	if (del_item == NULL)
		return;
	
	//ak sa kluce rovnaju
	if (del_item->key == key) {
		(*ptrht)[k] = del_item->ptrnext;
		free(del_item);
	} else { //ak sa kluce nerovnaju

		//prechod tabulkou a zistenie ci sme nasli kluc
		//alebo ci nie sme na konci tabulky
		while ((del_item != NULL) && (del_item->key != key)) {
			tmp_item = del_item;
			del_item = del_item->ptrnext;
		}
		
		//ak najdeme kluc uvolnime ho
		if (del_item != NULL) {
			tmp_item->ptrnext = del_item->ptrnext;
			free(del_item);
		}
	}
	return;
}

/* TRP s explicitně zřetězenými synonymy.
** Tato procedura zruší všechny položky tabulky, korektně uvolní prostor,
** který tyto položky zabíraly, a uvede tabulku do počátečního stavu.
*/

void htClearAll ( tHTable* ptrht ) {
	if (ptrht == NULL)
		return;
	
	//prechod celou tabulkou
	for (int k = 0; k < HTSIZE; k++) {
		tHTItem *tmp_item = NULL;
		
		//odstranenie prvkov
		while ((*ptrht)[k] != NULL) {
			tmp_item = (*ptrht)[k];
			(*ptrht)[k] = (*ptrht)[k]->ptrnext;
			free(tmp_item);
		}
	}
	return;
}



