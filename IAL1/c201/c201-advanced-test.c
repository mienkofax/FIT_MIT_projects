/**/
/*
 *  Předmět: Algoritmy (IAL) - FIT VUT v Brně
 *  Pokročilé testy pro příklad c201.c (Jednosměrně vázaný lineární seznam)
 *  Vytvořil: Martin Tuček, září 2005
 *  Úprava: Bohuslav Křena, říjen 2006
 */

#include "c201.h"
                                                         /* pracovní proměnné */
tList TEMPLIST;
int obsah;
int MaxListLength = 100;                     /* Handles wrongly linked lists. */

/*******************************************************************************
 * Pomocné funkce usnadňující testování vlastní implementace.
 ******************************************************************************/

void print_elements_of_list(tList SEZNAM)	{
/* Vytiskne seznam. */
	tList temp_S=SEZNAM;
	int CurrListLength = 0;
	printf("-----------------");
	while ((temp_S.First!=NULL) && (CurrListLength<MaxListLength))	{
		printf("\n\t %d",temp_S.First->data);
		if ((temp_S.First==SEZNAM.Act) && (SEZNAM.Act!=NULL))
			printf("\t <= toto je aktivní prvek ");
		temp_S.First=temp_S.First->ptr;
		CurrListLength++;
	}
    if (CurrListLength>=MaxListLength)
        printf("\nList exceeded maximum length!");
	printf("\n-----------------\n");
}

int use_copy(tList *TL, int *obsah)	{
/* Ošetřuje pouľití operace Copy. */
	int tmp;
	Copy(TL,&tmp);
	if (!solved) {
		printf("Operace Copy nebyla implementována!\n");
		return(FALSE);
	}
	else {
		if (errflg)	{
			printf("Operace Copy volala funkci Error.\n");
			errflg=FALSE;
			return(FALSE);
		}
		else	{
			*obsah=tmp;
			printf("Operace Copy vrací obsah %d.\n",tmp);
			return(TRUE);
		}
	}
}

int use_copy_first(tList *TL, int *obsah)	{
/* Ošetřuje pouľití operace CopyFirst. */
	int tmp;
	CopyFirst(TL,&tmp);

	if (!solved)	{
		printf("Operace CopyFirst nebyla implementována!\n");
		return(FALSE);
	}
	else {
		if (errflg)	{
			printf("Operace CopyFirst volala funkci Error.\n");
			errflg=FALSE;
			return(FALSE);
		}
		else	{
			*obsah=tmp;
			printf("Operace CopyFirst vrací obsah %d.\n",tmp);
			return(TRUE);
		}
	}
}

int use_active(tList TL)	{
/* Ošetřuje pouľití operace Active. */
	int IsActive = Active(&TL);
	if (!solved)	{
		printf("Operace Active nebyla implementována!\n");
		return(FALSE);
	}
	else	{
		printf("Operace Active vrací %d.\n",IsActive);
		return(TRUE);
	}
}

/*
 * Následující funkce volané z vlastních testů uvedených ve funkci main
 * kontrolují, zda byly jednotlivé funkce implementovány,
 * případně vypisují aktuální stav pracovního seznamu TEMPLIST.
 */

int test_InitList()	{

	solved=TRUE;
	InitList(&TEMPLIST);
	if (!solved)	{
		printf("Operace InitList nebyla implementovana \n");
		return(FALSE);
	}
	else	{
		print_elements_of_list(TEMPLIST);
		return(TRUE);
	}

}

int test_Copy()	{

	solved=TRUE;
	return(use_copy(&TEMPLIST,&obsah));

}

int test_CopyFirst()	{

	solved=TRUE;
	return(use_copy_first(&TEMPLIST,&obsah));

}

int test_InsertFirst()	{

	solved=TRUE;
	InsertFirst(&TEMPLIST,obsah);
	if (!solved)	{
		printf("Operace InsertFirst nebyla implementována!\n");
		return(FALSE);
	}
	else	{
		print_elements_of_list(TEMPLIST);
		return(TRUE);
	}

}

int test_First()	{

	solved=TRUE;
	First(&TEMPLIST);
	if (!solved)	{
		printf("Operace First nebyla implementována!\n");
		return(FALSE);
	}
	print_elements_of_list(TEMPLIST);
	return(TRUE);

}

int test_Active()	{

	solved=TRUE;
	return(use_active(TEMPLIST));

}

int test_Succ()	{

	solved=TRUE;
	Succ(&TEMPLIST);
	if (!solved)	{
		printf("Operace Succ nebyla implementována!\n");
		return(FALSE);
	}
	print_elements_of_list(TEMPLIST);
	return(TRUE);

}

int test_Actualize()	{

	solved=TRUE;
	Actualize(&TEMPLIST,obsah);
	if (!solved)	{
		printf("Operace Actualize nebyla implementována!\n");
		return(FALSE);
	}
	print_elements_of_list(TEMPLIST);
	return(TRUE);

}

int test_DeleteFirst()	{

	solved=TRUE;
	DeleteFirst(&TEMPLIST);
	if (!solved)	{
		printf("Operace DeleteFirst() nebyla implementována!\n");
		return(FALSE);
	}
	print_elements_of_list(TEMPLIST);
	return(TRUE);

}

int test_PostDelete()	{

	solved=TRUE;
	PostDelete(&TEMPLIST);
	if (!solved)	{
		printf("Operace PostDelete() nebyla implementována!\n");
		return(FALSE);
	}
	print_elements_of_list(TEMPLIST);
	return(TRUE);

}

int test_PostInsert()	{

	solved=TRUE;
	PostInsert(&TEMPLIST,obsah);
	if (!solved){
		printf("Operace PostInsert nebyla implementována!\n");
		return(FALSE);
	}
	print_elements_of_list(TEMPLIST);
	return(TRUE);

}

int test_DisposeList() {

    solved=TRUE;
    DisposeList(&TEMPLIST);
    if (!solved) {
        printf("Operace DisposeList() nebyla implementována!\n");
        return(FALSE);
    }
    print_elements_of_list(TEMPLIST);
    return(TRUE);
}

/*******************************************************************************
 * POKROČILÉ TESTY
 * ---------------
 * Nejsou dostupné studentům při řešení domácích úloh.
 * Za jejich úspěšné projítí získá student druhou část bodů za příklad.
 *
 ******************************************************************************/

int main(int argc, char *argv[])	{

    printf("Jednosměrně vázaný lineární seznam\n");
    printf("==================================\n");

                            /* Testy 01 aľ 09 se shodují se základními testy. */

    printf("\n[TEST01]\n");
    printf("Inicializace seznamu\n");
    printf("~~~~~~~~~~~~~~~~~~~~\n");
    test_InitList();

    printf("\n[TEST02]\n");
    printf("Pokus o volání CopyFirst nad prázdným seznamem => chyba\n");
    printf("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
    test_CopyFirst();

    printf("\n[TEST03]\n");
    printf("Zavoláme 4x operaci InsertFirst.\n");
    printf("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
    obsah=1; test_InsertFirst();
    obsah=2; test_InsertFirst();
    obsah=3; test_InsertFirst();
    obsah=4; test_InsertFirst();

    printf("\n[TEST04]\n");
    printf("Seznam je neaktivní -- ověříme si to voláním funce Active.\n");
    printf("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
    test_Active();

    printf("\n[TEST05]\n");
    printf("Otestujeme funkci First při neaktivním seznamu a funkci Active.\n");
    printf("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
    test_First();
    test_Active();

    printf("\n[TEST06]\n");
    printf("Test funkce Copy při aktivitě na prvním prvku\n");
    printf("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
    test_Copy();

    printf("\n[TEST07]\n");
    printf("Test funkce Succ -- voláme 3x, aktivita bude na posledním prvku.\n");
    printf("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
    test_Succ();
    test_Succ();
    test_Succ();
    test_Active();

    printf("\n[TEST08]\n");
    printf("Aktualizujeme obsah aktivního prvku.\n");
    printf("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
    obsah=10;
    test_Actualize();
    test_Active();
    test_Copy();

    printf("\n[TEST09]\n");
    printf("Provedeme ještě jednou Succ -- aktivita se ztratí.\n");
    printf("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
    test_Succ();
    test_Active();

           /* Testy 10 aľ 14 testují chování operací nad neaktivním seznamem. */

    printf("\n[TEST10]\n");
    printf("Pokusíme se o aktualizaci při neaktivním seznamu => nic\n");
    printf("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
    obsah=100;
    test_Actualize();
    test_Active();

    printf("\n[TEST11]\n");
    printf("Pokus o Copy při neaktivním seznamu => ošetřená chyba\n");
    printf("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
    test_Copy();

    printf("\n[TEST12]\n");
    printf("Operace Succ při neaktivním seznamu nesmí zhavarovat.\n");
    printf("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
    test_Succ();
    test_Active();

    printf("\n[TEST13]\n");
    printf("Pouľití operace CopyFirst při neaktivním seznamu\n");
    printf("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
    test_CopyFirst();

    printf("\n[TEST14]\n");
    printf("Vyzkoušíme operaci DeleteFirst při neaktivním seznamu,\n");
    printf("přičemľ smazaný prvek zase vrátíme zpátky do seznamu.\n");
    printf("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
    test_DeleteFirst();
    test_InsertFirst();
    test_Active();

                   /* Testy 15 aľ 23 se shodují se základními testy 10 aľ 18. */

    printf("\n[TEST10]\n");
    printf("Operace First nastaví aktivitu na první prvek.\n");
    printf("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
    test_First();
    test_Active();

    printf("\n[TEST11]\n");
    printf("DeleteFirst aktivního prvku povede ke ztrátě aktivity.\n");
    printf("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
    test_DeleteFirst();
    test_Active();

    printf("\n[TEST12]\n");
    printf("Uľitím operací First a Succ nastavíme aktivitu na konec seznamu.\n");
    printf("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
    test_First();
    test_Succ();
    test_Succ();
    test_Active();

    printf("\n[TEST13]\n");
    printf("Operace PostDelete při aktivitě na posledním prvku nedělá nic.\n");
    printf("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
    test_PostDelete();
    test_Active();

    printf("\n[TEST14]\n");
    printf("Nastavíme aktivitu na začátek a dvakrát zavoláme PostDelete.\n");
    printf("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
    test_First();
    test_Active();
    test_PostDelete();
    test_PostDelete();
    test_Active();

    printf("\n[TEST15]\n");
    printf("Otestujeme InsertFirst při seznamu s jediným prvkem, nastavíme\n");
    printf("aktivitu na nově vloľený prvek a opět posuneme aktivitu na konec\n");
    printf("seznamu.\n");
    printf("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
    obsah=101;
    test_InsertFirst();
    test_First();
    test_Succ();
    test_Active();

    printf("\n[TEST16]\n");
    printf("Operací PostInsert vloľíme nový prvek za poslední prvek seznamu.\n");
    printf("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
    obsah=102;
    test_PostInsert();
    test_Active();

    printf("\n[TEST17]\n");
    printf("Nastavíme aktivitu na první prvek seznamu a vyzkoušíme PostInsert.\n");
    printf("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
    test_First();
    obsah=103;
    test_PostInsert();
    test_Active();

    printf("\n[TEST18]\n");
    printf("Otestujeme funkčnost operace DisposeList.\n");
    printf("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
    test_DisposeList();

             /* Testy 24 aľ 30 testují chování operací nad prázdným seznamem. */

    printf("\n[TEST24]\n");
    printf("\n");
    printf("Následuje testování operací při prázdném seznamu\n");
    printf("================================================\n");
    test_Active();

    printf("\n[TEST25]\n");
    printf("DeleteFirst při prázdném seznamu => nic\n");
    printf("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
    test_DeleteFirst();

    printf("\n[TEST26]\n");
    printf("Test PostDelete při prázdném seznamu => nic \n");
    printf("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ \n");
    test_PostDelete();

    printf("\n[TEST27]\n");
    printf("Ještě jednou DisposeList => ľádná změna\n");
    printf("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
    test_DisposeList();

    printf("\n[TEST28]\n");
    printf("First ani PostInsert nad prázdným seznamem nic nedělá.\n");
    printf("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
    test_First();
    obsah=201;
    test_PostInsert();

    printf("\n[TEST29]\n");
    printf("Copy a CopyFirst nad prázdným seznamem => ošetřená chyba\n");
    printf("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
    test_Copy();
    test_CopyFirst();

    printf("\n[TEST30]\n");
    printf("Actualize ani Succ by neměly mít nad prázdným seznamem ľádny efekt.\n");
    printf("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
    obsah=202;
    test_Actualize();
    test_Succ();
    test_Active();

    printf("\n----------------------- konec příkladu c201 -------------------------\n");


	return(0);
}
/**/
