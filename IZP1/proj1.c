/**
 * @author Peter Tisovcik <xtisov00@fit.vutbr.cz
 * @description Projekt 1 - Vypocty v tabulke
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_ROW 1024 /* Maximalna dlzka riadka */
#define SECTION_NUMBER 3 /* Cislo, od ktoreho zacinaju parametre pre
                            vyber riadkov, stlpcov alebo rozsahu*/

/** Chybove kody programu */
enum errcode{
  ESUCCESS = 0, /**< Program prebehol v poriadku */
  ENARG, /**< Nebol zadany ziaden parameter */
  EUARG, /**< Bol zadany nepodporovany parameter/parametre */
  ELARG, /**< Bol zadany nedostatocny pocet parametrov */
  EMARG, /**< Boli zadane parametre navyse */
  ENEGARG, /**< Bol zadany zly argument, hodnoty mozu byt od 1, vratane 1 */
  ETWOISH, /**< Rozsah buniek musi byt a, b, kde a <= b */
  ENDATA, /**< Neboli najdene dalsie data na spracovanie */
  ENUMBER, /**< Na vykonanie tejto operacie musia byt v bunkach iba cisla */
  EOVRFLW, /**< Na riadok bolo zadanych viacej znakov ako je maximalny limit d*/
  EUNKNOWN /**< Neocakavana chyba */
};

/** Spravy programu */
char* errstr[] = {
  "Program prebehol v poriadku.",
  "Nebol zadany ziaden parameter.",
  "Bol zadany nepodporovany parameter/parametre.",
  "Bol zadany nedostatocny pocet parametrov.",
  "Boli zadane parametre navyse.",
  "Bol zadany zly argument, hodnoty mozu byt od 1, vratane 1, argument musi byt cislo.",
  "Rozsah buniek musi byt a, b, kde a <= b.",
  "Neboli najdene data na spracovanie, zadali ste neexistujuce cislo "
  "riadka/stlpca alebo v danom rozsahu sa nachada bunka bez dat.",
  "Na vykonanie tejto operacie musia byt v bunkach iba cisla.",
  "Na riadok bolo zadanych viacej znakov ako je maximalny limit.",
  "Neocakavana chyba."
};

/** Kody pre parametre programu */
enum comcode {
  HELP = 0,
  SELECT,
  MIN,
  MAX,
  SUM,
  AVG,
  ROW,
  COL,
  ROWS,
  COLS,
  RANGE,
  UNKNOWN
};

/** Textova reprezentacia parametrov programu */
char* command[] = {
  "--help",
  "select",
  "min",
  "max",
  "sum",
  "avg",
  "row",
  "col",
  "rows",
  "cols",
  "range",
  "unknown"
};

/** Struktura, ktora v sebe uchovava: operaciu,
    vyber, buniek, argumenty a stav programu/chybovu spravu. */
typedef struct com{
  char com1; // Prvy argument
  char com2; // Druhy argument
  int argv[4]; // Pole argumentov pre operacie
  char status; //Stav programu/chybovy stav programu
} Tcom;

/** Sprava po zadani argumentu --help */
const char *HELPMSG =
  "*****************************************************************\n"
  "* Program: Vypocty v tabulke                                    *\n"
  "* Autor:   Peter Tisovcik 2014 - xtisov00                       *\n"
  "* ------------------------------------------------------------- *\n"
  "* Pouzitie:                                                     *\n"
  "*   ./proj1 [operacia] [vyber_buniek]                           *\n"
  "*   ./proj1 --help zobrazi sa napoveda                          *\n"
  "*                                                               *\n"
  "* operacia:                                                     *\n"
  "*   select          vypise hodnoty zadanych buniek              *\n"
  "*   min             vyhlada minimum z danych buniek             *\n"
  "*   max             vyhlada maximum z danych buniek             *\n"
  "*   sum             vypise sucet danych buniek                  *\n"
  "*   avg             vypise priemer danych buniek                *\n"
  "* vyber_buniek:                                                 *\n"
  "*   row             upresnuje vyber vsetkych buniek na riadku   *\n"
  "*   col             upresnuje vyber vsetkych buniek v stlpci    *\n"
  "*   rows X Y        upresnuje vyber vsetkych buniek od riadka   *\n"
  "*                   X az po Y, vrate X a Y                      *\n"
  "*   cols X Y        upresnuje vyber vsetkych buniek od stlpca   *\n"
  "*                   X az po Y, vrate X a Y                      *\n"
  "*   range A B X Y   upresnuje vyber vsetkych buniek od riadka   *\n"
  "*                   A az po B a od stlpca X po stlpec Y         *\n"
  "*                   vratane A, B, X, Y                          *\n"
  "*****************************************************************\n";

/** Print error message
 * Vypise spravu na zaklade chyboveho kodu programu
 * @param error Chybovy kod programu
 */
void printemsg(int error) {
  fprintf(stderr, "%s\n", errstr[error]);
}

/** Get arguments
 * Zisti argumenty programu a zapise ich do struktury,
 * na ktoru sa ukazuje v parametroch programu
 * @param argc Pocet argumentov
 * @param argv Ukazovatel na pole argumentov
 * @param operation Ukazovatel na strukturu Tcom
 * @return Vracia kod chyby alebo uspesneho ukoncenia funkcie
*/
void get_argv(int argc, char** argv, Tcom* op) {
  int count_argv = 0; /* Pocet argumentov, ktore sa maju spracovat */
  int k = 0; /* Index pola cisel v strukture Tcom */
  int temp;
  char *endptr;

  /* Podmienka vyhodnoti na zaklade operacie pocet argumentov,
     ktore sa maju spracovat */
  if (op->com2 == ROW || op->com2 == COL)
    count_argv = 1;
  else if (op->com2 == ROWS || op->com2 == COLS)
    count_argv = 2;
  else if (op->com2 == RANGE)
    count_argv = 4;

  /* Podmienka vyhodnoti ci je pocet argumentov, ktore sa zadali do
     programu zhodny s poctom argumentov, ktore sa vyzaduju k prevedeniu
     jednotlivych operacii. Ak nezodpoveda pocet zadanych argumentov
     poctu argumentov, ktore operacia potrebuje, vypise sa chyba */
  if ( count_argv == (argc - SECTION_NUMBER) ) {
    /* Cyklus, ktory zapise jednotlive argumenty operacie do struktury,
       a predtym sa kontroluje ci je zadany argument cislo ak ano prevedie
       sa na cislo a ulozi sa do stuktury. Ak nie je vrati sa chyba */
    /* Avsak cislo zmensime o 1, lebo dalej s tymto udajom nebudeme
       pracovat ako s cislom riadku ale s indexom, ktory zacina od 0 */
    for (int i = SECTION_NUMBER; i < SECTION_NUMBER + count_argv; i++) {
      temp = strtol(argv[i], &endptr, 10) - 1; /* Nacitanie cisla*/
      if ( temp >= 0 && *endptr == '\0')
        /* Ulozenie argumentu do strktury. */
        op->argv[k++] = temp;
      else
        /* Osetrenie ak sa zada zaporne cislo*/
        op->status = ENEGARG;
    } /* Koniec cyklu */
  }
  else if (count_argv > (argc - SECTION_NUMBER))
    op->status = ELARG; /* Chyba ak bolo zadanych malo parametrov */
  else
    op->status = EMARG; /* Chyba ak bolo zadanych vela parametrov */

  /* Osetrenie ak sa zada b < a, podmienka mysli aj na stav, kedy
     sa bude pracovat iba s ROW a COL, cize vyzaduje sa iba jeden
     parameter a ostatne su 0 */
  if (( op->argv[0] > op->argv[1]) && op->com2 != ROW && op->com2 != COL)
    op->status = ETWOISH;

  /* Osetrenie ak sa zada x < y */
  if (( op->argv[2] > op->argv[3]))
    op->status = ETWOISH;
}

/** Get operation
 * Vyberie operaciu, ktora sa ma vykonat alebo
 * vrati chybovu spravu, ktora popisuje chybu.
 * @param argc Pocet argumentov
 * @param argv Pole argumentov
 * @return Vracia strukturu Tcom
 */
Tcom get_op(int argc, char** argv) {
  /* Deklaracia a inicializacia datovej struktury,
     hodnota -1 reprezentuje chybovy argument a
     v pripade, ze tato hodnota nebude vacsia ako -1
     vypise sa prisluchajuca chyba */
  Tcom operation = {
    .com1 = -1,
    .com2 = -1,
    .argv = {0,0,0,0},
    .status = ESUCCESS
  };

  /* Vyhodnoti ci bol zadany aspon jeden parameter */
  if (argc == 1 )
    operation.status = ENARG;

  /* Ak bol zadany len jeden parameter a to --help */
  else if (argc == 2 && (strcmp(argv[1], command[HELP]) == 0)) {
    operation.com1 = HELP;
    return operation;
  }

  /* Ak boli zadane aspon 3 parametre, zisti, ktore */
  else if (argc >= 3) {
    for (int i = 0; i <= AVG; i++) // Prvy argument
      if (strcmp(argv[1], command[i]) == 0)
        operation.com1 = i;

    for (int i = ROW; i <= RANGE; i++) // Druhy argument
      if (strcmp(argv[2], command[i]) == 0)
        operation.com2 = i;
  }

  /* Ak nebol najdeny argument vrati sa chybova hlaska
     V prvej podmienka sa testuje ci bo aspon jeden argument spravny
     V druhej podmienke sa testuje ci bol zadany zly druhy argument
     V tretej podmienke sa testuje ci bol zadany zly treti argument */
  if (operation.com1 == -1 && operation.com2 == -1 )
    operation.status = EUARG;
  if (operation.com1 != -1 && !(operation.com2 <= RANGE && operation.com2 > -1) )
    operation.status = EUARG;
  if (operation.com2 != -1 && !(operation.com1 <= RANGE && operation.com1 > -1) )
    operation.status = EUARG;

  /* Ak operacia nebola --help tak sa zisti pocet argumentov,
     ktore treba na dalsie spracovanie vypisu, takisto sa aj
     ulozia do struktury*/
  if (operation.status == ESUCCESS)
    get_argv(argc, argv, &operation);

  return operation;
}

/** Read empty line
 * Cyklus, ktory preskoci riadky pokial nenajde zaciatocnu
 * hodnotu, od ktorej sa maju riadky vypisat alebo pokial
 * nie je koniec suboru
 * @param to Limit, do ktoreho ma citat prazdne riadky
 * @param last_char Vrati posledny nacitany znak
 */
void re_line(int to, int* last_char) {
  int i = 0, c = 0;

  while (i != to && (c = getchar()) != EOF)
    if (c == '\n')
      i++;

  *last_char = c; /* Ulozenie posledneho znaku, vacsinou EOF ak je rozsah
                     znakov na vypisanie vacsi ako udajov na vstupe */
}

/** Get row
 * Funkcia vrati pole znakov z riadku
 * @param pole Obsahuje hodnoty z riadku
 * @param last_char Vrati posledny nacitany znak
 * @return Vracia kod chyby alebo uspesneho ukoncenia funkcie
 */
int get_row(char array[MAX_ROW], int *last_char) {
  int j = 0, c = 0;

  while ((c = getchar()) != '\n' && c != EOF && j < MAX_ROW) {
    /* Ak je precitany znak, biely znak, je nahradeny medzerou,
       neskor sa odstrani pri parsovani */
    if (isspace(c))
      c = ' ';

    array[j++] = c;
  }
  *last_char = c;

  array[j] = '\0'; /* Na koniec retazca pridame znak koncu retazca */

  /* Chyba v pripade ze pocet znakov je vacsi ako dovoluje
     konstanta */
  if (j >= MAX_ROW)
    return EOVRFLW;

  return ESUCCESS;
}

/** Select rows
 * Vyberie riadok/riadky a vypise ho/ich na obrazovku
 * @param r_from Cislo riadku odkial sa ma zacat citat, vratane
 * @param r_to Cislo riadku, do ktoreho sa ma citat, vratane
 * @return Vracia kod chyby alebo uspesneho ukoncenia funkcie
 */
int select_rows(int r_from, int r_to) {
  int c = 0, k, i = 0;
  char array[MAX_ROW];
  double temp;
  char *endptr;

  re_line(r_from, &c);

  /* Vypise riadky v danom rozsahu, v pripade, ze je koniec
     suboru, cyklus sa preskoci */
  for (i = r_from; i <= r_to && c != EOF; i++) {
    if ( (temp = get_row(array, &c)) != ESUCCESS)
      return temp;

    /* Nahradime medzeri znakom \0 a postupne budeme nacitavat
       retazce a konvertovat na cislo v cykle. */
    char *col = strtok(array, " ");
    k = 0;
    while (col != NULL) {
      temp = strtod(col, &endptr); /* Nacitanie cisla*/

      if (*endptr != '\0')
        printf("%s\n", col);
      else
        printf("%.10g\n", temp);

      col = strtok(NULL, " ");
      k++;
    }

    /* Podmienka zabezpeci vypis nicoho
    if(strlen(array) == 0) {
      k++;
    }*/
  }

 /* Vypise chybu v pripade, ze sa zadal vacsi pocet riadkov ako
     je pocet riadkov s datami */
  if (i-1 != r_to || k == 0)
    return ENDATA;

  return ESUCCESS;
}

/** Min, max, sum, avg rows
 * Na zaklade cisla prvej operacie vypise hodnotu a to bud
 * MIN, MAX, SUM alebo AVG, funkcia overuje ci su zadane
 * len cisla, pocita aj s moznostou, ze budu cisla uvedene
 * so znamienkom alebo desatinne
 * @param from Cislo riadku odkial sa ma zacat citat, vratane
 * @param to Cislo riadku, do ktoreho sa ma citat, vratane
 * @return Vracia kod chyby alebo uspesneho ukoncenia funkcie
*/
int msa_rows(int from, int to, int com1) {
  int c, i = 0, k = 0, j = 0, read_data = 0;
  int count_number = 0; /* Pocet cisel, sluzi na vypocet AVG */
  char array[MAX_ROW];
  double result = 0,temp;
  char *endptr;

  re_line(from, &c);

  /* Do pola sa ulozi retazec cakajuci na prevedenie na retazce
     rozdele na zaklade medzier a nasledne vykona MIN, MAX, SUM, AVG*/
  for (i = from; i <= to && c != (int)EOF; i++) {
    read_data = 0;
    if ( (temp = get_row(array, &c)) != ESUCCESS)
      return temp;

    /* Koniec programu ak uz nie su data na citanie */
    if (strlen(array) == 0 && c == (int)EOF) {
      /* Vypise chybu v pripade, ze sa zadal vacsi pocet riadkov ako
         je pocet riadkov s datami */
      if (i == to || i < to)
        return ENDATA;
      return ESUCCESS;
    }

    /* Nahradime medzeri znakom \0 a postupne budeme nacitavat
       retazce a konvertovat na cislo v cykle. */
    char *col = strtok(array, " ");
    j = 0;
    while (col != NULL) {
      temp = strtod(col, &endptr); /* Nacitanie cisla*/ /* Skonvertovanie stringu na cislo */
       if (*endptr != '\0')
        return ENUMBER;

      /* Prvotne priradenie hodnoty do premennej, kora vyjadruje
         MIN, MAX okrem AVG a SUM, pretoze tie by a pripocitali dvakrat */
      if (k == 0 && com1 != SUM && com1 != AVG) {
        result = temp;
        k++;
      }

      /* Podmienka, ktora ulozi MIN, MAX, SUM alebo AVG z cisiel,
         ktore dostava na vstup */
      if (com1 == MIN && result > temp)
        result = temp;
      else if (com1 == MAX && result < temp)
        result = temp;
      else if (com1 == SUM)
        result += temp;
      else if (com1 == AVG) {
        result += temp;
        count_number++;
      }

      col = strtok(NULL, " ");
      j++;
    }
    /* Osetrenie situacie, ked bol zadany EOF za stringom v riadku */
    if (c == (int)EOF)
        read_data = 1;

    if (j == 0)
      return ENDATA;
  } /* Koniec citania riadkov */

  /* Vypise sa len vtedy ak nie je mimo rozsahu zadanych dat,
     inaksie by vypisalo 0, ak by bol zadany riadok vacsi ako
     pocet zadanych udajov */
  if ((c != (int)EOF || read_data == 1 ) && i-1 == to) {
    /* Vypise MIN, MAX, SUM, AVG */
    if (com1 == AVG)
      printf("%.10g\n", result/count_number);
    else
      printf("%.10g\n", result);
  }
  else
    return ENDATA;

  return ESUCCESS;
}

/** Select cols
 * Vyberie stlpec/stlpce a vypise ho/ich na obrazovku
 * @param from Cislo riadku odkial sa ma zacat citat, vratane
 * @param to Cislo riadku, do ktoreho sa ma citat, vratane
 * @return Vracia kod chyby alebo uspesneho ukoncenia funkcie
 */
int select_cols(int from, int to) {
  int c, i;
  double temp;
  char array[MAX_ROW] = {0};
  char *endptr;

  /* Cyklus, ktory pojde dovtedy pokym sa neskonci vstup alebo pokial
     ma data na vstupe alebo pokial nedojde k chybe */
  while(1) {
    if ( (temp = get_row(array, &c)) != ESUCCESS)
      return temp;

    /* Koniec programu ak uz nie su data na citanie */
    if (strlen(array) == 0 && c == (int)EOF)
      return ESUCCESS;

    /* Nahradime medzeri znakom \0 a postupne budeme nacitavat
       retazce a konvertovat na cislo v cykle. */
    char *col = strtok(array, " ");

    i = 0; /* Pocitadlo poctu stlpcov*/
    while (col != NULL) {
      /* Ukoncenie v pripade ze je zadany vacsi pocet stlpcov ako sa
         objavilo na vstupe */
      if (i > to)
        break;

      /* Zobrazia sa iba data v zadanom rozsahu*/
      if (i >= from && i <= to) {
         temp = strtod(col, &endptr); /* Nacitanie cisla*/

         if (*endptr != '\0')
           printf("%s\n", col);
         else
           printf("%.10g\n", temp);
      }

      col = strtok(NULL, " ");
      i++;
    }

    /* Vrati chybu v pripade, ze bol zadany select na stlpce,
       ktore sa v zadanom rozsahu nevyskytuju */
    if (i-1 != to)
      return ENDATA;
  }
  return ESUCCESS;
}

/** Min, max, sum, avg cols
 * Na zaklade cisla prvej operacie vypise hodnotu a to bud
 * MIN, MAX, SUM alebo AVG
 * @param from Cislo riadku odkial sa ma zacat citat, vratane
 * @param to Cislo riadku, do ktoreho sa ma citat, vratane
 * @return Vracia kod chyby alebo uspesneho ukoncenia funkcie
*/
int msa_cols(int from, int to, int com1) {
  int c, i, k = 0, count_number = 0;
  char array[MAX_ROW] = {0};
  double temp = 0, result = 0;
  char *endptr;

  /* Cyklus, ktory pojde dovtedy pokym sa neskonci vstup alebo pokial
     ma data na vstupe */
  while(1) {
    if ( (temp = get_row(array, &c)) != ESUCCESS)
      return temp;

    /* Koniec programu ak uz nie su data na citanie */
    if (strlen(array) == 0 && c == (int)EOF)
      break;

    /* Nahradime medzeri znakom \0 a postupne budeme nacitavat
       retazce a konvertovat na cislo v cykle. */
    char *col = strtok(array, " ");

    i = 0;
    while (col != NULL) {
      /* Preskocenie nepotrebnych stlpcov */
      if (i < from) {
        i++;
        col = strtok(NULL, " ");
        continue;
      }

      if (i >= from && i <= to) {
        temp = strtod(col, &endptr); /* Nacitanie cisla*/ /* Skonvertovanie stringu na cislo */
        if (*endptr != '\0')
          return ENUMBER;

        /* Prvotne priradenie hodnoty do premennej, kora vyjadruje
           MIN, MAX, SUM alebo AVG */
        if (k == 0 && com1 != SUM && com1 != AVG) {
          result = temp;
          k++;
        }
      }
      /* Ukoncenie v pripade ze je zadany vacsi pocet stlpcov ako sa
         objavilo na vstupe */
      if (i > to)
        break;

      /* Podmienka, ktora bud ulozi MIN, MAX, SUM alebo AVR z cisiel,
         ktore dostava na vstup */
      if (com1 == MIN && result > temp)
        result = temp;
      else if (com1 == MAX && result < temp)
        result = temp;
      else if (com1 == SUM)
        result += temp;
      else if (com1 == AVG) {
        result += temp;
        count_number++;
      }

      col = strtok(NULL, " ");
      i++; /* Pocitadlo poctu stlpcov*/
    }

    /* Vrati chybu v pripade, ze bol zadany select na stlpce,
       ktore sa v zadanom rozsahu nevyskytuju */
    if (i-1 != to)
      return ENDATA;
  }

  /* Vypise MIN, MAX, SUM, AVG */
  if (com1 == AVG)
    printf("%.10g\n", result/count_number);
  else
    printf("%.10g\n", result);

  return ESUCCESS;
}

/** Select range
 * Vyberie rozsah z riadkov a stlpcov a tie vypise na obrazovku
 * @param x Cislo riadku odkial sa ma zacat citat, vratane
 * @param y Cislo riadku, do ktoreho sa ma citat, vratane
 * @param from Cislo stlpca, od ktoreho sa ma zacat citat, vratane
 * @param to Cislo stlpca, do ktoreho sa ma citat, vratane
 * @return Vracia kod chyby alebo uspesneho ukoncenia funkcie
 */
int select_range(int r_from, int r_to, int c_from, int c_to) {
  int temp, i, c;
  char array[MAX_ROW] = {0};

  re_line(r_from, &c);

  /* Cyklus, ktory pojde dovtedy pokym sa neskonci vstup alebo pokial
     ma data na vstupe */
  while(r_from <= r_to && c != EOF) {
    if ( (temp = get_row(array, &c)) != ESUCCESS)
      return temp;

    /* Nahradime medzeri znakom \0 a postupne budeme nacitavat
       retazce a konvertovat na cislo v cykle. */
    char *col = strtok(array, " ");

    i = 0; /* Pocitadlo poctu stlpcov*/
    while (col != NULL) {
      /* Ukoncenie v pripade ze je zadany vacsi pocet stlpcov ako sa
         objavilo na vstupe */
      if (i > c_to)
        break;

      /* Zobrazia sa iba data v zadanom rozsahu*/
      if (i >= c_from && i <= c_to)
        printf("%s\n", col);

      col = strtok(NULL, " ");
      i++;
    }

    /* Vrati chybu v pripade, ze bol zadany select na stlpec,
       ktore sa v zadanom rozsahu nevyskytuju */
    if (i-1 != c_to)
      return ENDATA;

    r_from++; /* pocet riadkov*/
  }

  /* Vrati chybu v pripade, ze bol zadany select na riadok,
     ktore sa v zadanom rozsahu nevyskytuju */
  if (r_from-1 != r_to)
    return ENDATA;

  return ESUCCESS;
}

/** Min, max, sum, avg range
 * Na zaklade cisla prvej operacie vypise hodnotu a to bud
 * MIN, MAX, SUM alebo AVG
 * @param r_from Cislo riadku odkial sa ma zacat citat, vratane
 * @param r_to Cislo riadku, do ktoreho sa ma citat, vratane
 * @param c_from Cislo stlpca odkial sa ma zacat citat, vratane
 * @param c_to Cislo stlpca, do ktoreho sa ma citat, vratane
 * @return Vracia kod chyby alebo uspesneho ukoncenia funkcie
*/
int msa_range(int r_from, int r_to, int c_from, int c_to, int com1) {
  int i, c, count_number = 0, k = 0;
  char array[MAX_ROW] = {0};
  double temp = 0, result = 0;
  char *endptr;

  re_line(r_from, &c);

  /* Cyklus, ktory pojde dovtedy pokym sa neskonci vstup alebo pokial
     ma data na vstupe */
  while(r_from <= r_to && c != EOF) {
    if ( (temp = get_row(array, &c)) != ESUCCESS)
      return temp;

    /* Nahradime medzeri znakom \0 a postupne budeme nacitavat
       retazce a konvertovat na cislo v cykle. */
    char *col = strtok(array, " ");

    i = 0; /* Pocitadlo poctu stlpcov*/
    while (col != NULL) {
      /* Preskocenie nepotrebnych stlpcov */
      if (i < c_from) {
        i++;
        col = strtok(NULL, " ");
        continue;
      }

      /* Ukoncenie v pripade ze je zadany vacsi pocet stlpcov ako sa
         objavilo na vstupe */
      if (i > c_to)
        break;

      if (i >= c_from && i <= c_to) {
        temp = strtod(col, &endptr); /* Nacitanie cisla*/ /* Skonvertovanie stringu na cislo */
        if (*endptr != '\0')
          return ENUMBER;

        /* Prvotne priradenie hodnoty do premennej, kora vyjadruje
           MIN, MAX, SUM alebo AVG */
        if (k == 0 && com1 != SUM && com1 != AVG) {
          result = temp;
          k++;
        }
      }

      /* Podmienka, ktora bud ulozi MIN, MAX, SUM alebo AVR z cisiel,
         ktore dostava na vstup */
      if (com1 == MIN && result > temp)
        result = temp;
      else if (com1 == MAX && result < temp)
        result = temp;
      else if (com1 == SUM)
        result += temp;
      else if (com1 == AVG) {
        result += temp;
        count_number++;
      }

      col = strtok(NULL, " ");
      i++;
    }

    /* Vrati chybu v pripade, ze bol zadany select na stlpec,
       ktore sa v zadanom rozsahu nevyskytuju */
    if (i-1 != c_to)
      return ENDATA;

    r_from++; /* pocet riadkov*/
  }

  /* Vrati chybu v pripade, ze bol zadany select na riadok,
     ktore sa v zadanom rozsahu nevyskytuju */
  if (r_from-1 != r_to)
    return ENDATA;

  /* Vypise MIN, MAX, SUM, AVG */
  if (com1 == AVG)
    printf("%.10g\n", result/count_number);
  else
    printf("%.10g\n", result);

  return ESUCCESS;
}

/** Select
 * Vypise konkretnu operaciu
 * @param op Struktura typu Tcom, ktora predava vsetky
 *           potrebne udaje
 */
void select_op(Tcom* op) {
  if (op->com2 == ROW )
    op->status = select_rows(op->argv[0], op->argv[0]);
  else if (op->com2 == ROWS)
    op->status = select_rows(op->argv[0], op->argv[1]);
  else if (op->com2 == COL)
    op->status = select_cols(op->argv[0], op->argv[0]);
  else if (op->com2 == COLS)
    op->status = select_cols(op->argv[0], op->argv[1]);
  else if (op->com2 == RANGE)
    op->status = select_range(op->argv[0], op->argv[1], op->argv[2], op->argv[3]);
}

/** Min, max, sum, avg
 * Vypise konkretnu operaciu
 * @param op Struktura typu Tcom, ktora predava vsetky
 *           potrebne udaje
 */
void msa(Tcom* op) {
  if (op->com2 == ROW)
    op->status = msa_rows(op->argv[0], op->argv[0], op->com1);
  if (op->com2 == ROWS)
    op->status = msa_rows(op->argv[0], op->argv[1], op->com1);
  else if (op->com2 == COL)
    op->status = msa_cols(op->argv[0], op->argv[0], op->com1);
  else if (op->com2 == COLS)
    op->status = msa_cols(op->argv[0], op->argv[1], op->com1);
  else if (op->com2 == RANGE)
    op->status = msa_range(op->argv[0], op->argv[1], op->argv[2], op->argv[3], op->com1);
}

/** Main
 * Hlavny program
 * @param argc Pocet zadanych argumentov programu
 * @param argv Pole obsahuje argumenty predane programu
 * @return Vracia kod chyby alebo uspesneho ukoncenia programu
 */
int main(int argc, char *argv[]) {
  /* Nacitanie argumentov do struktury */
  Tcom operation = get_op(argc, argv);

  /* Vypis chyby, ktora vznikne pri nacitani udajov
     do struktury */
  if (operation.status != ESUCCESS) {
    printemsg(operation.status);
    return operation.status;
  }

  /* Vyber operacii, ktore sa maju vykonad nad tabulkou */
  switch (operation.com1) {
    case HELP:
      printf("%s", HELPMSG);
      break;
    case SELECT:
      select_op(&operation);
      break;
    case MIN:
      msa(&operation);
      break;
    case MAX:
      msa(&operation);
      break;
    case SUM:
      msa(&operation);
      break;
    case AVG:
      msa(&operation);
      break;
    default:
      printemsg(EUARG);
      break;
  }

  /* Vypis chyby, ktora vznikne pri vypise udajov z
     tabulky */
  if (operation.status != ESUCCESS) {
    printemsg(operation.status);
    return operation.status;
  }

  return EXIT_SUCCESS;
}
