/**
 * @author Peter Tisovcik <xtisov00@fit.vutbr.cz
 * @description Projekt 3 - Prechod bludiskom
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>
#include <limits.h>

#define MAX_ARGUMENTOV 5
#define MAX_NUMBER 7
#define POCET_HRAN 3
#define LAVY_RAM 1
#define PRAVY_RAM 2
#define HORNY_DOLNY_RAM 4
#define STAY 3
#define UP 1
#define DOWN 0

const char *HELPMSG =
  "*****************************************************************\n"
  "* Program: Prechod bludiskom                                    *\n"
  "* Autor:   Peter Tisovcik 2014 - xtisov00                       *\n"
  "* ------------------------------------------------------------- *\n"
  "* Pouzitie:                                                     *\n"
  "*   ./proj3 --help zobrazi sa napoveda                          *\n"
  "*   ./proj3 --test subor.txt - overi platnost bludiska          *\n"
  "*   ./proj3 --rpath R C subor.txt - najde sa najkratsia cesta   *\n"
  "*             pomocou pravidla pravej ruky                      *\n"
  "*   ./proj3 --lpath R C subor.txt - najde sa najkratsia cesta   *\n"
  "*             pomocou pravidla lavej ruky                       *\n"
  "*   ./proj3 --shortest R C subor.txt - najde sa najkratsia      *\n"
  "*             cestu z bludiska                                  *\n"
  "*                                                               *\n"
  "*   R - urcuje suradnicu riadku                                 *\n"
  "*   C - urcuje suradnicu stlpca                                 *\n"
  "*   subor.txt - subor, ktory obsahuje bludisko                  *\n"
  "*****************************************************************\n";

/** Chybove kody programu */
enum errcode {
   E_OK,
   E_NARGV,
   E_VALID,
   E_INVALID,
   E_ALLOC,
   E_START,
   E_PATH,
   E_BORDER,
   E_COORDINATION
};

/** Spravy programu */
char *errstr[] = {
   "Program prebehol v poriadku.",
   "Bol zadany neplatny argument/argumenty.",
   "Valid.",
   "Invalid.",
   "Chyba pri dynamickom alokovani pola",
   "Bol zadany neplatny startovaci bod alebo zaciatocne suradnice su mimo bludisko.",
   "Nebola najdena najkratsia cesta.",
   "Na zadanej hranici je stena.",
   "Suradnice pre vstup do bludiska sa nenachadzaju na okraji bludiska."
};

/** Kody pre parametre programu */
enum comcode {
   HELP,
   TEST,
   RPATH,
   LPATH,
   SHORTEST,
   E_MNUMBER
};

/** Textova reprezentacia parametrov programu */
char *command[] = {
   "--help",
   "--test",
   "--rpath",
   "--lpath",
   "--shortest"
};

/** Struktura, ktora v sebe uchovava: operaciu,
    riadok a stlpec, kde sa vstupuje do bludiska a
    nazov suboru */
typedef struct com {
   int command;
   int row;
   int col;
   char *filename;
} Tcom;

/** Struktura, ktora obsahuje zaciatocne udaje, kde sa vstupuje
   do bludiska a ukazovatel na pole riadkov matice */
typedef struct {
   int rows;
   int cols;
   unsigned char *cells;
} Map;

/** Struktura pre cestu */
typedef struct {
   int rows;
   int cols;
   int index;
   int min_cost;
   int *cells;
} TPath;

/** Struktura pre polozky v bufferu */
typedef struct item {
   char row;
   char col;
   struct item *next;
} Titem;

/** Struktura pre buffer */
typedef struct buffer {
   Titem *first;
} Buffer;

/** Inicializacia bufferu
 * Funkcia inicializuje buffer
 * @param buf Ukazovatel na strukturu Buffer, ktora sa ma inicializovat
 */
void buffer_init(Buffer *buf) {
   buf->first = NULL;
}

/** Buffer insert first
 * Funkcia vlozi do bufferu prvu polozku
 * @param buf Ukazovatel na strukturu Buffer, do ktorej sa vlozi prva polozka
 * @param item Ukazovatel na skturutu dat, ktore sa vlocia ako prva polozka do Bufferu
 */
void buffer_insert_first(Buffer *buf, Titem *item) {
   item->next = buf->first; //priradenie pointra z prvej polozky do item ako nasledujuca polozka
   buf->first = item; //do prvej polozky vlozime pointer
}

/** Buffer insert data
 * Funkcia ulozi na zaciatok novu polozku.
 * @param Buffer Struktura, ktora obsahuje buffer
 * @param row Cislo riadku, ktore sa ma ulozit do buffera
 * @param coll Cislo stlpca,ktore sa ma ulozit do buffera
 * @return Vracia kod chyby alebo uspesneho ukoncenia
 */
int buffer_insert_data(Buffer *buffer, int row, int coll) {
   Titem *item = malloc(sizeof(Titem)); //alokovanie

   item->row = row; //zapis suradnice
   item->col = coll; //zapis suradnice
   item->next = NULL; //zapis nuloveho pointra00

   if (item == NULL)
      return 0;

      buffer_insert_first(buffer, item);
   return 1;
}

/** Buffer insert last
 * Funkcia ulozi do Buffera novu polozku na koniec.
 * @param Buffer Struktura, ktora obsahuje buffer
 * @param row Cislo riadku, ktore sa ma ulozit do buffera
 * @param coll Cislo stlpca,ktore sa ma ulozit do buffera
 * @return Vracia kod chyby alebo uspesneho ukoncenia
 */
int buffer_insert_last(Buffer *buffer, int row, int coll) {
   Titem *item = malloc(sizeof(Titem)); //alokovanie

   item->row = row; //zapis suradnice
   item->col = coll; //zapis suradnice
   item->next = NULL; //zapis nuloveho pointra00

   if (item == NULL)
      return 0;

   if (buffer->first == NULL)  //ak je polozka prva
      buffer_insert_first(buffer, item);
   else {
      Titem *cursor; //docasna premenna

      /* Najdenie poslednej polozky v bufferu */
      for(cursor = buffer->first; cursor->next != NULL; cursor = cursor->next)
         ;
      cursor->next = item; //pridanie ukazovatela na nasledujucu polozku, predposlednemu prvku
   }
   return 1;
}

/** Buffer is empty
 * Funkcia overi, ci sa este v bufferu nachadzaju udaje
 * @param Buffer Struktura, ktora obsahuje buffer
 */
int buffer_is_empty(Buffer *buf) {
   if (buf->first == NULL)
      return 1;
   return 0;
}

/** Buffer select data
 * Funkcia vyberie prvu hodnotu z bufferu.
 * @param Buffer Struktura, ktora obsahuje buffer
 * @param dest Struktura do, ktorej sa ulozia udaje
 */
void buffer_select_data(Buffer *buf, Titem *dest) {
   Titem *item;
   item = buf->first; //ulozenie pointra, ktory odkazuje na prvu polozku
   buf->first = item->next; //ulozenie noveho pointra, bude to vlastne prvy zaznam

   dest->row = item->row; //nacitanie suradnic
   dest->col = item->col; //nacitanie suradnic
   dest->next = NULL;

   free(item) ; //uvolnenie polozky z pamate
}

/** Select data
 * Funkcia vyberie udaje z bufferu typu FIFO
 * @param buf Struktura, ktora obsahuje buffer typu LIFO
 */
void print_coordinate(Buffer *buf) {
   while (!buffer_is_empty(buf)) {
      Titem item;
      buffer_select_data(buf, &item);
      printf("%d,%d\n", item.row+1, item.col+1);
   }
}

/** Initialize Map
 * Funkcia alokuje pamat na heape pre pole riadkov matice
 * @param map Ukazoval na strukturu, v ktorej sa ma alokovat pre
 *            bunku cells pamat
 * @return Vracia kod chyby alebo uspesneho ukoncenia funkcie
 */
int initialize_map(Map *map) {
   map->cells = (unsigned char *) malloc(map->rows * map->cols * sizeof(unsigned char));
   if (map->cells == NULL)
      return E_ALLOC;

   return E_OK;
}

/** Free Map
 * Funkcia dealokuje alokovanu pamat pre bunku cells
 */
int free_map(Map *map) {
   free(map->cells);
   return E_OK;
}

/** Free path
 * Funkcia dealokuje alokovanu pamat pre bunku cells
 */
int free_path(TPath *path) {
   free(path->cells);
   return E_OK;
}

/** Print error message
 * Vypise spravu na zaklade chyboveho kodu programu
 * @param error Chybovy kod programu
 */
void printemsg(int error) {
   fprintf(stderr, "%s\n", errstr[error]);
}

/** Is integer
 * Funkcia zisti, ci je cislo typu int a ci je to cislo
 * @param str Ukazovatel na retazec, ktory obsahuje int cislo
 * @return Vrati 1 ak to je cislo a 0 ak to nie je cislo typu int
 */
int isinteger(char *str) {
   char *endptr = NULL;
   strtol(str, &endptr, 10);

   if (*endptr != '\0')
      return 0;
   return 1;
}

/** Get operation
 * Vyberie operaciu, ktora sa ma vykonat alebo
 * vrati chybovu spravu, ktora popisuje chybu.
 * Operacie a argumenty operacii uklada do struktury.
 * @param argc Pocet argumentov
 * @param argv Pole argumentov
 * @param operation Ukazovatel na strukturu operacii a argumentov
 * @return Vracia kod chyby alebo uspesneho ukoncenia funkcie
 */
int get_operation(int argc, char **argv, Tcom *operation) {
   char *endptr = NULL;

   if (argc == 1 || argc > MAX_ARGUMENTOV)
      return E_NARGV;

   /* Nacitanie operacie HELP */
   if (argc == 2 && strcmp(argv[1], command[HELP]) == 0) {
      operation->command = HELP;
      return E_OK;
   }

   /* Nacitanie operacie TEST */
   if (argc == 3 && strcmp(argv[1], command[TEST]) == 0 ) {
      operation->command = TEST;
      operation->filename = argv[2];
      return E_OK;
   }

   /* Dalsie nacitanie pracuje s pevnym poctom 5 premennych */
   if (argc != MAX_ARGUMENTOV)
      return E_NARGV;

   /* Nacitanie argumentov rpath, lpath, shortest */
   for (int i = 0; i <= SHORTEST; i++)
      if (strcmp(argv[1], command[i]) == 0)
         operation->command = i;

   /* Nacitanie udajov do struktury */
   if (isinteger(argv[2]) && isinteger(argv[3])) {
      operation->row = strtol(argv[2], &endptr, 10) -1;
      operation->col = strtol(argv[3], &endptr, 10) -1;
      operation->filename = argv[4];
   }

   /* Otestovanie ak niektori udaj nebol zadany spravne */
   if (operation->command == -1 || operation->row < 0 || operation->col < 0)
         return E_NARGV;

   return E_OK;
}

/** Is Border
 * Funkcia zisti, ci v bunke na danom riadku a stlpci
 * sa nachadza zadana stena ak ano vrati True inak False
 * @param map Ukazovatel na strukturu Map
 * @param r Index riadku
 * @param c Index stlpca
 * @param border Hranica bunky, definovana konstantami
 * @return Ak sa nachadza zadana stena vrati True inak False
 */
bool isborder(Map *map, int r, int c, int border) {
   int index = (r * map->cols) + c;

   if ( ((map->cells[index])&border) == border)
      return true;
   return false;
}

/** Operation test
 * Funkcia otestuje, ci existuje subor, ci subor obsahuje ciselne
 * udaje na prvom riadku, ci je matica zadanych rozmerov a ci sa v matici
 * vyskytuju len ciselne udaje a hodnoty mensie ako 7
 * @param operation Struktura, ktora obsahuje argumenty a parametre argumentov
 * @return Vracia kod chyby alebo uspesneho ukoncenia funkcie
 */
int op_test(Tcom *operation, Map *map) {
   FILE *file = NULL;
   int temp_number;
   int count_cells = 0;
   int index = 0;
   int c = 0;


   /* Overenie ci je dostatok pamata a ci sa subor spravne nacital */
   if ((file = fopen(operation->filename, "r")) == NULL)
      return E_INVALID;

   /* Nacitanie poctu riadkov a stlpcov */
   if (fscanf(file, "%d %d", &map->rows, &map->cols) != 2 && map->rows > 0 && map->cols > 0) {
      fclose(file);
      return E_INVALID;
   }

   if (operation->col > map->cols || operation->row > map->rows)
      return E_NARGV;

   count_cells = map->rows * map->cols;
   initialize_map(map);

   /* Zisti sa ci sa v matici nachadzaju len cisla, nacitaju sa udaje do mapy */
   for (int i = 0; i < count_cells; i++) {
      if ((fscanf(file, " %d ", &temp_number)) == 0 || temp_number > MAX_NUMBER || temp_number < 0) {
         fclose(file);
         free_map(map);
         return E_INVALID;
      }
      map->cells[index] = temp_number;
      index++;
   }

   while ((c = fgetc(file)) != EOF)
      return E_INVALID;

   fclose(file);

   /* Porovna ci je dana hrana nastavena v bunkach na riadku,
      aby sa posledny stlpec neporovnaval s hodnotou napravo.
      Porovna sa ci su v neparnych stlpoch nad sebou spolocne steny,
      neporovnava sa posledny riadok. */
   for(int i = 0; i < count_cells; i++) {
      int row = i / map->cols;
      int col = i % map->cols;

      if ( (i+1)%map->cols != 0) { //vynechanie posledneho stlpca
         if (isborder(map,row,col,PRAVY_RAM) != isborder(map,row,col+1,LAVY_RAM)) {
            free_map(map);
            return E_INVALID;
         }
      }

      if ( (count_cells - map->cols) > i ) { //vynechanie posledneho riadku //&& (i&1) == 1 && i > 0
         if ((row%2) == 1 && col%2 == 0) { //porovnanie hodnot nad sebou v neparnom stlpci a riadku
            if (isborder(map,row,col,HORNY_DOLNY_RAM) != isborder(map,row+1,col,HORNY_DOLNY_RAM) ) {
               free_map(map);
               return E_INVALID;
            }
         }
         if ((row%2) == 0 && col%2 == 1 && i !=0) { //porovnanie hodnot nad sebou v parnom stlpci a riadku
            if (isborder(map,row,col,HORNY_DOLNY_RAM) != isborder(map,row+1,col,HORNY_DOLNY_RAM) ) {
               free_map(map);
               return E_INVALID;
            }
         }
      }
   } /* Koniec cyklu */
   return E_VALID;
}

/** Start Border
 * Funkcia vrati hodnotu zaciatocnej hranice podla definovanych
 * specifikacii zo zadania.
 * @param map Ukazoval na strukturu Map, ktora obsahuje mapu
 * @param x Riadok, na ktorom sa ma zacat prechadzat bludisko
 * @param y Stlpec, na ktorom sa ma zacat prechadzat bludisko
 * @param leftright Urcuje, ci sa ma vybrat moznost pre prechod
                    bludiskom pomocou pravej alebo lavej ruky
 * @return Hodnota steny, od ktorej sa ma zacat prechadzat
 */
int start_border(Map *map, int x, int y, int leftright) {
   x+=1; y+=1; /* Prepocet, aby indexi zacinali od 1 */

   /* Pri vstupe zlava na lichem riadku */
   if (y == 1 && x%2 == 1) {
      if (leftright == RPATH)
         return PRAVY_RAM;
      else
         return HORNY_DOLNY_RAM;
   }

   /* Pri vstupu zospodu, stlpec musi byt medzi (0, pocet stlpcov), hranice intervalu tam nepatria */
   if (map->rows == x && (y-1) > 0 && y < map->cols) {
      if (leftright == RPATH)
         return PRAVY_RAM;
      else
         return LAVY_RAM;
   }

   /* Pri vstupe zlava na sudem riadku */
   if (y == 1 && x%2 == 0) {
      if (leftright == RPATH)
         return HORNY_DOLNY_RAM;
      else
         return PRAVY_RAM;
   }

   /* Pri vstupe zhora, stlpec musi byt medzi (0, pocet stlpcov), hranice intervalu tam nepatria */
   if (x == 1 && (y-1) > 0 && y < map->cols) {
      if (leftright == RPATH)
         return LAVY_RAM;
      else
         return PRAVY_RAM;
   }

   /* pri vstupu sprava, pokial ma bunka hornu hranicu */
   if (map->cols == y && isborder(map, x-1, y-1, HORNY_DOLNY_RAM)
       && ((x%2 == 1 && y%2 ==1)||(x%2 == 0 && y%2 == 0)) ) {
      if (leftright == RPATH)
         return HORNY_DOLNY_RAM;
      else
         return LAVY_RAM;
   }

   /* Pri vstupe sprava, pokial ma policko dolnu hranicu(je neparny riadok) */
   if (y == map->cols && isborder(map, x-1, y-1, HORNY_DOLNY_RAM)
      && ((x%2 == 0 && y%2 ==1)||(x%2 == 1 && y%2 == 0)) ) {
      if (leftright == RPATH)
         return LAVY_RAM;
      else
         return HORNY_DOLNY_RAM;
   }
   return 0;
}

/** Select Operation
 * Funkcia vyberie operaciu, ktora sa ma urobit z tabulkou
 * @param operation Ukazoval na strukturu, ktora obsahuje argumenty a hodnoty
 * @param map Ukazoval na strukturu, ktora obsahuje udaje o matici
 * @return
 */
int select_op(Tcom *operation, Map *map) {

   if (HELP == operation->command)
      printf("HELP MSG");
   else if (TEST == operation->command) {

      if (op_test(operation, map) == E_VALID)
         printemsg(E_VALID);
      else
         printemsg(E_INVALID);
   }
   return E_OK;
}

/** Get next border
 * Funkcia vrati hodnotu nasledujucej hrany, ktora sa ma kontrolovat.
 * Funkcia je rozdelena na dve casti pre pravu a lavu ruku, jedotlive
 * hodnoty su odvodene z toho, ze v akom smere sa bude prechadzat trojuholnik.
 * @param row Urcuje riadok, na ktorom sa nachadza trojuholnik
 * @param coll Urcuje stpec, na ktorom sa nachadza trojuholnik
 * @param last_border Urcuje hodnotu predchadzajucej hrany, ktora sa kontrolovala
 * @param leftright Urcuje, ci sa bude robit prechod pomocou pravej/lavej ruky
 * @return Hodnota novej steny, ktora sa ma kontrolovat
 */
int get_next_border(int row, int coll, int last_border, int leftright) {
   int index = 0;
   int walls[POCET_HRAN] = {LAVY_RAM,PRAVY_RAM,HORNY_DOLNY_RAM};

   if (leftright == RPATH) {
      if ( (row%2 == 1 && coll%2 ==0)||(row%2 == 0 && coll%2 == 1) ) {
         walls[0] = PRAVY_RAM;
         walls[1] = LAVY_RAM;
         walls[2] = HORNY_DOLNY_RAM;
       }
      else {
         walls[0] = PRAVY_RAM;
         walls[1] = HORNY_DOLNY_RAM;
         walls[2] = LAVY_RAM;
      }
   }
   else if (leftright == LPATH) {
      if ( (row%2 == 1 && coll%2 ==0)||(row%2 == 0 && coll%2 == 1) ) {
         walls[0] = LAVY_RAM;
         walls[1] = PRAVY_RAM;
         walls[2] = HORNY_DOLNY_RAM;
       }
      else {
         walls[0] = HORNY_DOLNY_RAM;
         walls[1] = PRAVY_RAM;
         walls[2] = LAVY_RAM;
      }
   }

   for (int i = 0; i < POCET_HRAN; i++)
      if (last_border == walls[i])
         index = i+1;
   return walls[(index%POCET_HRAN)];
}

/** Get start border
 * Funkcia vrati hodnotu steny po presune do nasledujucej bunky.
 * @param border Hrana cez, ktoru sa preslo do dalsej bunky
 * @return Vrati hodnotu steny kadial sa do tejto bunky dostalo
 */
int get_start_border(int border) {
   if (border == LAVY_RAM)
      return PRAVY_RAM;
   else if (border == PRAVY_RAM)
      return LAVY_RAM;
   else
      return HORNY_DOLNY_RAM;
}

/** Limit border
 * Funkcia overi, ci su zadane suradnice na okraji bludiska
 * @param map Ukazovatel na strukturu Map, ktora obsahuje udaje o bludisku
 * @param operation Ukazovatel na strukturu, ktora obsahuje argumenty programu
 * @return bool vrati 1 ak su inak 0
 */
bool is_limit_border(Map *map, Tcom *operation) {
   int row = operation->row;
   int coll = operation->col;

   int map_row = map->rows - 1;
   int map_col = map->cols - 1;

   /* Ak je matica 1x1*/
   if (map_row == 0  && map_col == 0 && (row > 0 || coll > 0))
      return 0;

   if (row == 0 && coll >= 0 && coll <= map_col && coll%2 == 0)
      return 1;
   else if (row == map_row && coll >= 0 && coll <= map_col && ((coll%2 == 0 && row%2 == 1) || (coll%2 == 1 && row%2 == 0)) )
      return 1;
   else if (coll == map_col || coll == 0 )
     return 1;

   return 0;
}

/**
 * Funkcia zisti, ci sa nachadza volna stena pri vstupnom policku
 * @param map Ukazovatel na strukturu Map, ktora obsahuje udaje o bludisku
 * @param operation Ukazovatel na strukturu, ktora obsahuje argumenty programu
 * @param leftright Urcuje, ci sa bude robit prechod pomocou pravej/lavej ruky
 */
bool is_input (Tcom *operation, Map *map, int leftright) {
   int next = start_border(map, operation->row, operation->col, leftright);

   next = get_next_border(operation->row, operation->col, next, leftright);
   next = get_next_border(operation->row, operation->col, next, leftright);

   if (isborder(map, operation->row, operation->col, next))
      return false;

   return true;
}

/** Path
 * Funkcia hlada cestu v bludisku pomocou pravej alebo lavej ruky
 * @param map Ukazovatel na strukturu Map, ktora obsahuje udaje o bludisku
 * @param operation Ukazovatel na strukturu, ktora obsahuje argumenty programu
 * @param leftright Urcuje, ci sa bude robit prechod pomocou pravej/lavej ruky
 */
int path(Map *map, Tcom *operation, int leftright) {
   int row = operation->row;
   int coll = operation->col;
   int next_border;

   if (op_test(operation, map) != E_VALID)//overenie ci je bludisko validne
      return E_INVALID;

   if (!is_limit_border(map, operation))//overenie ci su hranice na okraji bludiska
      return E_COORDINATION;

   if (!is_input(operation, map, leftright) ) //Overenie ci je na vstupnom policku volna stena
      return E_BORDER;

   if ((next_border = start_border(map, row, coll, leftright)) == 0)
      return E_START;

   Buffer buf;
   buffer_init(&buf);
   int i;
   for (i = 0;i <= 3*map->rows*map->cols ; i++) { //Cyklus pokial sa nenajde vychod
      buffer_insert_last(&buf, row, coll); //Ulozenie jednotlivych suradnic
      for (int j = 0; j < 3; j++) { //Cyklus, ktory prevadza 3 rotacie suradnic
         if ( !isborder(map, row, coll, next_border) ) { //Kontrola ci je priechodna stena
            if (next_border == LAVY_RAM)
               coll--;
            else if (next_border == PRAVY_RAM)
               coll++;
            else if (next_border == HORNY_DOLNY_RAM
                     && ((row%2 == 1 && coll%2 == 1) || (row%2 == 0 && coll%2 == 0) ) )
               row--;
            else
               row++;

            next_border = get_start_border(next_border); //Vypocet novej zaciatocnej suradnice v bunke, po presune
            next_border = get_next_border(row, coll, next_border, leftright); //Vypocet nasledujucej steny
            break;
         }
         next_border = get_next_border(row, coll, next_border, leftright); //Vypocet nasledujucej steny
      }
      if (coll >= map->cols || row >= map->rows || coll < 0 || row < 0) //Ukoncovacia podmienka prechodu
         break;
   }
   if (i > 2*map->rows*map->cols) {
      while (!buffer_is_empty(&buf)) {
         Titem item;
         buffer_select_data(&buf, &item);
      }
      return E_PATH;
   }

   print_coordinate(&buf); //vypis suradnic
   return E_OK;
}

/** Initialize path
 * Funkcia alokuje pamat na heape pre pole riadkov cesty
 * @param path Ukazoval na strukturu, v ktorej sa ma alokovat pre
 *             bunku cells pamat
 * @return Vracia kod chyby alebo uspesneho ukoncenia funkcie
 */
int init_path(TPath *path) {
   path->cells = (int *) malloc(path->rows * path->cols * sizeof(int));
   if (path->cells == NULL)
      return E_ALLOC;

   path->index = 0;
   path->min_cost = 0;
   return E_OK;
}

/** Initialize path data
 * Funkcia ohodnoti vsetky bunky na -1.
 * @param path Sktura obsahujuca cestu
 */
void init_path_data(TPath *path) {
   init_path(path);
   for (int i = 0; i < path->rows * path->cols; i++)
      path->cells[i] = -1;
}

/** Add number path
 * Funkcia priradi hodnotu cesty, ak nie je bunka mimo rozsah
 * @param path Sktura obsahujuca cestu
 * @param row Riadok, kde sa ma zapisat hodnota cesty
 * @param coll Stlpec, kde sa ma zapisat hodnota cesty
 * @param counter Hodnota cesty, ktora sa zapise
 */
void add_number_path(TPath *path, int row, int coll, int counter) {
   if (coll < path->cols && row < path->rows && coll >= 0 && row >= 0) //Ukoncovacia podmienka prechodu
      path->cells[(row * path->cols) + coll] = counter;
}

/** Is empty map cell
 * Funkcia vrati 1 ak je na zadanom mieste ohodnotena cesta -1.
 * @param path Sktura obsahujuca cestu
 * @param row Riadok, kde sa ma zistit, ci na danom mieste je -1
 * @param coll Stlpec, kde sa ma zistit, ci na danom mieste je -1
 * @return Vrati 1 ak je cesta ohodnotena -1, inak 0
*/
bool is_empty_map_cell(TPath *path, int row, int coll) {
   if (coll < path->cols && row < path->rows && coll >= 0 && row >= 0) //Ukoncovacia podmienka prechodu
      if ( path->cells[row * path->cols + coll] == -1)
         return 1;
   return 0;
}

/////Pomocna funkcia
void print_data_path(TPath *bin) {
   for (int p = 0; p < bin->rows * bin->cols; p++) {
         if (p%(bin->cols) ==0 ) printf("\n");
           printf("%2d ", bin->cells[p]);
         }
}
/////Pomocna funkcia

/** Set cost path
 * Funkcia ohodnoti vsetky cesty - bunky.
 * @param map Struktura obsahujuca bludisko
 * @param path Struktura obsahujuca ohodnotenie bludiska
 * @param operation Struktura obsahujuca argumenty programu
 * @return Vracia kod chyby alebo uspesneho ukoncenia funkcie
 */
int set_cost_path(Map *map, TPath *path, Tcom *operation) {
   int row = operation->row; //nacitanie startovacieho riadku
   int coll = operation->col; //nacitamoe startovacieho stlpca
   int next_border = HORNY_DOLNY_RAM; //definovanie prvej hodnoty, ktora sa ma porovnat
   int counter = 1;

   if (op_test(operation, map) != E_VALID)
      return E_INVALID;

   path->rows = map->rows; //priradenie suradnic, do novej struktury
   path->cols = map->cols; //priradenie suradnic, do novej struktury

   init_path_data(path); //inicializacia struktury

   path->cells[(row * path->cols) + coll] = 1; //nastavenie hodnoty cesty
   int break_point = 1; //pomocna premenna na ukoncenie ohodnotenia cesty

   for (int k = 0; break_point; k++) {
      break_point = 0;

      for (int i = 0; i < map->cols * map->rows; i++) {
         row = i/map->cols; //nanovo vypocitane hodnoty riadku
         coll = i%map->cols; //nanovo vypocitane hodnoty stlpcu

         for (int j = 0; j < 3 && path->cells[i] == counter; j++) {
            if ( !isborder(map, row, coll, next_border) ) {
               if (next_border == LAVY_RAM && is_empty_map_cell(path, row, coll-1))
                  add_number_path(path, row, coll-1, counter+1);
               else if (next_border == PRAVY_RAM && is_empty_map_cell(path, row, coll+1))
                  add_number_path(path, row, coll+1, counter+1);
               else if (next_border == HORNY_DOLNY_RAM && is_empty_map_cell(path, row-1, coll)
                        && ((row%2 == 1 && coll%2 == 1) || (row%2 == 0 && coll%2 == 0)) )
                  add_number_path(path, row-1, coll, counter+1);
               else if (next_border == HORNY_DOLNY_RAM && is_empty_map_cell(path, row+1, coll)
                        && ((row%2 == 0 && coll%2 == 1) || (row%2 == 1 && coll%2 == 0)) )
                  add_number_path(path, row+1, coll, counter+1);
                  break_point++;
            }
            next_border = get_next_border(row, coll, next_border, RPATH);
         }
      } //koniec prehladavania matice
      counter++;
   }
  //print_data_path(path); printf("\n");
   return E_OK;
}

/** Set minimum path
 * @param map Ukazovatel na strukturu, ktora obsahuje udaje o bludisku
 * @param path Ukazovatel na strukturu, Sktura obsahujuca cestu
 * @return Vracia kod chyby alebo uspesneho ukoncenia funkcie
*/
int set_min_path(Map *map, TPath *path) {
   int row, coll;

   int min_cost_path = INT_MAX;
   for (int i = 0; i < path->cols; i++) {
      row = i/map->cols;
      coll = i%map->cols;

      if (path->cells[i] < 0)
         continue;

      if (min_cost_path > path->cells[i] && !isborder(map, row, coll, HORNY_DOLNY_RAM)
          && coll%2 == 0 )
         {
            min_cost_path = path->cells[i];
            path->index = i;
         }
   }

   for (int i = path->cols-1; i < path->cols*path->rows; i +=path->cols) {
      row = i/map->cols;
      coll = i%map->cols;

      if (path->cells[i] < 0)
         continue;

      if (min_cost_path > path->cells[i] && !isborder(map, row, coll, PRAVY_RAM) )
         {
            min_cost_path = path->cells[i];
             path->index = i;
         }
   }

   for (int i = path->cols*(path->rows -1); i < path->cols*path->rows; i++) {
      row = i/map->cols;
      coll = i%map->cols;

      if (path->cells[i] < 0)
         continue;

      if (min_cost_path > path->cells[i] && !isborder(map, row, coll, HORNY_DOLNY_RAM)
          && ((row%2 ==1 && coll%2 == 0) || (row%2 == 0 && coll%2 == 1) ) )
         {
            min_cost_path = path->cells[i];
             path->index = i;
         }
   }

   for (int i = 0; i < path->cols*path->rows; i += path->cols) {
      row = i/map->cols;
      coll = i%map->cols;

      if (path->cells[i] < 0)
         continue;

      if (min_cost_path > path->cells[i] && !isborder(map, row, coll, LAVY_RAM))
         {
            min_cost_path = path->cells[i];
             path->index = i;
         }
   }


   if (min_cost_path == INT_MAX)
      return E_PATH;

   path->min_cost = min_cost_path;
   return E_OK;
}

/** Save coordinate
 * Fumkcia ulozi suradnice do zasobnika typu LIFO
 * @param buf Struktura, ktora obsahuje buffer typu LIFO
 * @param map Ukazovatel na strukturu, ktora obsahuje udaje o bludisku
 * @param path Ukazovatel na strukturu, Sktura obsahujuca cestu
 * @return Vracia kod chyby alebo uspesneho ukoncenia funkcie
 */
int save_coordinate(Buffer *buf, Map *map, TPath *path) {
   int row; int coll;
   int old_row = row = path->index/map->cols;
   int old_coll = coll = path->index%map->cols;
   int next_border = HORNY_DOLNY_RAM; //nastavenie prvej hranice, ktora sa ma kontrolovat

   buffer_insert_data(buf, row, coll); //vlozime suradnicu, od ktorej sa ma hladat cesta

   for (int k = 0; path->min_cost != 1; k++) {
      for (int i = 0;  i < 3; i++) {
         if ( !isborder(map, row, coll, next_border) ) {
            if (next_border == LAVY_RAM)
               coll--;
            else if (next_border == PRAVY_RAM)
               coll++;
            else if (next_border == HORNY_DOLNY_RAM && ((row%2 == 1 && coll%2 == 1) || (row%2 == 0 && coll%2 == 0) ) )
               row--;
            else
               row++;

            /* Ulozenie suradnic bunky, ktora ma mensiu hodnotu ako aktualna bunka*/
            if (coll < path->cols && row < path->rows && coll >= 0 && row >= 0)
               if (path->min_cost > path->cells[row * path->cols + coll]) {
                  buffer_insert_data(buf, row, coll);
                  old_row = row; old_coll = coll;
                  path->min_cost--;
                  break;
               }

            row = old_row;
            coll = old_coll;
            next_border = get_next_border(row, coll, next_border, LPATH);
            continue;
         } //koniec podmienky isborder
         next_border = get_next_border(row, coll, next_border, LPATH);
      }
   }
   return E_OK;
}


int main(int argc, char **argv)
{
   Tcom operation = {
      .command = -1,
      .row = 0,
      .col = 0,
   };

   int error = get_operation(argc, argv, &operation); //premenna uchovava chyby programu

   if (error != E_OK) {
      printemsg(error);
      return error;
   }

   Map map;
   if (HELP == operation.command)
      printf("%s", HELPMSG);
   else if (TEST == operation.command) {
      if (op_test(&operation, &map) == E_VALID)
      {
         printemsg(E_VALID);
          free_map(&map);
      }
      else
         printemsg(E_INVALID);
   }
   else if (RPATH == operation.command) {
      error = path(&map, &operation, RPATH);
      free_map(&map);
   }
   else if (LPATH == operation.command) {
      error = path(&map, &operation, LPATH);
      free_map(&map);
   }
   else if (SHORTEST == operation.command) {
      TPath path;
      if (( error = set_cost_path(&map, &path, &operation)) == E_OK) {
         Buffer buf;

         if ((error = set_min_path(&map, &path)) == E_OK) {
            buffer_init(&buf);
            save_coordinate(&buf, &map, &path);
            print_coordinate(&buf);
         }
         free_path(&path);
         free_map(&map);
      }
   }



   if (error != E_OK) {
      printemsg(error);
      return error;
   }
   return 0;
}
