 /**
  * @author Peter Tisovcik <xtisov00@fit.vutbr.cz
  * @description Projekt 2 - Iteracne vypocty
  */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#define POCET_KOEFICIENTOV 13
#define VYSKA 1.5
#define MAX_VYSKA 100
#define HORNY_INTERVAL 1.4
#define POCET_ITERACI 9
#define PI 3.141592653589

const double tay_citatel[POCET_KOEFICIENTOV] = {
   1, 1, 2, 17, 62, 1382, 21844, 929569, 6404582, 443861162,
  	18888466084, 113927491862, 58870668456604
};
const double tay_menovatel[POCET_KOEFICIENTOV] = {1, 3, 15, 315, 2835, 155925, 6081075,
   638512875, 10854718875, 1856156927625, 194896477400625, 49308808782358125,
   3698160658676859375};

const char *HELPMSG =
  "*****************************************************************\n"
  "* Program: Iteracne vypocty                                     *\n"
  "* Autor:   Peter Tisovcik 2014 - xtisov00                       *\n"
  "* ------------------------------------------------------------- *\n"
  "* Pouzitie:                                                     *\n"
  "*   ./proj2 --help zobrazi sa napoveda                          *\n"
  "*   ./proj2 --tan A N M                                         *\n"
  "*   ./proj2 [-c X] -m A [B]                                     *\n"
  "*                                                               *\n"
  "* --tan A N M                                                   *\n"
  "*   Porovna presnos vypoctu tangensu uhla A pomocou Taylorovho  *\n"
  "*   polynomu a zretazeneho zlomku a vypise absolutnu chybu      *\n"
  "*   medzi matematickou knihovnou. N, M su argumenty, v ktorych  *\n"
  "*   iteracnych vypoctoch ma porovnanie prebiehat.               *\n"
  "*                                                               *\n"
  "* [-c X] -m A [B]                                               *\n"
  "*   [-c X]    argument nastavuje vysku objektu                  *\n"
  "*   -m        vypocita a zmeria vzdialenost                     *\n"
  "*   A         uhol A, udava sa v radianoch, sluzi na vypocet    *\n"
  "*             vzdialenosti                                      *\n"
  "*   [B]       uhol B, udava sa v radianoch, sluzi na vypocet    *\n"
  "*             vysky meraneho objektu                            *\n"
  "*****************************************************************\n";

/* Chybove kody */
enum errcode {
   E_OK,
   E_ARGV,
   E_NUMB,
   E_SIG,
   E_ARGVB,
   E_NARGV,
   E_ARGC,
   E_NNUMB,
   E_HEIGHT,
   E_INF_NAN
};

/* Textova reprezentacia chybovych kodov */
char *errstr[] = {
   "Program prebehol v poriadku.",
   "Bol zadany nespravny pocet parametrov alebo boli zadane chybne.",
   "Nebol zadany podporovany format vstupu argumentu.",
   "Bola zadana zaporna hodnota alebo nula.",
   "Bol zadany argument mimo rozsah pocet iteracii moze byt maximalne 13, vratane.",
   "Bol zadany neplatny rozsah pre zobrazenie iteracii.",
   "Hodnota argumentu -c moze byt do 100, vratane.",
   "Zadany uhol v radianoch, nie je z prveho kvadrantu",
   "Zadana vyska nie je v rozsahu (0;100>.",
   "Zadany uhol je nan alebo inf."
};

/* Kody pre parametre programu */
enum comcode {HELP = 0, TAN, M, C};

/* Textova reprezentacia parametrov programu */
char *command[] = {"--help", "--tan", "-m", "-c"};

/** Print error message
 * Vypise spravu na zaklade chyboveho kodu programu
 * @param error Chybovy kod programu
 */
void printemsg(int error) {
  fprintf(stderr, "%s\n", errstr[error]);
}

/** Moja absolutba hodnota
 * Funkcia prevedie cislo na absolutnu hodnotu
 * @param x Cislo, ktore sa ma previest na absolutnu hodnotu
 * @return Vracia sa absolutna hodnota z cisla x
 */
double my_abs(double x) {
   if (x < 0.0)
      return -x;
   return x;
}

/** Taylorov tangens
 * Vypocita tangens pomocou Taylorovej rady
 * @param x Stupne v radianoch
 * @param n Pocet iteracii
 * @return Hodnota tangensu pre dane x po n iteraciach
 */
double taylor_tan(double x, unsigned int n) {
   /* V pripade, ze pocet iteracii ma byt 1 */
   if (n == 1)
      return x;

   double sum = x;
   unsigned int i = 1; /* Od jedna kvoli tomu, ze ak je zadana jedna iterace
                 tak to osetruje podmienka */

   double xx = x*x; /* x^2 */
   while (i < n) {
      x *= xx;
      sum += (x * tay_citatel[i])/tay_menovatel[i];
      i++;
   }
   return sum;
}

/** Tangens pomocou zretazenych zlomkov
 * Vypocita tangens pomocou zretazenych zlomkov
 * @param x Stupne v radianoch
 * @param n Pocet iteracii
 * @return Hodnota tangensu pre dane x po n iteraciach
 */
double cfrac_tan(double x, unsigned int n) {
   n = n*2-1; /* Vypocet cisla, od ktoreho sa ma zacat pocitat */

   double cf = (x*x) /n;
   if (n == 1)
      return (x);

   for (int i = n-2; i >= 3; i -=2) {
      cf = (x*x)/ (i - cf);
   }
   return (x)/ (1 - cf);
}

/** Comparison
 * Funkcia porovna jednotlive vypocitane tangensy a urci ich absolutne hodnoty
 * z matematickou kniznicou a nasledne sa tieto udaje vypisu, hodnoty sa vypisuju
 * na zaklade zadaneho rozsahu, ktory vypisuje dane iteracie
 * @param x Stupne v radianoch
 * @param a Rozsah odkial sa maju vypisat iteracie vypoctu
 * @param b Rozsah dokial sa maju vypisat iteracie vypoctu
 */
void cmp(double x, unsigned int a, unsigned int b) {
   for (;a <=b; a++) {
      double m_tan = tan(x); /* Matematicky tan */
      double t_tan = taylor_tan(x, a); /* Taylorov tan */
      printf("%d %e %e %e ", a, m_tan, t_tan, my_abs(m_tan - t_tan));

      double cf_tan = cfrac_tan(x, a);
      printf("%e %e \n", cf_tan, my_abs(m_tan - cf_tan));
   }
}

/** Is double
 * Funkcia zisti, ci je cislo typu double a ci je to cislo
 * @param str Ukazovatel na retazec, ktory obsahuje double cislo
 * @return Vrati 1 ak to je cislo a 0 ak to nie je cislo typu double
 */
int isdouble(char *str) {
   char *endptr;
   strtod(str, &endptr);

   if (*endptr != '\0')
      return 0;
   return 1;
}

/** Is integer
 * Funkcia zisti, ci je cislo typu int a ci je to cislo
 * @param str Ukazovatel na retazec, ktory obsahuje int cislo
 * @return Vrati 1 ak to je cislo a 0 ak to nie je cislo typu int
 */
int isinteger(char *str) {
   char *endptr;
   strtol(str, &endptr, 10);

   if (*endptr != '\0')
      return 0;
   return 1;
}

/** Edit rad
 * Funkcia upravy radiany, do prveho kvadrantu ak sa
 * to da
 * @param x Ukazovatel na cislo typu double, ktore sa upravuje
 */
void edit_rad(double *angle) {
   while(*angle > HORNY_INTERVAL)
      *angle -= PI/2;
}

/** Tangens print
 * Funkcia osetri a vypise udaje prisluchajuce tomuto prepinacu
 * @param argv Ukazovatel na pole argumentov, ktore treba spracovat
 *             a overit ci sa s nimi moze pocitat
 * @return Vracia kod uspesneho alebo neuspesneho ukoncenia funkcie
 */
int tan_print(char **argv) {
   /* Overenie ci su argumenty cisla */
   if (isdouble(argv[2]) && isinteger(argv[3]) && isinteger(argv[4]) ) {
      char *endptr;
      double angle_a = strtod(argv[2], &endptr); /* Uhol a */
      int from = strtol(argv[3], &endptr, 10); /* Interval iteracii od */
      int to = strtol(argv[4], &endptr, 10); /* Interval iteracii do */

      if(isnan(angle_a) || isinf(angle_a))
         return E_INF_NAN;

      edit_rad(&angle_a); /* Prepocet uhla */

      if (angle_a <= 0 || angle_a > HORNY_INTERVAL)
         return E_NNUMB;

      /* M < 14 - pocet iteracii */
      if (to > 13)
         return E_ARGVB;

      /* Osetrenie ak M < N */
      if (to < from)
         return E_NARGV;

      /* Overenie ci su cisla kladne */
      if (from > 0 && to > 0)
         cmp(angle_a, from, to);
      else
         return E_SIG; /* Chyba ak je zaporne cislo */
   }
   else
      return E_NUMB; /* Chyba ak je zadany znak alebo chybne cislo */
   return E_OK;
}

/** M A
 * Vypocet vzdialenosti na zaklade uhla
 * @param argv Ukayoval na zoznam argumentov programu
 * @return Vracia kod uspesneho alebo neuspesneho ukoncenia funkcie
 */
int m_a(char **argv) {
   char *endptr;
   double angle_a = strtod(argv[2], &endptr); /* Uhol A */

   if(isnan(angle_a) || isinf(angle_a))
         return E_INF_NAN;

   edit_rad(&angle_a); /* Prepocet uhla */

   if (angle_a <= 0 || angle_a > HORNY_INTERVAL)
      return E_NNUMB;

   double length = VYSKA / cfrac_tan(angle_a, POCET_ITERACI); /* Vypocet dlzky */
   printf("%.10e\n", length); /* Vypis dlzky */

   return E_OK;
}

/** M A B
 * Vypocet vzdialenosti a vysky na zaklade uhlov
 * @param argv Ukazovatel na zoznam argumentov programu
 * @return Vracia kod uspesneho alebo neuspesneho ukoncenia funkcie
 */
int m_ab(char **argv) {
   char *endptr;
   double angle = strtod(argv[2], &endptr); /* Uhol A */

   if(isnan(angle) || isinf(angle))
         return E_INF_NAN;

   edit_rad(&angle); /* Prepocet uhla */

   if (angle <= 0 || angle > HORNY_INTERVAL)
      return E_NNUMB;

   double length = VYSKA / cfrac_tan(angle, POCET_ITERACI); /* Vypocet dlzky */

   angle = strtod(argv[3], &endptr); /* Uhol B */

   if(isnan(angle) || isinf(angle))
         return E_INF_NAN;

   edit_rad(&angle); /* Prepocet uhla */

   if (angle <= 0 || angle > HORNY_INTERVAL)
      return E_NNUMB;

   double height = cfrac_tan(angle, POCET_ITERACI) * length + VYSKA; /* Vypocet vysky */
   printf("%.10e\n%.10e\n", length, height); /* Vypis dlzky a vysky */

   return E_OK;
}

/** C M A
 * Vypocet vzdialenosti na zaklade uhlov a zadanej vysky
 * @param argv Ukazovatel na zoznam argumentov programu
 * @return Vracia kod uspesneho alebo neuspesneho ukoncenia funkcie
 */
int cm_a(char **argv) {
   char *endptr;
   double temp_height = strtod(argv[2], &endptr); /* Ulozenie vysky objektu */

   if (temp_height > MAX_VYSKA || temp_height <= 0)
      return E_HEIGHT;

   double angle = strtod(argv[4], &endptr); /* Uhol A */

   if(isnan(angle) || isinf(angle))
         return E_INF_NAN;

   edit_rad(&angle); /* Prepocet uhla */

   if (angle <= 0 || angle > HORNY_INTERVAL)
      return E_NNUMB;

   double height = temp_height / cfrac_tan(angle, POCET_ITERACI); /* Vypocet dlzky */
   printf("%.10e\n", height); /* Vypis dlzky */

   return E_OK;
}

/** C M A B
 * Vypocet vzdialenosti a vysky na zaklade uhlov a zadanej vysky
 * @param argv Ukazovatel na zoznam argumentov programu
 * @return Vracia kod uspesneho alebo neuspesneho ukoncenia funkcie
 */
int cm_ab(char **argv) {
   char *endptr;
   double temp_height = strtod(argv[2], &endptr); /* Ulozenie novej vysky */

   if(isnan(temp_height) || isinf(temp_height))
         return E_INF_NAN;

   if (temp_height > MAX_VYSKA || temp_height <= 0)
      return E_HEIGHT;

   double angle = strtod(argv[4], &endptr); /* Uhol A */

   if(isnan(angle) || isinf(angle))
         return E_INF_NAN;

   edit_rad(&angle); /* Prepoct uhla */

   if (angle <= 0 || angle > HORNY_INTERVAL)
      return E_NNUMB;

   double length = temp_height / cfrac_tan(angle, POCET_ITERACI); /* Vypocet dlzky */

   angle = strtod(argv[5], &endptr); /* Uhol B */

   if(isnan(angle) || isinf(angle))
         return E_INF_NAN;

   edit_rad(&angle); /* Prepocet uhla */

   if (angle <= 0 || angle > HORNY_INTERVAL)
      return E_NNUMB;

   double height = cfrac_tan(angle, POCET_ITERACI) * length + temp_height; /* Vypocet vysky */
   printf("%.10e\n%.10e\n", length, height); /* Vypis dlzky a vysky */

   return E_OK;
}

/** M print
 * Funkcia vypocita a vypise vzdialenost, popripade aj vysku.
 * Kazda podmienka osetruje jednu moznost vstupu.
 * @param argc Pocet zadanych argumentov
 * @param argv Argumenty programu
 * @return Vracia kod uspesneho alebo neuspesneho ukoncenia funkcie
 */
int m_print(int argc, char *argv[]) {
   int error;

   /* Podmienka pre m A */
   if (argc == 3 && strcmp(argv[1], command[M]) == 0 && isdouble(argv[2]) ) {
      if ( (error = m_a(argv)) != E_OK )
         return error;
   }
   /* Podmienka pre m A B */
   else if (argc == 4 && strcmp(argv[1], command[M]) == 0
            && isdouble(argv[2]) && isdouble(argv[3])) {
      if ( (error = m_ab(argv)) != E_OK )
         return error;
   }
   /* Podmienka pre -c X -m A */
   else if (argc == 5 && strcmp(argv[1], command[C]) == 0 && strcmp(argv[3], command[M]) == 0
            && isdouble(argv[2]) && isdouble(argv[4]) ) {
      if ( (error = cm_a(argv)) != E_OK )
         return error;
   }
   /* Podmienka pre -c X -m A B */
   else if (argc == 6 && strcmp(argv[1], command[C]) == 0 && strcmp(argv[3], command[M]) == 0
            && isdouble(argv[2]) && isdouble(argv[4]) && isdouble(argv[5]) ) {
      if ( (error = cm_ab(argv)) != E_OK )
         return error;
   }
   else
      return E_ARGV;

   return E_OK;
}

//Funkcia vypocita minimalny pocet iteracii, ktore
//su potrebne pre rozne cisla pre vypocet tan v danom
//rozsahu
/*
void accuracy(double from, double to, double accuracy) {
   int min = 0;
   double j = from;

   while(j <= to) {
      for(int i = 0; i < 100; i++) {
         double m_tan = tan(j);
         double cf_tan = cfrac_tan(j, i);
         //printf("%d:: M: %.11f\tC: %.11f ABS: %.11f\n", i, m_tan, cf_tan, my_abs(m_tan - cf_tan));
         if (my_abs(m_tan - cf_tan) < accuracy) {
            min = i;
            break;
         }
      }
      j += 0.01;
   }
   printf("Minimalny pocet iteracii v intervalu <%.10f;%.10f> ", from, to);
   printf("je: %d s presnostou: %e\n", min, accuracy);
}
*/

/** Main
 * Hlavny program
 * @param argc Pocet zadanych argumentov programu
 * @param argv Pole obsahuje argumenty predane programu
 * @return Vracia kod chyby alebo uspesneho ukoncenia programu
 */
int main(int argc, char *argv[])
{
   //accuracy(0, 1.4, 1e-10);

   int error = E_OK;

   /* Ak pocet argumentov nezodpoveda zadaniu */
   if (argc == 1 || argc > 6)
      error = E_ARGV;

   /* Ak sa rovna ---help vypise sa sprava */
   if (argc == 2 && (strcmp(argv[1], command[HELP]) == 0) ){
      printf("%s", HELPMSG);
   }
   /* Volba pre tan */
   else if (argc == 5 && (strcmp(argv[1], command[TAN]) == 0) )
      error = tan_print(argv);

   /* Volba pre m */
   else if (argc >= 3 && argc <= 6) {
      error = m_print(argc, argv);
   }
   else
      error = E_ARGV; /* Chyba ak argumenty nie su zadane spravne */

   if (error != E_OK) {
      printemsg(error);
      return error;
   }

   return EXIT_SUCCESS;
}
