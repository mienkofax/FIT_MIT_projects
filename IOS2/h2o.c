/**
 * @author Peter Tisovcik <xtisov00@fit.vutbr.cz
 * @description 2. projekt do IOS - Building H2O Problem
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <sys/shm.h>
#include <sys/mman.h>
#include <sys/wait.h>
#include <semaphore.h>
#include <sys/stat.h>
#include <unistd.h>
#include <pthread.h>
#include <string.h>
#include <fcntl.h>

#define MIN_TIME_LIMIT 0
#define MAX_TIME_LIMIT 5001
#define POCET_ARGUMENTOV 5
#define FILE_WRITE "h2o.out"
#define OXYGEN 'O'
#define HYDROGEN 'H'
#define POCET_ATOMOV_PROCESU 3
#define POCET_ATOMOV_OXYGENU 2
#define TIME_US 1000

//chybove kody
enum errcode {
    E_OK = 0,
    E_ARGV,
    E_SEMAPHORE_I,
    E_SEMAPHORE_F,
    E_MAP,
    E_FILE,
    E_PROCES
};

//chybove spravy
char *errstr[] = {
    "Ok.",
    "Chybne argumenty.",
    "Chyba pri inicializovani semaforov.",
    "Chyba pri uvolnovani semaforov.",
    "Chyba pri vytvarani mapy.",
    "Chyba pri otvarani suboru.",
    "Chyba pri vytvarani procesov."
};

//stavove kody
enum status_code {
    STARTED = 0,
    WAITING,
    READY,
    BEGIN_BONDING,
    BONDED,
    FINISHED,
};

//stavove spravy
const char *status[] =
{
  "started",
  "waiting",
  "ready",
  "begin bonding",
  "bonded",
  "finished",
};

//struktura pre parametre
typedef struct T_com {
    int oxygen;
    int max_time_oxygen; //kyslik
    int max_time_hydrogen; //vodik
    int max_time_bond;
} T_com;

//funkcia vrati True|False podla toho ci to je alebo nie je cele cislo
int is_int(char *str) {
   char *endptr = NULL;
   strtol(str, &endptr, 10);

   if (*endptr != '\0')
      return E_OK;
   return 1;
}

//overi a ulozi parametre
int get_arg(int argc, char **argv, T_com *arg) {
    if ( argc != POCET_ARGUMENTOV || !is_int(argv[1])  || !is_int(argv[2]) ||
         !is_int(argv[3]) || !is_int(argv[4]) ) {
        return 1;
    }

    //priradenie hodnot
    arg->oxygen = atoi(argv[1]);
    arg->max_time_oxygen = atoi(argv[2]);
    arg->max_time_hydrogen = atoi(argv[3]);
    arg->max_time_bond = atoi(argv[4]);

    //kontrola rozsahu podla zadania
    if ( arg->oxygen <= 0 ) return 1;
    if ( arg->max_time_oxygen < 0 || arg->max_time_oxygen >= MAX_TIME_LIMIT ) return 1;
    if ( arg->max_time_hydrogen < 0 || arg->max_time_hydrogen >= MAX_TIME_LIMIT ) return 1;
    if ( arg->max_time_bond  < 0 || arg->max_time_bond  >= MAX_TIME_LIMIT ) return 1;

    return E_OK;
}

//struktura pre zdielane semafory a premenne
// m - mutex
// q - queue
// c - counter
typedef struct T_shrVar {
    sem_t m_actual;
    sem_t m_qHydrogen;
    sem_t m_qOxygen;
    sem_t m_hydrogen;
    sem_t m_oxygen;
    sem_t m_bond;
    sem_t m_barrier;
    sem_t m_barrier2;
    sem_t m_barrier3;
    sem_t mutex;
    sem_t m_end;
    sem_t m_barrier_end;
    int c_actual;
    int c_barrier;
    int c_end;
    int hydrogen;
    int oxygen;
} T_shrVar;

//inicializacia struktury pre premenne a semafory
int init_shrVar(T_shrVar *shrVar) {
    int err = E_OK;

    if (sem_init(&(shrVar->m_qHydrogen),1,0) != 0) err++;
    if (sem_init(&(shrVar->m_qOxygen),1,0) != 0) err++;
    if (sem_init(&(shrVar->m_actual),1,1) != 0) err++;
    if (sem_init(&(shrVar->m_hydrogen),1,1) != 0) err++;
    if (sem_init(&(shrVar->m_oxygen),1,1) != 0) err++;
    if (sem_init(&(shrVar->m_bond),1,1) != 0) err++;
    if (sem_init(&(shrVar->m_barrier),1,0) != 0) err++;
    if (sem_init(&(shrVar->m_barrier2),1,1) != 0) err++;
    if (sem_init(&(shrVar->m_barrier3),1,1) != 0) err++;
    if (sem_init(&(shrVar->m_end),1,1) != 0) err++;
    if (sem_init(&(shrVar->m_barrier_end),1,0) != 0) err++;
    if (sem_init(&(shrVar->mutex),1,1) != 0) err++;
    shrVar->c_actual = 0;
    shrVar->c_barrier = 0;
    shrVar->c_end = 0;
    shrVar->hydrogen = 0;
    shrVar->oxygen = 0;

    if (err != E_OK)
        return 2;
    return E_OK;
}
//uvolnenie
int free_shrVar(T_shrVar *shrVar) {
    int err = E_OK;

    //uvolnenie jednotlivych semaforov
    if (sem_destroy(&(shrVar->m_qHydrogen)) != 0) err++;
    if (sem_destroy(&(shrVar->m_qOxygen)) != 0) err++;
    if (sem_destroy(&(shrVar->m_actual)) != 0) err++;
    if (sem_destroy(&(shrVar->m_hydrogen)) != 0) err++;
    if (sem_destroy(&(shrVar->m_oxygen)) != 0) err++;
    if (sem_destroy(&(shrVar->m_bond)) != 0) err++;
    if (sem_destroy(&(shrVar->m_barrier)) != 0) err++;
    if (sem_destroy(&(shrVar->m_barrier2)) != 0) err++;
    if (sem_destroy(&(shrVar->m_barrier3)) != 0) err++;
    if (sem_destroy(&(shrVar->m_end)) != 0) err++;
    if (sem_destroy(&(shrVar->m_barrier_end)) != 0) err++;
    if (sem_destroy(&(shrVar->mutex)) != 0) err++;

    if ( munmap(shrVar, sizeof(T_shrVar)) != 0 || err != 0)
        return 2;
    return E_OK;
}


//bond
void bond(int number, int time, char proces, FILE *fw, T_shrVar *shrVar) {
    sem_wait(&(shrVar->m_actual));
    fprintf(fw, "%d\t: %c %d\t: %s\n", ++(shrVar->c_actual), proces, number, status[BEGIN_BONDING]);
    fflush(fw);
    sem_post(&(shrVar->m_actual));
    usleep(1000*(rand() % (time + 1)));
}

//generator kyslika
void oxygen(int counter, T_com *arguments, FILE *fw, T_shrVar *shrVar) {
    sem_wait(&(shrVar->m_actual));
    fprintf(fw, "%d\t: O %d\t: %s\n", ++(shrVar->c_actual), counter, status[STARTED]);
    sem_post(&(shrVar->m_actual));

    sem_wait(&(shrVar->mutex));
    sem_wait(&(shrVar->m_oxygen));
    shrVar->oxygen++;
    sem_post(&(shrVar->m_oxygen));

    if(shrVar->hydrogen >= POCET_ATOMOV_OXYGENU) {
        sem_wait(&(shrVar->m_actual));
        fprintf(fw, "%d\t: O %d\t: %s\n", ++(shrVar->c_actual), counter, status[READY]);
        sem_post(&(shrVar->m_actual));

        sem_post(&(shrVar->m_qHydrogen));
        sem_post(&(shrVar->m_qHydrogen));
        shrVar->hydrogen -= POCET_ATOMOV_OXYGENU;
        sem_post(&(shrVar->m_qOxygen));
        shrVar->oxygen--;
    } else {
        sem_wait(&(shrVar->m_actual));
        fprintf(fw, "%d\t: O %d\t: %s\n", ++(shrVar->c_actual), counter, status[WAITING]);
        sem_post(&(shrVar->m_actual));

        sem_post(&(shrVar->mutex));
    }

    sem_wait(&(shrVar->m_qOxygen));
    bond(counter, arguments->max_time_bond, OXYGEN, fw, shrVar);

    sem_wait(&(shrVar->m_barrier3));
    shrVar->c_barrier++;

    if(shrVar->c_barrier == POCET_ATOMOV_PROCESU) {
        sem_wait(&(shrVar->m_barrier2));
        sem_post(&(shrVar->m_barrier));
    }

    sem_post(&(shrVar->m_barrier3));
    sem_wait(&(shrVar->m_barrier));
    sem_post(&(shrVar->m_barrier));

    sem_wait(&(shrVar->m_actual));
    fprintf(fw, "%d\t: O %d\t: %s\n", ++(shrVar->c_actual), counter, status[BONDED]);
    sem_post(&(shrVar->m_actual));

    sem_wait(&(shrVar->m_barrier3));
    shrVar->c_barrier--;
    if(shrVar->c_barrier == 0) {
        sem_wait(&(shrVar->m_barrier));
        sem_post(&(shrVar->m_barrier2));
    }

    sem_post(&(shrVar->m_barrier3));
    sem_wait(&(shrVar->m_barrier2));
    sem_post(&(shrVar->m_barrier2));

    sem_post(&(shrVar->mutex));
    sem_wait(&(shrVar->m_end));
    shrVar->c_end++;
    sem_post(&(shrVar->m_end));

    if(shrVar->c_end == arguments->oxygen * POCET_ATOMOV_PROCESU)
        sem_post(&(shrVar->m_barrier_end));

    sem_wait(&(shrVar->m_barrier_end));
    sem_post(&(shrVar->m_barrier_end));

    sem_wait(&(shrVar->m_actual));
    fprintf(fw, "%d\t: O %d\t: %s\n", ++(shrVar->c_actual), counter, status[FINISHED]);
    sem_post(&(shrVar->m_actual));

    fclose(fw);
    exit(0);
}

//generator vodika
void hydrogen(int counter, T_com *arguments, FILE *fw, T_shrVar *shrVar) {
    sem_wait(&(shrVar->m_actual));
    fprintf(fw, "%d\t: H %d\t: %s\n", ++(shrVar->c_actual), counter, status[STARTED]);
    sem_post(&(shrVar->m_actual));

    sem_wait(&(shrVar->mutex));
    shrVar->hydrogen++;

    if(shrVar->hydrogen >= POCET_ATOMOV_OXYGENU && shrVar->oxygen >= 1) {
        sem_wait(&(shrVar->m_actual));
        fprintf(fw, "%d\t: H %d\t: %s\n", ++(shrVar->c_actual), counter, status[READY]);
        sem_post(&(shrVar->m_actual));

        sem_post(&(shrVar->m_qHydrogen));
        sem_post(&(shrVar->m_qHydrogen));
        shrVar->hydrogen -= POCET_ATOMOV_OXYGENU;
        sem_post(&(shrVar->m_qOxygen));
        shrVar->oxygen--;
    } else {
        sem_wait(&(shrVar->m_actual));
        fprintf(fw, "%d\t: H %d\t: %s\n", ++(shrVar->c_actual), counter, status[WAITING]);
        sem_post(&(shrVar->m_actual));

        sem_post(&(shrVar->mutex));
    }

    sem_wait(&(shrVar->m_qHydrogen));
    bond(counter, arguments->max_time_bond, HYDROGEN, fw, shrVar);

    sem_wait(&(shrVar->m_barrier3));
    shrVar->c_barrier++;
    if(shrVar->c_barrier == POCET_ATOMOV_PROCESU) {
        sem_wait(&(shrVar->m_barrier2));
        sem_post(&(shrVar->m_barrier));
    }

    sem_post(&(shrVar->m_barrier3));
    sem_wait(&(shrVar->m_barrier));
    sem_post(&(shrVar->m_barrier));

    sem_wait(&(shrVar->m_actual));
    fprintf(fw, "%d\t: H %d\t: %s\n", ++(shrVar->c_actual), counter, status[BONDED]);
    sem_post(&(shrVar->m_actual));

    sem_wait(&(shrVar->m_barrier3));
    shrVar->c_barrier--;

    if(shrVar->c_barrier == 0) {
        sem_wait(&(shrVar->m_barrier));
        sem_post(&(shrVar->m_barrier2));
    }

    sem_post(&(shrVar->m_barrier3));
    sem_wait(&(shrVar->m_barrier2));
    sem_post(&(shrVar->m_barrier2));

    sem_wait(&(shrVar->m_end));
    shrVar->c_end++;
    sem_post(&(shrVar->m_end));

    if(shrVar->c_end == arguments->oxygen * POCET_ATOMOV_PROCESU)
        sem_post(&(shrVar->m_barrier_end));

    sem_wait(&(shrVar->m_barrier_end));
    sem_post(&(shrVar->m_barrier_end));

    sem_wait(&(shrVar->m_actual));
    fprintf(fw, "%d\t: H %d\t: %s\n", ++(shrVar->c_actual), counter, status[FINISHED]);
    sem_post(&(shrVar->m_actual));

    fclose(fw);
    exit(0);
}

int main(int argc, char **argv)
{

    T_shrVar *shrVar = NULL;
    FILE *fw = NULL;
    T_com arguments;

    //overenie a nacitanie arguementov
    if (get_arg(argc, argv, &arguments) != 0) {
        fprintf(stderr, "%s\n", errstr[E_ARGV]);
        return 1;
    }

    //vytvorenie premennych v pamate a osetrenie
    if ((shrVar = mmap(NULL, sizeof(T_shrVar), PROT_READ | PROT_WRITE , MAP_ANONYMOUS | MAP_SHARED,0,0)) ==  MAP_FAILED) {
        fprintf(stderr, "%s\n", errstr[E_MAP]);
        return 2;
    }

    // otvorenie/vytvorenie suboru pre zapis vystupu
    if ( (fw = fopen(FILE_WRITE, "w+")) == NULL ) {
        fprintf(stderr, "%s\n", errstr[E_FILE]);
        return 2;
    }
    setbuf(fw, NULL);

    //inicializacia struktury a osetrenie v pripade chybneho
    //vytvorenia a nasledneho chybneho uvolnenia
    if (init_shrVar(shrVar) != E_OK) {
        fprintf(stderr, "%s\n", errstr[E_SEMAPHORE_I]);

        if (free_shrVar(shrVar) != E_OK)
            fprintf(stderr, "%s\n", errstr[E_SEMAPHORE_F]);

        fclose(fw);
        return 2;
    }

    srand(time(NULL));
    int oxygens = arguments.oxygen;
    int j = 0;

    pid_t generatorH, generatorO;
    pid_t offspringH[POCET_ATOMOV_OXYGENU * oxygens], offspringO[oxygens];

    generatorH = fork();
    if(generatorH == 0) {
        for(j = 1; j <= POCET_ATOMOV_OXYGENU *oxygens; j++) {
            usleep(TIME_US * (rand() % (arguments.max_time_hydrogen + 1)));
            pid_t offspring = fork();

            if(offspring == 0) {
                hydrogen(j, &arguments, fw, shrVar);
            } else if(offspring > 0) {
                offspringH[j-1] = offspring;
            } else {
                fprintf(stderr, "%s\n", errstr[E_PROCES]);
                free_shrVar(shrVar);
                exit(2);
            }
        }
        for(j = 0; j < POCET_ATOMOV_OXYGENU * oxygens; j++)
            waitpid(offspringH[j], NULL, 0);
        exit(0);
    } else if(generatorH > 0) {
        generatorO = fork();
        if(generatorO == 0) {
            for(j = 1; j <= oxygens; j++) {
                usleep(TIME_US * (rand() % (arguments.max_time_oxygen + 1)));
                pid_t offspring = fork();

                if(offspring == 0) {
                    oxygen(j, &arguments, fw, shrVar);
                } else if(offspring > 0) {
                    offspringO[j-1] = offspring;
                } else {
                    fprintf(stderr, "%s\n", errstr[E_PROCES]);
                    free_shrVar(shrVar);
                    exit(2);
                }
            }
            for(j = 0; j < oxygens; j++)
                waitpid(offspringO[j], NULL, 0);
            exit(0);

        } else if(generatorO > 0) {
            waitpid(generatorO, NULL, 0);
            waitpid(generatorH, NULL, 0);

            fclose(fw);
            free_shrVar(shrVar);
            exit(0);
        } else {
             fprintf(stderr, "%s\n", errstr[E_PROCES]);
             free_shrVar(shrVar);
            exit(2);
        }
    }

    free_shrVar(shrVar);
    return 0;
}
