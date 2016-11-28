#include <iostream>
#include "simlib.h"

#define seconds        * 1
#define minutes        * 60
#define hours          minutes * 60
#define percentages    + 0;

const short COUNT_OF_PERCENT = 100 percentages; // Pocet percent
const long SIMULATION_TIME = 1000 seconds; // Doba behu simulacie
const short CASH_COUNT = 10; // Pocet pokladni
const long ARRIVAL_CUSTOMER = 20 seconds; // Prichod zakaznikov
const long PAY_TIME_SHOPPING = 600 seconds; // Cas straveny pri plateni nakupu
const bool ENABLE_REALLOCATION = false;
const short CASH_COUNT_IN_DELICATESSEN = 4; // Pocet pokladni v lahodkach
const short MAX_PEOPLE_IN_CASH_QUEUE = 10;
const short MIN_PEOPLE_IN_CASH_QUEUE = 3;

// Premenne pre realokaciu  v pokladniach
const short CASH_SELLER = 2;
const short CASH_SELLER_IN_DELICATESSEN = 2;


const short PASTRY_PERCENTAGE = 8 percentages; // Pecivo
const short DRINKS_PERCENTAGE = 8 percentages; // Napoje
const short ALCOHOL_PERCENTAGE = 8 percentages; // Alkoholicke napoje
const short CHEMISTS_PERCENTAGE = 8 percentages; // Drogeria
const short DAIRY_PRODUCTS_PERCENTAGE = 30 percentages; // Mliecne vyrobky
const short FRUITS_VEGETABLES_PERCENTAGE = 20 percentages; // Ovocie a zelenina
const short DELICATESSEN_PERCENTAGE = 8 percentages; // Lahodky

const short BREAD_SLICER_PERCENTAGE = 8 percentages; // Krajac chleba
const short WINE_TAPS_PERCENTAGE = 8 percentages; // Vycap vina
const short RETURNABLE_BOTTLES_PERCENTAGE = 8 percentages; // Automat na vratenie flias

const short BREAD_WAIT_TIME = 40 seconds; // Doba krajania chleba
const short PASTRY_WAIT_TIME = 2 minutes; // Doba stravena v sekcii pecivo
const short WINE_TAPS_WAIT_TIME = 60 seconds; // Doba capovania sudoveho vina
const short RETURNABLE_BOTTLES_WAIT_TIME = 5 minutes; // Doba stravena u automatu na vracanie flias
const short CHEMISTS_WAIT_TIME = 2 minutes; // Doba stravena v sekcii drogerie
const short DAIRY_WAIT_TIME = 8 minutes; // Doba stravena v sekcii mliecne vyrobky
const short FRUITS_VEGETABLES_WAIT_TIME = 30 seconds; // Doba stravena v sekcii s ovocim a zeleninou
const short DELICATESSEN_WAIT_TIME = 2 minutes; // Doba stravena v sekcii lahodky

Facility FacilityCashes[CASH_COUNT]; // Pokladne
Facility FacilityCashesInDelicatessen[CASH_COUNT_IN_DELICATESSEN]; // Pokladne v lahodkach
Facility FacilityBreadSlicer; // Krajac
Facility FacilityWineTaps; // Vycapne zariadenie na sudove vina
Facility FacilityReturnableBottles; // Automat na vracanie flias


// Pole oznacujuce aktivne pokladne
bool ACTIVE_CASH[CASH_COUNT] = {false}; // Aktivne pokladne pri plateni
bool ACTIVE_CASH_IN_DELICATESSEN[CASH_COUNT_IN_DELICATESSEN] = {false}; // Pokladne v lahodkach

/*
 *
 */
class Shopper : public Process {
private:
	int percents;
	int oldPercents;

	void Behavior() override
	{
	LOOP:
		percents = Random()*COUNT_OF_PERCENT;
		oldPercents = percents;
		// Sekcia peciva
		if (percents <= PASTRY_PERCENTAGE) {
			//std::cout << "pecivo: " << oldPercents << ", ";

			if (Random()*COUNT_OF_PERCENT <= BREAD_SLICER_PERCENTAGE) {
				Seize(FacilityBreadSlicer);
				Wait(BREAD_WAIT_TIME);
				Release(FacilityBreadSlicer);
			}
			goto LOOP;
		}
		
		// Sekcia napoje
		percents -= PASTRY_PERCENTAGE;
		if (percents <= DRINKS_PERCENTAGE) {
			Wait(PASTRY_WAIT_TIME);
			//std::cout << "napoje: " << oldPercents << ", ";
			goto LOOP;
		}

		// Sekcia alkoholicke napoje
		percents -= DRINKS_PERCENTAGE;
		if (percents <= ALCOHOL_PERCENTAGE) {
			//std::cout << "alkohol: " << oldPercents << ", ";

			if (Random()*COUNT_OF_PERCENT <= WINE_TAPS_PERCENTAGE) {
				Seize(FacilityWineTaps);
				Wait(WINE_TAPS_WAIT_TIME);
				Release(FacilityWineTaps);
			}

			if (Random()*COUNT_OF_PERCENT <= RETURNABLE_BOTTLES_PERCENTAGE) {
				Seize(FacilityReturnableBottles);
				Wait(RETURNABLE_BOTTLES_WAIT_TIME);
				Release(FacilityReturnableBottles);
			}
			goto LOOP;
		}

		// Sekcia drogeria
		percents -= ALCOHOL_PERCENTAGE;
		if (percents <= CHEMISTS_PERCENTAGE) {
			//std::cout << "drogeria: " << oldPercents << ", ";
			Wait(CHEMISTS_WAIT_TIME);
			goto LOOP;
		}

		// Sekcia mliecne vyrobky
		percents -= CHEMISTS_PERCENTAGE;
		if (percents <= DAIRY_PRODUCTS_PERCENTAGE) {
			//std::cout << "mliecne: " << oldPercents << ", ";
			Wait(DAIRY_WAIT_TIME);
			goto LOOP;
		}

		// Sekcia ovocie a zelenina
		percents -= DAIRY_PRODUCTS_PERCENTAGE;
		if (percents <= FRUITS_VEGETABLES_PERCENTAGE) {
			//std::cout << "ovocie: " << oldPercents << ", ";
			Wait(FRUITS_VEGETABLES_WAIT_TIME);
			goto LOOP;
		}

		// Sekcia s lahodkami
		percents -= DELICATESSEN_PERCENTAGE;
		if (percents <= FRUITS_VEGETABLES_PERCENTAGE) {
			//std::cout << "lahodky: " << oldPercents << ", ";
			goto LOOP;
		}

		//std::cout << "koniec: " << oldPercents << "\n";

		/*
		 * Obsadenie pokladne podla dlzky fronty. Obsadi sa pokladna, ktora
		 * ma ako prva najmensiu frontu.
		 * Ak je pokladna aktivna a je vybrana ako pokladna s najmensou
		 * frontou ale obsahuje viacej zakaznikov vo fronte ako je dovolene
		 * Pokusi sa otvorit dalsiu pokladnu.
		 * Ak pokladna obsahuje menej zakaznikov ako je dovolene zavrie sa
		 * pokladna.
		 */
		int index = 0;
		for (size_t i = 1; i < CASH_COUNT; i++) {
			if (FacilityCashes[i].QueueLen() < MIN_PEOPLE_IN_CASH_QUEUE
				&& ENABLE_REALLOCATION)
				ACTIVE_CASH[i] = false;

			if (!ACTIVE_CASH[i] && ENABLE_REALLOCATION) {
				if (FacilityCashes[index].QueueLen() > MAX_PEOPLE_IN_CASH_QUEUE)
					ACTIVE_CASH[i] = true;
			}

			if (FacilityCashes[i].QueueLen() < FacilityCashes[index].QueueLen()
				&& ACTIVE_CASH[i])
				index = i;
		}
	
		Seize(FacilityCashes[index]);
		Wait(Exponential(PAY_TIME_SHOPPING));
		Release(FacilityCashes[index]);
	}

public:
	Shopper()
	{
		Activate();
	}
};

/*
 * Generator pre prichod zakaznikov.
 */
class Generator : public Event {
	void Behavior()
	{
		new Shopper; // Vytvorenie noveho zakaznika

		Activate(Time + Exponential(ARRIVAL_CUSTOMER));
	}

public:
	Generator()
	{
		Activate();
	}
};

int main()
{
	// Alokacia pokladni podla poctu predavaciek a zakaznikov
	for (size_t i = 0; i < CASH_SELLER; i++)
		ACTIVE_CASH[i] = true;

	for (size_t i = 0; i < CASH_SELLER_IN_DELICATESSEN; i++)
		ACTIVE_CASH_IN_DELICATESSEN[i] = true;

	RandomSeed(time(NULL)); // inicializacia generatora 
	Init(0, SIMULATION_TIME); // start simulacie

	new Generator(); // Aktivacia generatora

	Run();

	for (size_t i = 0; i < CASH_COUNT; i++)
		FacilityCashes[i].Output();
}
