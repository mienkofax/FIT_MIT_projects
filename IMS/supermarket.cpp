#include <iostream>
#include "simlib.h"

#define seconds        * 1
#define minutes        * 60
#define hours          minutes * 60
#define percentages    + 0;

const long SIMULATION_TIME = 1000 seconds; // Doba behu simulacie
const short CASH_COUNT = 10; // Pocet pokladni

const long ARRIVAL_CUSTOMER = 20 seconds; // Prichod zakaznikov
const long PAY_TIME_SHOPPING = 600 seconds; // Cas straveny pri plateni nakupu

const short PASTRY_PERCENTAGE = 8 percentages; // Pecivo
const short DRINKS_PERCENTAGE = 8 percentages; // Napoje
const short ALCOHOL_PERCENTAGE = 8 percentages; // Alkoholicke napoje
const short CHEMISTS_PERCENTAGE = 8 percentages; // Drogeria
const short DAIRY_PRODUCTS_PERCENTAGE = 30 percentages; // Mliecne vyrobky
const short FRUITS_VEGETABLES_PERCENTAGE = 30 percentages; // Ovocie a zelenina

const short BREAD_SLICER_PERCENTAGE = 8 percentages; // Krajac chleba
const short WINE_TAPS_PERCENTAGE = 8 percentages; // Vycap vina
const short RETURNABLE_BOTTLES_PERCENTAGE = 8 percentages; // Automat na vratenie flias

const short BREAD_WAIT_TIME = 40 seconds; // Doba krajania chleba
const short PASTRY_WAIT_TIME = 2 minutes; // Doba stravena v sekcii pecivo
const short WINE_TAPS_WAIT_TIME = 60 seconds; // Doba capovania sudoveho vina
const short RETURNABLE_BOTTLES_WAIT_TIME = 5 minutes; // Doba stravena u automatu na vracanie flias
const short CHEMISTS_WAIT_TIME = 2 minutes; // Doba stravena v sekcii drogerie
const short DAIRY_WAIT_TIME = 8 minutes; // Doba stravena v sekcii mliecne vyrobky

Facility FacilityCashes[CASH_COUNT]; // Pokladne
Facility FacilityBreadSlicer; // Krajac
Facility FacilityWineTaps; // Vycapne zariadenie na sudove vina
Facility FacilityReturnableBottles; // Automat na vracanie flias

/*
 *
 */
class Shopper : public Process {
	void Behavior() override
	{
		//Wait(30); // Cakanie v sekcii
		// 1-10
		//switch sekcia:
		// Vahy

	LOOP:
		int percents = Random()*100;
		int oldPercents = percents;
		// Sekcia peciva
		if (percents <= PASTRY_PERCENTAGE) {
			std::cout << "pecivo: " << oldPercents << ", ";

			if (Random()*100 <= BREAD_SLICER_PERCENTAGE) {
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
			std::cout << "napoje: " << oldPercents << ", ";
			goto LOOP;
		}

		// Sekcia alkoholicke napoje
		percents -= DRINKS_PERCENTAGE;
		if (percents <= ALCOHOL_PERCENTAGE) {
			std::cout << "alkohol: " << oldPercents << ", ";

			if (Random()*100 <= WINE_TAPS_PERCENTAGE) {
				Seize(FacilityWineTaps);
				Wait(WINE_TAPS_WAIT_TIME);
				Release(FacilityWineTaps);
			}

			if (Random()*100 <= RETURNABLE_BOTTLES_PERCENTAGE) {
				Seize(FacilityReturnableBottles);
				Wait(RETURNABLE_BOTTLES_WAIT_TIME);
				Release(FacilityReturnableBottles);
			}
			goto LOOP;
		}

		// Sekcia drogeria
		percents -= ALCOHOL_PERCENTAGE;
		if (percents <= CHEMISTS_PERCENTAGE) {
			std::cout << "drogeria: " << oldPercents << ", ";
			Wait(CHEMISTS_WAIT_TIME);
			goto LOOP;
		}

		// Sekcia mliecne vyrobky
		percents -= CHEMISTS_PERCENTAGE;
		if (percents <= DAIRY_PRODUCTS_PERCENTAGE) {
			std::cout << "mliecne: " << oldPercents << ", ";
			Wait(DAIRY_WAIT_TIME);
			goto LOOP;
		}

		// Sekcia ovocie a zelenina
		percents -= DAIRY_PRODUCTS_PERCENTAGE;
		if (percents <= FRUITS_VEGETABLES_PERCENTAGE) {
			std::cout << "ovocie: " << oldPercents << ", ";
			goto LOOP;
		}

		std::cout << "koniec: " << oldPercents << "\n";

		int index = 0;
		for (size_t i = 1; i < CASH_COUNT; i++) {
			if (FacilityCashes[i].QueueLen() < FacilityCashes[index].QueueLen())
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
	RandomSeed(time(NULL)); // inicializacia generatora 
	Init(0, SIMULATION_TIME); // start simulacie

	new Generator(); // Aktivacia generatora

	Run();

	//FacilityCashes[0].Output();
	//FacilityCashes[1].Output();

	std::cout << "end\n";
}
