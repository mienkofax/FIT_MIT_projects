#include <iostream>
#include "simlib.h"

#define seconds        * 1
#define minutes        * 60
#define hours          minutes * 60
#define percentages    + 0;

const double SIMULATION_TIME = 72 hours; // Doba behu simulacie
const long ARRIVAL_CUSTOMER[] = {50 seconds, 80 seconds, 100 seconds}; // Prichod zakaznikov
const long PAY_TIME_SHOPPING = 2 minutes; // Cas straveny pri plateni nakupu
const bool ENABLE_REALLOCATION = true;
const short CASH_COUNT = 3; // Pocet pokladni
const short CASH_COUNT_IN_DELICATESSEN = 3; // Pocet pokladni v lahodkach
const short MAX_PEOPLE_IN_CASH_QUEUE = 10;
const short MIN_PEOPLE_IN_CASH_QUEUE = 3;

// Premenne pre realokaciu  v pokladniach
const short CASH_SELLER = 2;
const short CASH_SELLER_IN_DELICATESSEN = 2;


const short COUNT_OF_PERCENT = 100 percentages; // Pocet percent
const short PASTRY_PERCENTAGE = 6 percentages; // Pecivo
const short DRINKS_PERCENTAGE = 6 percentages; // Napoje
const short ALCOHOL_PERCENTAGE = 6 percentages; // Alkoholicke napoje
const short CHEMISTS_PERCENTAGE = 6 percentages; // Drogeria
const short DAIRY_PRODUCTS_PERCENTAGE = 6 percentages; // Mliecne vyrobky
const short FRUITS_VEGETABLES_PERCENTAGE = 6 percentages; // Ovocie a zelenina
const short DELICATESSEN_PERCENTAGE = 6 percentages; // Lahodky

const short BREAD_SLICER_PERCENTAGE = 2 percentages; // Krajac chleba
const short WINE_TAPS_PERCENTAGE = 1 percentages; // Vycap vina
const short RETURNABLE_BOTTLES_PERCENTAGE = 2 percentages; // Automat na vratenie flias

const short BREAD_WAIT_TIME = 20 seconds; // Doba krajania chleba
const short PASTRY_WAIT_TIME = 1 minutes; // Doba stravena v sekcii pecivo
const short WINE_TAPS_WAIT_TIME = 30 seconds; // Doba capovania sudoveho vina
const short RETURNABLE_BOTTLES_WAIT_TIME = 4 minutes; // Doba stravena u automatu na vracanie flias
const short CHEMISTS_WAIT_TIME = 2 minutes; // Doba stravena v sekcii drogerie
const short DAIRY_WAIT_TIME = 4 minutes; // Doba stravena v sekcii mliecne vyrobky
const short FRUITS_VEGETABLES_WAIT_TIME = 30 seconds; // Doba stravena v sekcii s ovocim a zeleninou
const short DELICATESSEN_WAIT_TIME = 40 seconds; // Doba stravena v sekcii lahodky

const short DELICATESSEN_TIMEOUT = 4 minutes; // Doba, za ktoru zakaznik opusti frontu, musi byt vacsia ako DELICATESSEN_WAIT_TIME

Facility FacilityCashes[CASH_COUNT]; // Pokladne
Facility FacilityCashesInDelicatessen[CASH_COUNT_IN_DELICATESSEN]; // Pokladne v lahodkach
Facility FacilityBreadSlicer("Krajac chleba");
Facility FacilityWineTaps("Vycapne zariadenie na sudove vina");
Facility FacilityReturnableBottles("Automat na vracanie fias");

Queue QueueDelicatessen("Fronta lahodok");
Store kosiky(150);

// Pole oznacujuce aktivne pokladne
bool ACTIVE_CASH[CASH_COUNT] = {false}; // Aktivne pokladne pri plateni
bool ACTIVE_CASH_IN_DELICATESSEN[CASH_COUNT_IN_DELICATESSEN] = {false}; // Pokladne v lahodkach

size_t activeCashCount = CASH_SELLER_IN_DELICATESSEN; // Pocet aktivnych pokladni
int arriveState = 0;

/*
 * Timeout po, ktorom zakaznik opusta frontu pri lahodkach.
 */
class Timeout : public Event {
	Process *m_process;

public:
	Timeout(Process *process, double delay):
		m_process(process)
	{
		Activate(Time + delay);
	}

	void Behavior() override
	{
//		m_process->Out();
//		delete m_process;
//		Cancel();
	}
};

/*
 *
 */
class Shopper : public Process {
private:
	int percents;
	int oldPercents;
	size_t m_index = 0;

	void Behavior() override
	{
	Enter(kosiky, 1);
//	std::cout << "take " << Time << std::endl;	
	
	LOOP:
		percents = Random()*COUNT_OF_PERCENT;
		oldPercents = percents;
		// Sekcia peciva
		if (percents <= PASTRY_PERCENTAGE) {
			//std::cout << "pecivo: " << oldPercents << ", ";

			if (Random()*COUNT_OF_PERCENT <= BREAD_SLICER_PERCENTAGE) {
				Seize(FacilityBreadSlicer);
				Wait(Exponential(BREAD_WAIT_TIME));
				Release(FacilityBreadSlicer);
			}
			goto LOOP;
		}
		
		// Sekcia napoje
		percents -= PASTRY_PERCENTAGE;
		if (percents <= DRINKS_PERCENTAGE) {
			Wait(Exponential(PASTRY_WAIT_TIME));
			//std::cout << "napoje: " << oldPercents << ", ";
			goto LOOP;
		}

		// Sekcia alkoholicke napoje
		percents -= DRINKS_PERCENTAGE;
		if (percents <= ALCOHOL_PERCENTAGE) {
			//std::cout << "alkohol: " << oldPercents << ", ";

			if (Random()*COUNT_OF_PERCENT <= WINE_TAPS_PERCENTAGE) {
				Seize(FacilityWineTaps);
				Wait(Exponential(WINE_TAPS_WAIT_TIME));
				Release(FacilityWineTaps);
			}

			if (Random()*COUNT_OF_PERCENT <= RETURNABLE_BOTTLES_PERCENTAGE) {
				Seize(FacilityReturnableBottles);
				Wait(Exponential(RETURNABLE_BOTTLES_WAIT_TIME));
				Release(FacilityReturnableBottles);
			}
			goto LOOP;
		}

		// Sekcia drogeria
		percents -= ALCOHOL_PERCENTAGE;
		if (percents <= CHEMISTS_PERCENTAGE) {
			//std::cout << "drogeria: " << oldPercents << ", ";
			Wait(Exponential(CHEMISTS_WAIT_TIME));
			goto LOOP;
		}

		// Sekcia mliecne vyrobky
		percents -= CHEMISTS_PERCENTAGE;
		if (percents <= DAIRY_PRODUCTS_PERCENTAGE) {
			//std::cout << "mliecne: " << oldPercents << ", ";
			Wait(Exponential(DAIRY_WAIT_TIME));
			goto LOOP;
		}

		// Sekcia ovocie a zelenina
		percents -= DAIRY_PRODUCTS_PERCENTAGE;
		if (percents <= FRUITS_VEGETABLES_PERCENTAGE) {
			//std::cout << "ovocie: " << oldPercents << ", ";
			Wait(Exponential(FRUITS_VEGETABLES_WAIT_TIME));
			goto LOOP;
		}

		// Sekcia s lahodkami
		percents -= FRUITS_VEGETABLES_PERCENTAGE;
		bool close = false;
		if (percents <= DELICATESSEN_PERCENTAGE) {
		REPEAT:
			for (size_t i = 1; i < CASH_COUNT_IN_DELICATESSEN; i++) {
				if (QueueDelicatessen.Length() <= MIN_PEOPLE_IN_CASH_QUEUE
					&& ACTIVE_CASH_IN_DELICATESSEN[i]) {
						m_index = i;
						close = true;
					}

				if (QueueDelicatessen.Length() >= MAX_PEOPLE_IN_CASH_QUEUE
						&& !ACTIVE_CASH_IN_DELICATESSEN[i]) {
					if (activeCashCount * MAX_PEOPLE_IN_CASH_QUEUE > QueueDelicatessen.Length())
						continue;

					ACTIVE_CASH_IN_DELICATESSEN[i] = true;
					activeCashCount++;
					break;
				}

				if (i == CASH_COUNT_IN_DELICATESSEN -1 && close) {
					ACTIVE_CASH_IN_DELICATESSEN[m_index] = false;
					activeCashCount--;
				}
			}

			m_index = -1;
			for (size_t i = 0; i < CASH_COUNT_IN_DELICATESSEN; i++) {
				if (i >= CASH_SELLER_IN_DELICATESSEN && !ENABLE_REALLOCATION)
					break;

				if (!ACTIVE_CASH_IN_DELICATESSEN[i])
					continue;

				if (!FacilityCashesInDelicatessen[i].Busy()) {
					m_index = i;
					break;
				}
			}

			Event *timeout = new Timeout(this, DELICATESSEN_TIMEOUT);
/*			if (m_index != -1) {
				Seize(FacilityCashesInDelicatessen[m_index]);
				delete timeout;
			}
			else {
				QueueDelicatessen.Insert(this);
				Passivate();
				delete timeout;
				goto REPEAT;
			}*/
				Seize(FacilityCashesInDelicatessen[0]);
			//	QueueDelicatessen.Insert(this);
			
delete timeout;
			Wait(Exponential(DELICATESSEN_WAIT_TIME));
			Release(FacilityCashesInDelicatessen[0]);
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
			if (FacilityCashes[i].QueueLen() <= MIN_PEOPLE_IN_CASH_QUEUE
				&& ENABLE_REALLOCATION)
				ACTIVE_CASH[i] = false;

			if (!ACTIVE_CASH[i] && ENABLE_REALLOCATION) {
				if (FacilityCashes[index].QueueLen() >= MAX_PEOPLE_IN_CASH_QUEUE)
					ACTIVE_CASH[i] = true;
			}

			if (FacilityCashes[i].QueueLen() < FacilityCashes[index].QueueLen()
				&& ACTIVE_CASH[i])
				index = i;
		}
	
		Seize(FacilityCashes[index]);
		Wait(Exponential(PAY_TIME_SHOPPING));
		Release(FacilityCashes[index]);
		Leave(kosiky, 1);

	}

	void gotoSection()
	{
		Behavior();
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
//		if (arriveState == 4) {
//			std::cout << "sleep" << Time << std::endl;
//			Activate(Time + 11 hours);
//			arriveState = 0;
//			return;
//		}
//else {

		new Shopper; // Vytvorenie noveho zakaznika
		Activate(Time + Exponential(ARRIVAL_CUSTOMER[arriveState]));}
//}

public:
	Generator()
	{
		Activate();
	}
};

class Gen2 : public Process {
	void Behavior()
	{
		//std::cout << Time << std::endl;
		arriveState = 4;
		//Activate(Time + 2 hours);
		//std::cout << Time << std::endl;

	//	Wait(7 hours);
	//	std::cout << Time << std::endl;

	//	Wait(4 hours);
	//	std::cout << Time << std::endl;

	//	Wait(11 hours);
	//	std::cout << Time << std::endl;
		
		

	}

public:
	Gen2()
	{
		Activate();
	}
};

class Ev : public Process {
	bool m_isNight = false;
	void Behavior()
	{
		if (m_isNight) {
			m_isNight = false;
			arriveState = 0;
			Activate(Time + 11 hours);
		}
		else {
			switch (arriveState) {
				case 0:
					Activate(Time + 2 hours);
					break;
				case 1:
					Activate(Time + 7 hours);
					break;
				case 2:
					Activate(Time + 4 hours);
					m_isNight = true;
					break;
			}
			arriveState++;
		}

	std::cout << "isNight" << m_isNight << ", " <<Time/3600 << "\n";

	}

public:
	Ev()
	{
		Activate();
	}
};

int main()
{
//DebugON ();
	// Alokacia pokladni podla poctu predavaciek a zakaznikov
	for (size_t i = 0; i < CASH_SELLER; i++)
		ACTIVE_CASH[i] = true;

	for (size_t i = 0; i < CASH_SELLER_IN_DELICATESSEN; i++)
		ACTIVE_CASH_IN_DELICATESSEN[i] = true;

	// Nastavenie zdielanej fronty vsetkym obsluznym zariadeniam
	for (size_t i = 0; i < CASH_COUNT_IN_DELICATESSEN; i++) {
		FacilityCashesInDelicatessen[i].SetQueue(QueueDelicatessen);

		if (i < CASH_SELLER_IN_DELICATESSEN)
			ACTIVE_CASH_IN_DELICATESSEN[i] = true;
	}

	for (size_t i = 0; i < CASH_COUNT; i++)
		FacilityCashes[i].SetName("Pokladna");

	RandomSeed(time(NULL)); // inicializacia generatora 
	Init(0, SIMULATION_TIME); // start simulacie

	new Generator(); // Aktivacia generatora
//	new Gen2();
	new Ev();
	Run();
	DebugOFF();
	// Vypis pokladni
	for (size_t i = 0; i < CASH_COUNT; i++)
		FacilityCashes[i].Output();

	// Vypis pokladni v lahodkach
	for (size_t i = 0; i < CASH_COUNT_IN_DELICATESSEN; i++)
		FacilityCashesInDelicatessen[i].Output();

	QueueDelicatessen.Output();

	kosiky.Output();
}

