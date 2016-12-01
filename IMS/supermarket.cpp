/**
 * @file supermarket.cpp
 * @author Nečasová Klára <xnecas24@stud.fit.vutbr.cz>
 * @author Peter Tisovčík <xtisov00@stud.fit.vutbr.cz>
 * @date 30.11.2016
 */

#include <iostream>
#include "simlib.h"

using std::string;
using std::to_string;
using std::cout;
using std::endl;

#define seconds        * 1
#define minutes        * 60
#define hours          minutes * 60
#define percentages    + 0;

const double SIMULATION_TIME = 68 hours + 20 minutes; // Doba behu simulacie
const long ARRIVAL_CUSTOMER[] = {50 seconds, 40 seconds, 30 seconds}; // Prichod zakaznikov
const double PAY_TIME_SHOPPING = 2 minutes; // Cas straveny pri plateni nakupu
const bool ENABLE_REALLOCATION = false; // Povolenie zmeny kapacity pokladni na zaklade vytazenia
const short CASH_COUNT = 4; // Pocet pokladni
const short CASH_COUNT_IN_DELICATESSEN = 4; // Pocet pokladni v lahodkach
const short WINE_TAPS_COUNT = 12; // Pocet vycapov
const short MAX_PEOPLE_IN_CASH_QUEUE = 8; // Maximalny pocet zakaznikov vo fronte, pri prekroceni tejto hodnoty sa otvori nova pokladna (len pri ENABLE_REALLOCATION = true)
const short MIN_PEOPLE_IN_CASH_QUEUE = 3; // Minimalny pocet zakaznikov vo fronte, pri prekroceni tejto hodnoty sa zatvori poklad (len pri ENABLE_REALLOCATION = true

// Premenne pre realokaciu  v pokladniach
const short CASH_SELLER = 2; // Pocet aktivnych pokladni (len pri ENABLE_REALLOCATION = false)
const short CASH_SELLER_IN_DELICATESSEN = 1; // Pocet aktivnych pokladni v lahodkach (Len pri ENABLE_REALLOCATION = false)

// Percentualne vyjadrenie prichodu do jednotlivych sekcii v obchode
const double COUNT_OF_PERCENT = 100 percentages; // Pocet percent
const double PASTRY_SECTION_PERCENTAGE = 8 percentages; // Pecivo
const double DRINKS_SECTION_PERCENTAGE = 3 percentages; // Napoje
const double ALCOHOL_SECTION_PERCENTAGE = 3 percentages; // Alkoholicke napoje
const double CHEMISTS_SECTION_PERCENTAGE = 3 percentages; // Drogeria
const double DAIRY_PRODUCTS_SECTION_PERCENTAGE = 5 percentages; // Mliecne vyrobky
const double FRUITS_VEGETABLES_SECTION_PERCENTAGE = 5 percentages; // Ovocie a zelenina
const double DELICATESSEN_SECTION_PERCENTAGE = 5 percentages; // Lahodky

// Percentualne vyjadrenie nakupu v jednotlivych sekciach
const double PASTRY_PERCENTAGE = 12 percentages; // Pecivo
const double ALCOHOL_PERCENTAGE = 8 percentages; // Vyber alkoholu

// Percentualne vyjadrenie vyuzitia obsluznych liniek
const double BREAD_SLICER_PERCENTAGE = 1 percentages; // Krajac chleba
const double WINE_TAPS_PERCENTAGE = 1 percentages; // Vycap vina
const double RETURNABLE_BOTTLES_PERCENTAGE = 2 percentages; // Automat na vratenie flias
const double TAKE_BASKET_PERCENTAGE = 75 percentages; // Zabratie kosika

// Doba stravena v obsluznych linkach 
const short BREAD_WAIT_TIME = 20 seconds; // Doba krajania chleba
const short WINE_TAPS_WAIT_TIME = 30 seconds; // Doba capovania sudoveho vina
const short RETURNABLE_BOTTLES_WAIT_TIME = 4 minutes; // Doba stravena u automatu na vracanie flias
const short INTERRUPT_WAIT_TIME = 40 seconds; // Doba vyriesenia problemu na pokladne

// Doba stravena v jednotlivych sekciach
const short PASTRY_WAIT_TIME = 1 minutes; // Doba stravena v sekcii pecivo
const short DRINKS_WAIT_TIME = 1 minutes; // Doba stravena v sekci napoje
const short DAIRY_WAIT_TIME = 3 minutes; // Doba stravena v sekcii mliecne vyrobky
const short CHEMISTS_WAIT_TIME = 2 minutes; // Doba stravena v sekcii drogerie
const short FRUITS_VEGETABLES_WAIT_TIME = 60 seconds; // Doba stravena v sekcii s ovocim a zeleninou
const short DELICATESSEN_WAIT_TIME = 120 seconds; // Doba stravena v sekcii lahodky
const short ALCOHOL_WAIT_TIME = 30 seconds; // Doba stravena v sekcii alkohol

const short DELICATESSEN_TIMEOUT = 5 minutes; // Doba, za ktoru zakaznik opusti frontu, musi byt vacsia ako DELICATESSEN_WAIT_TIME

const short SELLER_MANAGER_COMMING_TME = 30 seconds; // Doba, za ktoru pride veduci

// Doba dennych rezimov
const double MOD_MORNING = 2 hours;
const double MOD_NOON = 7 hours;
const double MOD_AFTERNOON = 4 hours;
const double MOD_NIGHT = 11 hours;

// Obsluzne zariadenia
Facility FacilityCashes[CASH_COUNT]; // Pokladne
Facility FacilityCashesInDelicatessen[CASH_COUNT_IN_DELICATESSEN]; // Pokladne v lahodkach
Facility FacilityBreadSlicer("Krajac chleba");
Facility FacilityWineTaps[WINE_TAPS_COUNT]; // Vycapne zariadenie na sudove vina
Facility FacilityReturnableBottles("Automat na vratenie flias");
Store StoreBaskets("Sklad kosikov", 150);

// Spolocna fronta pre pokladne pri lahodkach 
Queue QueueDelicatessen("Fronta lahodok");

// Pole oznacujuce aktivne pokladne
bool ACTIVE_CASH[CASH_COUNT] = {false}; // Aktivne pokladne pri plateni
bool ACTIVE_CASH_IN_DELICATESSEN[CASH_COUNT_IN_DELICATESSEN] = {false}; // Aktivne pokladne v lahodkach

size_t activeCashCount = CASH_SELLER_IN_DELICATESSEN; // Pocet aktivnych pokladni v lahodkach
int arriveState = 0; // Denni rezim prevadzky
bool interrupt = false; // Prerusenie pri ukoncni otvaracej doby, aby zakaznici zaplaili pri pokladni a opustili obchod

/*
 * Denne rezimy
 */
enum {
	MORNING,
	NOON,
	AFTERNOON,
	NIGHT,
};

/*
 * Timeout po, ktorom zakaznik opusta frontu pri lahodkach.
 */
class Timeout : public Event {
	Process *m_process;

public:
	Timeout(Process *process, double delay):
		m_process(process)
	{
		Activate(Time + Uniform(1, delay));
	}

	void Behavior() override
	{
		m_process->Out();
		delete m_process;
		Cancel();
	}
};

/*
 * Zakaznik, ktory vykonava nakup. Moze s urcitou pravdepodobnostou
 * pri vstupe zobrat kosik. S urcitou pravdepodobnostou vstupuje do
 * jednotlivych sekcii a ak sa v sekcii nachadza obsluzna linka, s urcitou
 * pravdepodobnostou ju vyuzije.
 */
class Shopper : public Process {
private:
	int m_percents;
	int m_index = 0;
	bool m_close = false;
	bool m_basket = false;

	void Behavior() override
	{
		// Zabratie kosika pri vstupe do obchodu
		if (Random()*COUNT_OF_PERCENT <= TAKE_BASKET_PERCENTAGE) {
			Enter(StoreBaskets, 1);
			m_basket = true;
		}
	
	LOOP:
		m_percents = Random()*COUNT_OF_PERCENT;

		// Sekcia peciva
		if (m_percents <= PASTRY_SECTION_PERCENTAGE && !interrupt) {
			// Vyber peciva
			if (Random()*COUNT_OF_PERCENT <= PASTRY_PERCENTAGE)
				Wait(Exponential(PASTRY_WAIT_TIME));

			// Krajanie chleba
			if (Random()*COUNT_OF_PERCENT <= BREAD_SLICER_PERCENTAGE) {
				Seize(FacilityBreadSlicer);
				Wait(Exponential(BREAD_WAIT_TIME));
				Release(FacilityBreadSlicer);
			}

			if (!interrupt)
				goto LOOP;
		}
		
		// Sekcia napoje
		m_percents -= PASTRY_SECTION_PERCENTAGE;
		if (m_percents <= DRINKS_SECTION_PERCENTAGE && !interrupt) {
			Wait(Exponential(DRINKS_WAIT_TIME));

			if (!interrupt)
				goto LOOP;
		}

		// Sekcia alkoholicke napoje
		m_percents -= DRINKS_SECTION_PERCENTAGE;
		if (m_percents <= ALCOHOL_SECTION_PERCENTAGE && !interrupt) {
			// Vyber nejakeho alkohoolu
			if (Random()*COUNT_OF_PERCENT <= ALCOHOL_PERCENTAGE)
				Wait(ALCOHOL_WAIT_TIME);

			//Obsuzna linka na vycap vina
			if (Random()*COUNT_OF_PERCENT <= WINE_TAPS_PERCENTAGE) {
				m_index = Uniform(0, WINE_TAPS_COUNT);

				Seize(FacilityWineTaps[m_index]);
				Wait(Exponential(WINE_TAPS_WAIT_TIME));
				Release(FacilityWineTaps[m_index]);
			}

			// Obsluzna linka na vratenie flias
			if (Random()*COUNT_OF_PERCENT <= RETURNABLE_BOTTLES_PERCENTAGE) {
				Seize(FacilityReturnableBottles);
				Wait(Exponential(RETURNABLE_BOTTLES_WAIT_TIME));
				Release(FacilityReturnableBottles);
			}

			if (!interrupt)
				goto LOOP;
		}

		// Sekcia drogeria
		m_percents -= ALCOHOL_SECTION_PERCENTAGE;
		if (m_percents <= CHEMISTS_SECTION_PERCENTAGE && !interrupt) {
			Wait(Exponential(CHEMISTS_WAIT_TIME));

			if (!interrupt)
				goto LOOP;
		}

		// Sekcia mliecne vyrobky
		m_percents -= CHEMISTS_SECTION_PERCENTAGE;
		if (m_percents <= DAIRY_PRODUCTS_SECTION_PERCENTAGE && !interrupt) {
			Wait(Exponential(DAIRY_WAIT_TIME));

			if (!interrupt)
				goto LOOP;
		}

		// Sekcia ovocie a zelenina
		m_percents -= DAIRY_PRODUCTS_SECTION_PERCENTAGE;
		if (m_percents <= FRUITS_VEGETABLES_SECTION_PERCENTAGE && !interrupt) {
			Wait(Exponential(FRUITS_VEGETABLES_WAIT_TIME));

			if (!interrupt)
				goto LOOP;
		}

		// Sekcia s lahodkami
		m_percents -= FRUITS_VEGETABLES_SECTION_PERCENTAGE;
		if (m_percents <= DELICATESSEN_SECTION_PERCENTAGE && !interrupt) {
		REPEAT:
			/*
			 * Realokacia pokladni ak je vo fronte nadbytok alebo nedostatok
			 * zakaznikov. Plati len pri ENABLE_REALLOCATION = true;
			 */
			for (size_t i = 1; i < CASH_COUNT_IN_DELICATESSEN; i++) {
				// Ak je vo fronte nedostatok ludi, pokladna sa uzavrie
				if (QueueDelicatessen.Length() <= MIN_PEOPLE_IN_CASH_QUEUE
					&& ACTIVE_CASH_IN_DELICATESSEN[i]) {
						m_index = i;
						m_close = true;
					}

				// Ak je vo fronte nadbytok ludi, poklad sa otvori
				if (QueueDelicatessen.Length() >= MAX_PEOPLE_IN_CASH_QUEUE
						&& !ACTIVE_CASH_IN_DELICATESSEN[i]) {
					/*
					 * Otvorenie novej pokladne len v pripade, ze 
					 * pocet otvorenych pokladni krat maximalny pocet zakaznikov
					 * vo fronte je vacsi ako aktualna dlzka spolocnej fronty.
					 */
					if (activeCashCount * MAX_PEOPLE_IN_CASH_QUEUE > QueueDelicatessen.Length())
						continue;

					ACTIVE_CASH_IN_DELICATESSEN[i] = true;
					activeCashCount++;
					break;
				}

				// Zatvorenie pokladne
				if (i == (CASH_COUNT_IN_DELICATESSEN - 1) && m_close) {
					ACTIVE_CASH_IN_DELICATESSEN[m_index] = false;
					activeCashCount--;
				}
			}

//			ACTIVE_CASH_IN_DELICATESSEN[0] = true;

			/*
			 * Vyber volnej pokladne v lahodkach.
			 */
			m_index = -1;
			for (size_t i = 0; i < CASH_COUNT_IN_DELICATESSEN; i++) {
				/*
				 * A nie je povolena realokacia pokladni, tak je mozne pristupit
				 * len k pokladniam, ktore obsluhuju predavaci.
				 */
				if (i >= CASH_SELLER_IN_DELICATESSEN && !ENABLE_REALLOCATION)
					break;

				// Zakaznik moze pristupit len k aktivnym pokladniam
				if (!ACTIVE_CASH_IN_DELICATESSEN[i])
					continue;

				// Zakaznik moze pristupit len k pokladnei, ktora je volna
				if (!FacilityCashesInDelicatessen[i].Busy()) {
					m_index = i;
					break;
				}
			}

			// Maximalna doba, po ktoru zakaznik caka vo fronte
			Event *timeout = new Timeout(this, DELICATESSEN_TIMEOUT);

			/*
			 * Pokial nie je ziadna pokladna volna, tak je zakaznik umiestneny
			 * do fronty. Inak obsadi obsluznu linku a po obsluzeni ju uvolni.
			 */
			if (m_index == -1) {
				delete timeout;
				QueueDelicatessen.Insert(this);
				Passivate(); // Proces je presureny
				goto REPEAT; // Po aktivacii proces sa pokracuje na tomto mieste
			}
			else
				Seize(FacilityCashesInDelicatessen[m_index]);	

			delete timeout;
			Wait(Exponential(DELICATESSEN_WAIT_TIME));
			Release(FacilityCashesInDelicatessen[m_index]);

			/*
			 * Spustenie procesu sa naplanuje na aktualny cas a pokracuje
			 * v mieste, kde bol preruseny(Passivate()).
			 */
			if (QueueDelicatessen.Length() > 0) {
				(QueueDelicatessen.GetFirst())->Activate();
			}

			if (!interrupt)
				goto LOOP;
		}

		// Priorita zakaznika, ktory ma malo poloziek a chce byt obsluseny skor
		if (Random()*COUNT_OF_PERCENT <= 1)
			Priority = 1;

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

		if (m_basket)
			Leave(StoreBaskets, 1);
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
class GenShopperArrival : public Event {
	void Behavior() override
	{
		if (arriveState == NIGHT) {
			/*
			 * Nastavenie uspania po dobu nocneho rezimu a nastavenie
			 * casu prebudenia na celu hodinu.
			 */
			Activate(Time + MOD_NIGHT - long(Time)%(1 hours));
			return;
		}

		// Vytvorenie noveho zakaznika
		new Shopper;

		// Nastavenie vygenerovania dalsieho zakazika, znovu zavolanie udalosti
		Activate(Time + Exponential(ARRIVAL_CUSTOMER[arriveState]));
	}

public:
	GenShopperArrival()
	{
		Activate();
	}
};

/*
 * Riadenie denneho rezimu obchodu.
 */
class GenDailyMode : public Process {
	void Behavior() override
	{
	RET:
		interrupt = false;
		arriveState = MORNING;
		Wait(MOD_MORNING);

		arriveState = NOON;
		Wait(MOD_NOON);

		arriveState = AFTERNOON;
		Wait(MOD_AFTERNOON);

		arriveState = NIGHT;
		interrupt = true;
		Wait(MOD_NIGHT);
		
		goto RET;
	}

public:
	GenDailyMode()
	{
		Activate();
	}
};

/*
 * Veduci zmeny, ktory odstrani zle pridanu polozku v nakupe.
 */
class SellerManager : public Process {
	size_t m_index = 0;
	size_t m_count = 0;

	void Behavior() override
	{
		if (ENABLE_REALLOCATION) {
			for (size_t i = 0; i < CASH_COUNT && ACTIVE_CASH[i]; i++)
				m_count++;

			m_index = Uniform(0, m_count);
		}
		else {
			m_index = Uniform(0, CASH_SELLER_IN_DELICATESSEN);
		}

		Wait(Exponential(SELLER_MANAGER_COMMING_TME));
		Seize(FacilityCashes[m_index], 1);
		Wait(Exponential(INTERRUPT_WAIT_TIME));
		Release(FacilityCashes[m_index]);
	}

public:
	SellerManager()
	{
		Activate();
	}
};

/*
 * Generovanie preresenia pri zlom zadani produktu na pokladni.
 */
class GenInterrupt : public Event {
	void Behavior() override
	{
		new SellerManager;
		Activate(Time + Exponential(4 hours));
	}

public:
	GenInterrupt()
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

	// Nastavenie zdielanej fronty vsetkym obsluznym zariadeniam
	for (size_t i = 0; i < CASH_COUNT_IN_DELICATESSEN; i++) {
		//FacilityCashesInDelicatessen[i].SetQueue(QueueDelicatessen);

		if (i < CASH_SELLER_IN_DELICATESSEN)
			ACTIVE_CASH_IN_DELICATESSEN[i] = true;
	}

	for (size_t i = 0; i < WINE_TAPS_COUNT; i++)
		FacilityWineTaps[i].SetName("Vycap vina");

	for (size_t i = 0; i < CASH_COUNT; i++)
		FacilityCashes[i].SetName("Pokladna ");

	RandomSeed(time(NULL)); // inicializacia generatora 
	Init(0, SIMULATION_TIME); // start simulacie

	new GenShopperArrival(); // Aktivacia generatora prichodu zakaznikov
	new GenDailyMode(); // Aktivacia casovaca pre dennu dobu
	new GenInterrupt();

	Run();

	// Vypis pokladni
	for (size_t i = 0; i < CASH_COUNT; i++)
		FacilityCashes[i].Output();

	// Vypis pokladni v lahodkach
	for (size_t i = 0; i < CASH_COUNT_IN_DELICATESSEN; i++)
		FacilityCashesInDelicatessen[i].Output();

	QueueDelicatessen.Output();
	StoreBaskets.Output();
}

