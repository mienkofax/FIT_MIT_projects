/**
 * @file Statistics.h
 * @Author Peter Tisovčík <xtisov00@stud.fit.vutbr.cz>
 * @date October, 2016
 */

#include <string>
#include <tuple>
#include <vector>

#include "LayerMessage.h"

typedef std::tuple<std::string, int, int> mytuple;

/*
 * Trieda uchovavajuca informacie o statistickych datach z protokolov.
 * Poskytuje metody na vlozenie statistickych dat a na ich vypisanie.
 * Vypisanie statistickych dat je vzdy usporiadane na zaklade value1.
 */
class Statistics {
public:
	/*
	 * Vlozenie noveho zaznamu do statistiky. V pripade, ze sa jedna o prvy
	 * zaznam s danymindexom je vytvorena dana polozka na zadanom indexe/kluci
	 * ak uz existuje aktualizuju sa udaje na danej pozicii.
	 * @param &key index podla, ktoreho sa bude vkladat sprava
	 * @param &value1 dlzka dat v protokole
	 * @param &value2 dlzka dat v protokole
	 */
	void insert(const std::string &key, const int &value1,
		const int &value2 = 0);

	/*
	 * Vypise usporiadanu statistiku pre Top10.
	 */
	void showTop10();

	/*
	 * Vypise usporiadanu statistiku pre celkovu statistiku prenesenych dat
	 * v ethernetovom ramci a v pozadovanom protokole.
	 */
	void showFilterStatistics();

private:
	std::vector<mytuple> m_data;

	/*
	 * Porovnanie hodnot v tuple. Porovnavaju sa hodnoty na indexe 1.
	 * Vyberu sa dva tuple zaznamy a kontroluju sa ci je prvy vacsi.
	 * @param &first prvy tuple prvok na porovnanie
	 * @param $second druhy tuple prvok na porovnanie
	 * @return true ak je prvy tuple prvok vacsi ako druhy
	 */
	static bool compare(const mytuple &first, const mytuple &second);
};
