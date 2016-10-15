/**
 * @file Statistics.h
 * @Author Peter Tisovčík <xtisov00@stud.fit.vutbr.cz>
 * @date October, 2016
 */

#include <fstream>
#include <string>
#include <vector>
#include <utility>
#include <tuple>

#include "LayerMessage.h"

typedef std::tuple<std::string, int, int> mytuple;

class Top10 {
public:
	void insertMessage(const std::string &key, const int &value1, const int &value2);
	void showStatistics();

private:
	std::vector<mytuple> m_data;

	static bool compare(const mytuple &lhs, const mytuple &rhs);
};
