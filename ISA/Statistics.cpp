/**
 * @file Statistics.cpp
 * @Author Peter Tisovčík <xtisov00@stud.fit.vutbr.cz>
 * @date October, 2016
 */

#include <algorithm>
#include <string>
#include <utility>
#include <tuple>

#include "Statistics.h"

#define TOP10_RECORDS 10

using namespace std;

bool Statistics::compare(const mytuple &first, const mytuple &second)
{
	return get<1>(first) > get<1>(second);
}

void Statistics::insert(const std::string &key, const int &value1,
	const int &value2)
{
	for (vector<mytuple>::iterator it = m_data.begin(); it != m_data.end();
		++it) {
		if (get<0>(*it) == key) {
			get<1>(*it) += value1;
			get<2>(*it) += value2;
			return;
		}
	}

	m_data.push_back(make_tuple(key, value1, value2));
}

void Statistics::showTop10()
{
	short a = 0;

	sort(m_data.begin(), m_data.end(), compare);
	for (vector<mytuple>::iterator it = m_data.begin();
		it != m_data.end() && a < TOP10_RECORDS; it++, a++)
		cout << dec <<get<0>(*it) << " " << get<1>(*it) << " "<< get<2>(*it) << endl;
}

void Statistics::showFilterStatistics()
{
	long int value1 = 0;
	long int value2 = 0;

	for (vector<mytuple>::iterator it = m_data.begin();
		it != m_data.end(); it++) {
		value1 += get<1>(*it);
		value2 += get<2>(*it);
	}

	cout << value1 << " " << value2 << endl;
}
