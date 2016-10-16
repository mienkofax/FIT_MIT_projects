/**
 * @file Statistics.cpp
 * @Author Peter Tisovčík <xtisov00@stud.fit.vutbr.cz>
 * @date October, 2016
 */

#include <algorithm>

#include "Statistics.h"

using namespace std;

bool Statistics::compare(const mytuple &lhs, const mytuple &rhs)
{
	return get<1>(lhs) > get<1>(rhs);
}


void Statistics::insert(const std::string &key, const int &value1, const int &value2)
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
		it != m_data.end() && a < 10; it++, a++)
		cout << get<0>(*it) << " " << get<1>(*it) << " "<< get<2>(*it) << endl;
}

void Statistics::showFilterStatistics()
{
	sort(m_data.begin(), m_data.end(), compare);
	for (vector<mytuple>::iterator it = m_data.begin();
		it != m_data.end(); it++)
		cout << get<1>(*it) << " "<< get<2>(*it) << endl;
}
