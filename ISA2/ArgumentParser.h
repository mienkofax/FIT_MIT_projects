/**
 * @file ArgumentParser.h
 * @Author Peter Tisovčík <xtisov00@stud.fit.vutbr.cz>
 * @date October, 2016
 */

#pragma once

#include <list>
#include <string>
#include <vector>

#include "Option.h"

class ArgumentParser {
public:
	ArgumentParser():
		m_isTop10(false)
	{
	}

	void setOption(std::string shortName, std::string description, bool required,
		std::string argName, bool argRequired);

	void show();
	bool validateArguments(const int &argc, char *argv[]);

private:
	std::list<Option> m_options;
	bool m_isTop10;

	void extractArguments(const int &argc, char *argv[]);
	void validateRequired();
	std::string getArgument(const std::string &arg);
	std::string getOptString();
	Option &getOption(std::string shortName);
	std::vector<std::string> split(const std::string &text, const char &separator);
	std::vector<std::string> getFilterValue(const std::string &str, const std::string &filter);
};
