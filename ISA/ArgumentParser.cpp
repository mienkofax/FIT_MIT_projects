/**
 * @file ArgumentParser.cpp
 * @Author Peter Tisovčík <xtisov00@stud.fit.vutbr.cz>
 * @date October, 2016
 */

#include <algorithm>
#include <iostream>

#include <getopt.h>

#include "ArgumentParser.h"
#include "ArgumentValidator.h"
#include "LayerMessage.h"

using namespace std;

void ArgumentParser::show()
{
	for (Option &option : m_options)
		option.show();
}

bool ArgumentParser::checkFiltersExists(const vector<string> &filterArgumets)
{
	for (const auto &item : filterArgumets) {
		if (find(filterType.begin(), filterType.end(), item) == filterType.end())
			return false;
	}

	return true;
}

bool ArgumentParser::checkIsTop10()
{
	if (find(filterType.begin(), filterType.end(), getArgument("f")) != filterType.end()) {
		m_isTop10 = true;
		return true;
	}

	return false;
}

bool ArgumentParser::checkFilterValues(const vector<string> &filterArgumets)
{
	vector<string> filterValue = split(getArgument("v"), ';');
	string str;
	size_t i = 0;

	if (filterValue.size() != filterArgumets.size())
		return false;

	for (const auto &item : filterArgumets) {
		if (filterValue.size() == i)
			continue;

		str = filterValue[i];
		LayerData address;

		address.sourceAddress = getFilterValue(str, item);
		address.destinationAddress = address.sourceAddress;

		if (m_layerMessage.address.count(m_layerMessage.extractProtocol(item)) >= 1)
			return false;

		m_layerMessage.address[m_layerMessage.extractProtocol(item)] = address;
		i++;
	}

	return true;
}

bool ArgumentParser::validateArguments(const int &argc, char *argv[])
{
	extractArguments(argc, argv);

	vector<string> filterArgumets = split(getArgument("f"), ';');

	if (!checkFiltersExists(filterArgumets))
		return false;

	//kontrola ci bol zadany argument pre source/destination adresu
	if (!getOption("s").m_entered && !getOption("d").m_entered)
		throw invalid_argument("Invalid argument");

	if (getArgument("v") == "top10")
		return checkIsTop10();

	return checkFilterValues(filterArgumets);
}

string ArgumentParser::getNormalizeString(const string &data,
	const string &protocol)
{
	if (protocol == "mac")
		return Normalization::getMac(data);
	else if (protocol == "ipv4")
		return Normalization::getIPv4(data);
	else if (protocol == "ipv6")
		return Normalization::getIPv6(data);
	else
		return data;
}

vector<std::string> ArgumentParser::getFilterValue(const std::string &str, const string &filter)
{
	std::vector<std::string> values;

	for (std::string item : split(str, ',')) {
		if ((filter == "mac" &&  !ArgumentValidator::macAddress(item))
			|| (filter == "ipv4" && !ArgumentValidator::ipv4Address(item))
			|| (filter == "ipv6" && !ArgumentValidator::ipv6Address(item))
			|| ((filter == "tcp" || filter == "udp") && !ArgumentValidator::port(item)))
			throw std::invalid_argument("Bad filter value: " + item + " " + filter);

		values.push_back(getNormalizeString(item, filter));
	}

	return values;
}

string ArgumentParser::getArgument(const std::string &arg)
{
	for (Option &option : m_options) {
		if (option.m_shortName == arg)
			return option.m_argName;
	}

	throw std::invalid_argument("Parameter " + arg + " not found");
}

void ArgumentParser::extractArguments(const int &argc, char *argv[])
{
	int c;
	int loadOptions = 0;
	string optValue;
	opterr = 0;

	while ((c = getopt(argc, argv, getOptString().c_str())) != -1) {
		optValue = (char) c;

		for (Option &option : m_options) {
			if (optValue != option.m_shortName)
				continue;

			if (option.m_argRequired && option.m_entered)
				throw invalid_argument("Repeat parameter " + option.m_shortName);

			if (option.m_argRequired) {
				option.m_argName = optarg;
				loadOptions++;
			}

			option.m_entered = true;
		}

		if (optopt != 0)
			throw invalid_argument("Invalid argument");
	}

	validateRequired();

	m_layerMessage.source = getOption("s").m_entered;
	m_layerMessage.destination = getOption("d").m_entered;
}

string ArgumentParser::getOptString()
{
	string optString;

	for (const Option &option : m_options)
		optString += option.m_shortName + (option.m_argRequired ? ":" : "");

	return optString;
}

Option &ArgumentParser::getOption(const std::string &shortName)
{
	for (auto &opt : m_options) {
		if (opt.m_shortName == shortName)
			return opt;
	}

	throw std::invalid_argument("Unknown short name " + shortName);
}

void ArgumentParser::validateRequired()
{
	for (Option &option : m_options) {
		if (option.m_required && !option.m_entered)
			throw invalid_argument("Required parameter " + option.m_shortName
				+ " not found");
	}
}

std::vector<std::string> ArgumentParser::split(const std::string &text,
	const char &separator)
{
	std::vector<std::string> tokens;
	size_t start = 0;
	size_t end = 0;

	while ((end = text.find(separator, start)) != std::string::npos) {
		if (end != start)
			tokens.push_back(text.substr(start, end - start));

		start = end + 1;
	}

	if (end != start)
		tokens.push_back(text.substr(start));

	return tokens;
}
