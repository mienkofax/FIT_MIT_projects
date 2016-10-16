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

void ArgumentParser::setOption(std::string shortName, std::string description, bool required,
	std::string argName, bool argRequired)
{
	m_options.push_back(Option(shortName, description, required, argName, argRequired));
}

void ArgumentParser::show()
{
	for (Option &option : m_options)
		option.show();
}

bool ArgumentParser::validateArguments(const int &argc, char *argv[])
{
	extractArguments(argc, argv);

	vector<string> filterType = {"mac", "ipv4", "ipv6", "tcp", "udp"};
	vector<string> filterArgumets = split(getArgument("f"), ',');
	size_t i = 0;
	string str;

	//validacia filtra ci existuje
	for (auto item : filterArgumets) {
		if (find(filterType.begin(), filterType.end(), item) == filterType.end())
			return false;
	}

	//kontrola ci bol zadany argument pre source/destination adresu
	if (!getOption("s").m_entered && !getOption("d").m_entered)
		return false;

	m_layerMessage.source = getOption("s").m_entered;
	m_layerMessage.destination = getOption("d").m_entered;

	//najpr kontrola ci sa nejedna o top10, povolena len jedna hodnota filtra,
	//jednoducha kontrola, hladanie vo vektore filtrov
	if (getArgument("v") == "top10") {
		if (find(filterType.begin(), filterType.end(), getArgument("f")) != filterType.end()) {
			m_isTop10 = true;
			return true;
		}

		return false;
	}

	vector<string> filterValue = split(getArgument("v"), ';');

	if (filterValue.size() != filterArgumets.size())
		return false;

	//validacia zadanych hodnot
	for (auto item : filterArgumets) {
		if (filterValue.size() == i)
			continue;

		str = filterValue[i];
		LayerData address;
	
		address.sourceAddress = getFilterValue(str, item);
		address.destinationAddress = address.sourceAddress;

		m_layerMessage.address[getProtocol(item)] = address;

		i++;
	}

	return true;
}

string ArgumentParser::getNormalizeString(string data, string protocol)
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

Protocols ArgumentParser::getProtocol(string protocol)
{
	if (protocol == "mac")
		return MAC;
	else if (protocol == "ipv4")
		return IPV4;
	else if (protocol == "ipv6")
		return IPV6;
	else if (protocol == "tcp")
		return TCP;
	else if (protocol == "udp")
		return UDP;
}

LayerMessage ArgumentParser::getLayersMessage()
{
	return m_layerMessage;		
}

vector<std::string> ArgumentParser::getFilterValue(const std::string &str, const string &filter)
{
	std::vector<std::string> values;

	for (std::string item : split(str, ',')) {
		//cout << "\t" << item << " " << filter << endl;
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

	throw std::invalid_argument("parameter not found");
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
				throw invalid_argument("repeat parameters");

			if (option.m_argRequired) {
				option.m_argName = optarg;
				loadOptions++;
			}

			option.m_entered = true;
		}

		if (optopt != 0)
			throw invalid_argument("invalid argument");
	}

	validateRequired();
}

string ArgumentParser::getOptString()
{
	string optString;

	for (Option &option : m_options)
		optString += option.m_shortName + (option.m_argRequired ? ":" : "");

	return optString;
}

Option &ArgumentParser::getOption(std::string shortName)
{
	for (auto &opt : m_options) {
		if (opt.m_shortName == shortName)
			return opt;
	}

	throw std::invalid_argument("neznamy argument");
}

void ArgumentParser::validateRequired()
{
	for (Option &option : m_options) {
		if (option.m_required && !option.m_entered)
			throw invalid_argument("Required parameter not found");
	}
}

std::vector<std::string> ArgumentParser::split(const std::string &text,
	const char &separator)
{
	std::vector<std::string> tokens;
	size_t start = 0;
	size_t end = 0;
//TODO throw ak sa podarilo najst prazdy retazec
	while ((end = text.find(separator, start)) != std::string::npos) {
		if (end != start)
			tokens.push_back(text.substr(start, end - start));

		start = end + 1;
	}

	if (end != start)
		tokens.push_back(text.substr(start));
	return tokens;
}
