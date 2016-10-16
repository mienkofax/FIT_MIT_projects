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
#include "LayerMessage.h"

class ArgumentParser {
public:
	ArgumentParser():
		m_isTop10(false)
	{
	}

	bool isTop10()
	{
		return m_isTop10;
	}

	std::vector<std::string> getFilter()
	{
		return split(getArgument("f"), ',');
	}

	void setOption(std::string shortName, std::string description, bool required,
		std::string argName, bool argRequired);

	void show();
	bool validateArguments(const int &argc, char *argv[]);
	LayerMessage getLayersMessage();
	std::string getArgument(const std::string &arg);

private:
	std::list<Option> m_options;
	bool m_isTop10;
	LayerMessage m_layerMessage;

	std::string getNormalizeString(std::string data, std::string protocol);
	void extractArguments(const int &argc, char *argv[]);
	Layer getLayerFromProtocol(std::string protocol);
	void validateRequired();
	std::string getOptString();
	Option &getOption(std::string shortName);
	std::vector<std::string> split(const std::string &text, const char &separator);
	std::vector<std::string> getFilterValue(const std::string &str, const std::string &filter);
};
