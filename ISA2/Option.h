/**
 * @file Option.h
 * @Author Peter Tisovčík <xtisov00@stud.fit.vutbr.cz>
 * @date October, 2016
 */

#pragma once

#include <iostream>
#include <list>
#include <string>

class Option {
public:
	Option();
	Option(std::string shortName, std::string description, bool required,
		std::string argName, bool argRequired):
		m_shortName(shortName),
		m_description(description),
		m_required(required),
		m_argName(argName),
		m_argRequired(argRequired),
		m_entered(false)
	{
	}

	void show()
	{
		std::cout << "[-" << m_shortName << " " << m_argName << "] " << m_description << std::endl;
		std::cout << "\t parameter required: " << (m_required ? "true" : "false") << std::endl;
		std::cout << "\t argument required: " << (m_argRequired ? "true" : "false") << std::endl;
		std::cout << "\t entered: " << (m_entered ? "true" : "false") << std::endl;
	}

public:
	std::string m_shortName;
	std::string m_description;
	bool m_required; //whitch this parameter is required
	std::string m_argName; //required argument std::string
	bool m_argRequired; //whitch this argument is required
	bool m_entered;
};
