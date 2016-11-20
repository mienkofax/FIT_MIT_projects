/**
 * @file Option.h
 * @Author Peter Tisovčík <xtisov00@stud.fit.vutbr.cz>
 * @date October, 2016
 */

#pragma once

#include <iostream>
#include <list>
#include <string>

/*
 * Trieda reprezentuje informacie o jednom prepinaci. Uchovava si informacie
 * ci bol zadany parameter zadany, ci je pozadovany, jeho popis, kratke meno,
 * ci je vyzadovany jeho argument a ak ano tak uchovava aj jeho hodnotu.
 */
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
		std::cerr << "[-" << m_shortName << " " << m_argName << "] " << m_description << std::endl;
		std::cerr << "\t parameter required: " << (m_required ? "true" : "false") << std::endl;
		std::cerr << "\t argument required: " << (m_argRequired ? "true" : "false") << std::endl;
		std::cerr << "\t entered: " << (m_entered ? "true" : "false") << std::endl;
	}

public:
	std::string m_shortName;
	std::string m_description;
	bool m_required;
	std::string m_argName;
	bool m_argRequired;
	bool m_entered;
};
