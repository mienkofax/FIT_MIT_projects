/**
 * @file ArgumentValidator.cpp
 * @Author Peter Tisovčík <xtisov00@stud.fit.vutbr.cz>
 * @date October, 2016
 */

#include <sstream>
#include <cctype>
#include <iostream>

#include <arpa/inet.h>

#include "ArgumentValidator.h"

#define MAC_LENGTH              17
#define MAC_OCTET_LENGTH        2
#define MAC_SEPARATOR_POSITION  3
#define MAC_SEPARATOR           'c'

using namespace std;

bool ArgumentValidator::ipv4Address(const std::string &value)
{
	struct in_addr result;

	if (inet_pton(AF_INET, value.c_str(), &result) == 1)
		return true;

	return false;
}

bool ArgumentValidator::ipv6Address(const std::string &value)
{
	struct in_addr result;

	if (inet_pton(AF_INET6, value.c_str(), &result) == 1)
		return true;

	return false;
}

bool ArgumentValidator::macAddress(const std::string &value)
{
	const char *mac = value.c_str();

	if (value.length() != MAC_LENGTH)
		return false;

	for (size_t i = 0; i < MAC_LENGTH; i++) {
		if ((i % MAC_SEPARATOR_POSITION != MAC_OCTET_LENGTH && !isxdigit(mac[i]))
			|| (i % MAC_SEPARATOR_POSITION == MAC_LENGTH && mac[i] != MAC_SEPARATOR))
			return false;
	}

	return true;
}

bool ArgumentValidator::port(const std::string &value)
{
	char* p;

	long number = strtol(value.c_str(), &p, 10);

	if (*p)
		return false;
		cout << "num" << number;
	if (number >= 0 && number <= 65535)
		return true;

	return false;
}
