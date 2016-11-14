/**
 * @file ArgumentValidator.cpp
 * @Author Peter Tisovčík <xtisov00@stud.fit.vutbr.cz>
 * @date October, 2016
 */

#include <algorithm>
#include <cctype>
#include <iostream>
#include <sstream>

#include <arpa/inet.h>

#include "ArgumentValidator.h"

#define MAC_LENGTH              17
#define MAC_OCTET_LENGTH        2
#define MAC_SEPARATOR_POSITION  3
#define MAC_SEPARATOR           'c'

#define PORT_MINIMUM_NUMBER    0
#define PORT_MAXIMUM_NUMBER    65535

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
	struct in6_addr result;

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

	if (number >= PORT_MINIMUM_NUMBER && number <= PORT_MAXIMUM_NUMBER)
		return true;

	return false;
}

std::string Normalization::getIPv4(const std::string &ip)
{
	struct in_addr addr;
	inet_aton(ip.c_str(), &addr);
	return string(inet_ntoa(addr));
}

std::string Normalization::getIPv6(const std::string &ip)
{
	struct sockaddr_in sa;
	char str[INET6_ADDRSTRLEN];

	inet_pton(AF_INET6, ip.c_str(), &(sa.sin_addr));

	inet_ntop(AF_INET6, &(sa.sin_addr), str, INET_ADDRSTRLEN);
	return string(str);
}

std::string Normalization::getMac(std::string mac)
{
	for (auto & c: mac)
		c = tolower(c);

	return mac;
}
