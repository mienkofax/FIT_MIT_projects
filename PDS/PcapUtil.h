#pragma once

#include <iomanip>
#include <set>
#include <sstream>
#include <string>

class PcapUtil {
public:
	/**
	 * Zistenie dostupnych rozhrani na komunikaciu.
	 */
	static std::set<std::string> allDevices();

	uint64_t randomUint64();

	/**
	 * Pomocna metoda pre vypis cisla v hexadecimalnom formate.
	 */
	template<typename T>
	static std::string intToHex(
		T value, const std::string &prefix = "")
	{
		std::stringstream stream;
		stream << prefix
		       << std::setfill ('0') << std::setw(sizeof(T)*2)
		       << std::hex << unsigned(value);
		return stream.str();
	}

	/**
	 * Vratenie aktualneho timestampu v milisekundach.
	 */
	static uint64_t timestamp();

	/**
	 * Zistenie mac adresy daneho zariadenia.
	 */
	static int MACAddress(const std::string &dev, uint8_t *mac);
};

