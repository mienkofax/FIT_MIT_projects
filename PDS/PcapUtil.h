#pragma once

#include <string>
#include <set>
#include <sstream>
#include <iomanip>

class PcapUtil {
public:
	static std::set<std::string> allDevices();

	uint64_t randomUint64();

	template< typename T >
	static std::string intToHex(T value, const std::string &prefix = "")
	{
		std::stringstream stream;
		stream << prefix
			   << std::setfill ('0') << std::setw(sizeof(T)*2)
			   << std::hex << unsigned(value);
		return stream.str();
	}
};

