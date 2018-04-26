#pragma once

#include <string>
#include <set>
#include <sstream>
#include <iomanip>
#include <chrono>

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

	static uint64_t timestamp()
	{

		return std::chrono::duration_cast< std::chrono::milliseconds >(
			std::chrono::system_clock::now().time_since_epoch()).count();
	}
};

