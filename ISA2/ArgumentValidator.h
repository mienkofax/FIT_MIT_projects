/**
 * @file ArgumentValidator.h
 * @Author Peter Tisovčík <xtisov00@stud.fit.vutbr.cz>
 * @date October, 2016
 */

class ArgumentValidator {
public:
	static bool macAddress(const std::string &value);
	static bool ipv4Address(const std::string &value);
	static bool ipv6Address(const std::string &value);
	static bool port(const std::string &value);
};
