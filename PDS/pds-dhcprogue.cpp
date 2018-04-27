#include <string>
#include <getopt.h>
#include <iostream>
#include <string.h>

using namespace std;

static const string helpMessage =
"R(Help message)";

struct TIPv4 {
	int raw[4];

	static TIPv4 fromString(const string &ip, char separator = '.')
	{
		TIPv4 ipv4 = {0};
		size_t separatorCount = 0;
		string value;
		size_t index = 0;

		for (size_t i = 0; i < ip.size(); i++) {
			if (ip.at(i) != separator) {
				value.push_back(ip.at(i));

				if (i != ip.size()-1)
					continue;
			}

			separatorCount++;
			ipv4.raw[index++] = stoi(value, nullptr, 10);
			value.erase();
		}

		return ipv4;
	}

	string toString(const string &separator = "\n") const
	{
		string repr;

		for (auto it : raw)
			repr += to_string(it) + ".";
		repr.pop_back();
		repr += separator;

		return repr;
	}

	bool operator ==(const TIPv4 &ip) const
	{
		for (size_t i = 0; i < 4; i++) {
			if (raw[i] != ip.raw[i])
				return false;
		}

		return true;
	}

	bool operator !=(const TIPv4 &ip) const
	{
		for (size_t i = 0; i < 4; i++) {
			if (raw[i] != ip.raw[i])
				return true;
		}

		return false;
	}

	bool operator <(const TIPv4 &ip) const
	{
		for (size_t i = 0; i < 4; i++) {
			if (raw[i] < ip.raw[i])
				return true;
		}

		return false;
	}

	bool operator >(const TIPv4 &ip) const
	{
		for (size_t i = 0; i < 4; i++) {
			if (raw[i] > ip.raw[i])
				return true;
		}

		return false;
	}
};

struct TPool {
	TIPv4 start;
	TIPv4 end;

	static TPool fromString(const std::string &pool, char separator = '-')
	{
		TPool tPool;
		const size_t index = pool.find_first_of('-');
		const string first = pool.substr(0, index);

		tPool.start = TIPv4::fromString(first);
		tPool.end = TIPv4::fromString(
			pool.substr(first.size() + 1, -1));

		return tPool;
	}

	size_t size() const
	{
		uint64_t size = 0;

		for (size_t i = 0; i < 4; i++) {
			const int range = end.raw[i] - start.raw[i];

			if (range <= 0)
				continue;

			size += range << ((3 - i) * 8);
		}

		if (size > 0)
			size++;

		return size;
	}

	string toString (const std::string &separator = "\n") const
	{
		string repr;

		repr += "pool: \n";
		repr += "\tstart: " + start.toString("") + "\n";
		repr += "\tend  : " + end.toString("") + "\n";
		repr += "\tsize : " + to_string(size());
		repr += separator;

		return repr;
	}

};

struct TParams {
	string interface;
	TPool pool;
	TIPv4 gateway;
	TIPv4 dnsServer;
	std::string domain;
	uint64_t leaseTime;

	std::string toString(const std::string &separator = "\n") const
	{
		string repr;

		repr += "interface: ";
		repr += interface;
		repr += separator;

		repr += pool.toString();

		repr += "gateway: ";
		repr += gateway.toString();

		repr += "DNS Server: ";
		repr += dnsServer.toString();

		repr += "domain: ";
		repr += domain;
		repr += separator;

		repr += "lease time: ";
		repr += to_string(leaseTime);
		repr += separator;

		return repr;
	}
};


int extractArguments(int argc, char *argv[], TParams *t)
{
	int c;
	while ((c = getopt(argc, argv, "i:p:g:n:d:l:")) != -1) {
		switch (c){
		case 'i':
			t->interface = optarg;
			break;

		case 'p':
			t->pool = TPool::fromString(optarg);
			break;

		case 'g':
			t->gateway = TIPv4::fromString(optarg);
			break;

		case 'n':
			t->dnsServer = TIPv4::fromString(optarg);
			break;

		case 'd':
			t->domain = optarg;
			break;

		case 'l':
			t->leaseTime = stoi(optarg, nullptr, 10);
			break;

		default:
			cerr << "invalid arguments" << endl;
			return -1;
		}
	}

	return 0;
}

int main(int argc, char *argv[])
{
	if (argc != 13) {
		cerr << "invalid number of arguments" << endl;
		return EXIT_FAILURE;
	}

	TParams params;
	if (extractArguments(argc, argv, &params) == -1) {
		cerr << "invalid arguments" << endl;
		cerr << helpMessage << endl;
		return EXIT_FAILURE;
	}

	// TODO semantiku zadanych prikazov

	cout << params.toString() << endl;

}
