#include "PcapUtil.h"
#include <pcap.h>
#include <exception>
#include <error.h>
#include <stdexcept>

using namespace std;

static const int MAC_ADDRESS_SIZE = 6;

set<string> PcapUtil::allDevices()
{
	char err[PCAP_ERRBUF_SIZE];
	pcap_if_t *devs;

	if(pcap_findalldevs(&devs, err) == -1)
		throw invalid_argument("find all devs");

	set<string> devices;
	for(pcap_if_t *dev = devs; dev; dev = dev->next)
		devices.emplace(dev->name);

	return devices;
}

uint64_t PcapUtil::randomUint64()
{
	return (uint64_t(rand()) << 32) | uint32_t(rand());
}




