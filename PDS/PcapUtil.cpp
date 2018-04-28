#include <chrono>
#include <cstring>
#include <error.h>
#include <exception>
#include <net/if.h>
#include <netinet/in.h>
#include <stdexcept>
#include <sys/ioctl.h>
#include <sys/socket.h>

#include <pcap.h>

#include "PcapUtil.h"

using namespace std;

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

uint64_t PcapUtil::timestamp()
{
	return (uint64_t) chrono::duration_cast< chrono::milliseconds >(
		chrono::system_clock::now().time_since_epoch()).count();
}

int PcapUtil::MACAddress(const string &dev, uint8_t *mac)
{
	struct ifreq s = {0};
	int fd = socket(PF_INET, SOCK_DGRAM, IPPROTO_IP);
	if (fd <= 0 )
		return -1;

	strcpy(s.ifr_name, dev.c_str());
	if (0 == ioctl(fd, SIOCGIFHWADDR, &s)) {
		int i;
		for (i = 0; i < 6; ++i)
			mac[i] = (uint8_t) s.ifr_addr.sa_data[i];

		return 0;
	}

	return -2;
}
