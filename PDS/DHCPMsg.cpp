#include "DHCPMsg.h"
#include "PcapUtil.h"

using namespace std;

string TEthHeader::toString(const string &separator) const
{
	string repr;

	repr += "destination MAC address: ";
	for (uint8_t i : destinationMacAddr)
		repr += PcapUtil::intToHex(i) + ":";
	repr.pop_back();
	repr += separator;

	repr += "source MAC address: ";
	for (uint8_t i : sourceMacAddr)
		repr += PcapUtil::intToHex(i) + ":";
	repr.pop_back();
	repr += separator;

	repr += "ethernet type: ";
	repr += PcapUtil::intToHex(ethType, "0x");
	repr += separator;

	return repr;
}

string TIpHeader::toString(const string &separator) const
{
	string repr;

	repr += "IP (4b) version and IHL (4b): ";
	repr += PcapUtil::intToHex(ipVersionAndIHL);
	repr += separator;

	repr += "DSCP (6b) and ECN (2b): ";
	repr += PcapUtil::intToHex(ipDscpEAndECN);
	repr += separator;

	repr += "Total length: ";
	repr += to_string(ipTotalLength);
	repr += separator;

	repr += "Identification: ";
	repr += PcapUtil::intToHex(ipIdentification);
	repr += separator;

	repr += "Flags and fragmentation: ";
	repr += PcapUtil::intToHex(ipFlagsAndFragmentOffset);
	repr += separator;

	repr += "TTL: ";
	repr += PcapUtil::intToHex(ipTtl);
	repr += separator;

	repr += "Protocol: ";
	repr += PcapUtil::intToHex(ipProtocol);
	repr += separator;

	repr += "Header check sum: ";
	repr += PcapUtil::intToHex(ipHeaderChecksum);
	repr += separator;

	repr += "Destination IP addr: ";
	for (uint8_t i : ipDestinationIPAddr)
		repr += to_string(i) + ".";
	repr.pop_back();
	repr += separator;

	repr += "Source IP addr: ";
	for (uint8_t i : ipSourceIPAddr)
		repr += to_string(i) + ".";
	repr.pop_back();
	repr += separator;

	return repr;
}

string TUdpHeader::toString(const string &separator) const
{
	string repr;

	repr += "Source port: ";
	repr += to_string(udpSourcePort);
	repr += separator;

	repr += "Destination port: ";
	repr += to_string(udpDestinationPort);
	repr += separator;

	repr += "Length: ";
	repr += to_string(udpLength);
	repr += separator;

	repr += "Checksum port: ";
	repr += to_string(udpChecksum);
	repr += separator;

	return repr;
}

string TDHCPHeader::toString(const string &separator) const
{
	string repr;

	repr += "OpCode port: ";
	if (opCode == 1)
		repr += "request";
	else if (opCode == 2)
		repr += "reply";
	else
		repr += "unknown";
	repr += separator;

	repr += "Hardware type: ";
	if (hardwareType == 1)
		repr += "ethernet";
	else
		repr += "unknown";
	repr += separator;

	repr += "Hardware address length: ";
	repr += to_string(hardwareAddrLength);
	repr += separator;

	repr += "Hop count: ";
	repr += to_string(hopCount);
	repr += separator;

	repr += "Transaction ID: ";
	repr += to_string(transactionID);
	repr += separator;

	repr += "Number of seconds: ";
	repr += to_string(numberOfSeconds);
	repr += separator;

	repr += "Flags: ";
	repr += PcapUtil::intToHex(flags, "0x");
	repr += separator;

	repr += "Client IP addr: ";
	for (uint8_t i : clientIPAddr)
		repr += to_string(i) + ".";
	repr.pop_back();
	repr += separator;

	repr += "Your IP addr: ";
	for (uint8_t i : yourIPAddr)
		repr += to_string(i) + ".";
	repr.pop_back();
	repr += separator;

	repr += "Server IP addr: ";
	for (uint8_t i : serverIPAddr)
		repr += to_string(i) + ".";
	repr.pop_back();
	repr += separator;

	repr += "Gateway IP addr: ";
	for (uint8_t i : gatewayIPAddr)
		repr += to_string(i) + ".";
	repr.pop_back();
	repr += separator;

	return repr;
}

string TDHCPData::toString(const string &separator) const
{
	string repr;

	repr += "Client hardware address: ";
	for (uint8_t i : clientHardwareAddress)
		repr += PcapUtil::intToHex(i) + " ";
	repr += separator;

	repr += "Server host name: ";
	for (uint8_t i : serverHostName)
		repr += i;
	repr += separator;

	repr += "Boot filename: ";
	for (uint8_t i : bootFilename)
		repr += i;
	repr += separator;

	return repr;
}
