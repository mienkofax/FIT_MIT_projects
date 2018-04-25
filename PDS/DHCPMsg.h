#pragma once

#include <string>

#include <cstdint>

struct Tmp {
	uint8_t dstMac[6];
	uint8_t srcMac[6];
	uint16_t ipType;

	//IP header
	uint8_t version;
	uint8_t servField;
	uint16_t totLength;
	uint16_t identification;

	uint8_t hdrFlags;
	uint8_t offset;
	uint8_t ttl;
	uint8_t protocol;
	uint16_t ipSum;

	uint8_t srcAddr[4];
	uint8_t dstAddr[4];

	uint16_t srcPort;
	uint16_t dstPort;
	uint16_t len;
	uint16_t udpCheckSum;

	//DHCP zprava
	uint8_t messageType;
	uint8_t hwType;
	uint8_t hwAddresLen;
	uint8_t hops;
	uint16_t transactionID[2];
	uint16_t seconds;
	uint16_t flags;

	//IP adresy
	uint8_t clientIP[4];
	uint8_t yourIP[4];
	uint8_t serverIP[4];
	uint8_t gatewayIP[4];

	uint8_t MAC[6];
	uint16_t padding[5];
	uint16_t serverHost[32];
	uint16_t bootFile[64];
	uint16_t cookie[2];

	//DHCP options
	uint8_t typeID;
	uint8_t length;
	uint8_t DHCPoperation;

	uint8_t QtypeID;
	uint8_t Qlength;
	uint8_t Qmsg[13];

	uint8_t endMSG;
};

/**
 * @see https://en.wikipedia.org/wiki/Ethernet_frame
 * size: 14B
 */
struct TEthHeader {
	uint8_t destinationMacAddr[6] = {0xff};
	uint8_t sourceMacAddr[6] = {0xaa, 0xbb, 0x11, 0x12, 0x12, 0x12};
	uint16_t ethType = 0x0008;

	std::string toString(const std::string &separator) const;
};

/**
 * @see https://en.wikipedia.org/wiki/IPv4
 * size: 20
 */
struct TIpHeader {
	// 0 - 3 octets
	uint8_t ipVersionAndIHL = 0x45; // IPv4
	uint8_t ipDscpEAndECN = 0;
	uint16_t ipTotalLength = 0x1001;//0x2001;

	// 4 - 7 octets
	uint16_t ipIdentification = 0;
	uint16_t ipFlagsAndFragmentOffset = 0x40; // TTL=64

	// 8 - 11 octets
	uint8_t ipTtl = 0x40; // do not fragment
	uint8_t ipProtocol = 0x11; //UDP 17
	uint16_t ipHeaderChecksum = 0x9939;

	// 12 - 15 octets
	uint8_t ipDestinationIPAddr[4] = {0xff}; // broadcast ip address

	// 16 - 19 octets
	uint8_t ipSourceIPAddr[4] = {0};

	std::string toString(const std::string &separator) const;
};

/**
 * @see https://en.wikipedia.org/wiki/User_Datagram_Protocol
 * size: 8B
 */
struct TUdpHeader {
	uint16_t udpSourcePort = 0x4400; // 67
	uint16_t udpDestinationPort = 0x4300; // 68
	uint16_t udpLength = 0x0c01; // 284
	uint16_t udpChecksum = 0;

	std::string toString(const std::string &separator) const;
};

/**
 * @see http://www.networksorcery.com/enp/protocol/dhcp.htm
 * size: 28B
 */
struct TDHCPHeader {
	uint8_t opCode = 1; // dhcp request
	uint8_t hardwareType = 1; // ethernet type
	uint8_t hardwareAddrLength = 6;
	uint8_t hopCount = 0;
	uint32_t transactionID = 0;
	uint16_t numberOfSeconds = 0;
	uint16_t flags = 0;

	uint8_t clientIPAddr[4] = {0};
	uint8_t yourIPAddr[4] = {0};
	uint8_t serverIPAddr[4] = {0};
	uint8_t gatewayIPAddr[4] = {0};

	std::string toString(const std::string &separator) const;
};

/**
 * @see http://www.networksorcery.com/enp/protocol/dhcp.htm
 * size: 216
 */
struct TDHCPData {
	uint8_t clientHardwareAddress[16] = {0}; // 6B mac + 10B padding
	uint8_t serverHostName[64] = {0};
	uint8_t bootFilename[128] = {0};

	uint64_t magicCookie;

	std::string toString(const std::string &separator) const;
};

struct DHCPMessage {
	TEthHeader ethHeader;
	TIpHeader ipHeader;
	TUdpHeader udpHeader;
	TDHCPHeader dhcpHeader;
	TDHCPData dhcpData;
//	uint8_t tmp[9];
	//uint8_t endMSG = 255;

};

