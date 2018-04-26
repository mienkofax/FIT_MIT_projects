#pragma once

#include <string>

#include <cstdint>
#include <vector>
#include <iostream>

struct DHCPMsgInfo {
	uint64_t fakeMACAddr;
	uint64_t firstPacket;
	uint64_t change;
	uint64_t lastLacket;
	uint8_t opCode;
};

/**
 * @see https://en.wikipedia.org/wiki/Ethernet_frame
 * size: 14B
 */
struct __attribute__((__packed__)) TEthHeader {
	uint8_t dstMACAddr[6];
	uint8_t srcMACAddr[6];
	uint16_t ethType;

	TEthHeader():
		dstMACAddr{0xff, 0xff, 0xff, 0xff, 0xff, 0xff}, // L2 broadcast addr
		srcMACAddr{0x00},
		ethType(0x0008) // IPv4 protokol na L3 vrstve
	{
	}

	std::vector<uint8_t> raw() const
	{
		std::vector<uint8_t> raw;

		for (auto i : dstMACAddr)
			raw.push_back(i);

		for (auto i : srcMACAddr)
			raw.push_back(i);

		raw.push_back(ethType);
		raw.push_back(ethType >> 8);

		return raw;
	}

	std::string toString(const std::string &separator) const;
};

/**
 * @see https://en.wikipedia.org/wiki/IPv4
 * size: 20
 *
 *  - versionAndIHL - verzia ip (4) a velkost hlavicky (20B)
 *  - DSCPAndECN -
 *  - totalLength - velkost paketu bez L2 hlavicky
 */
struct __attribute__((__packed__)) TIP4Header {
	uint8_t versionAndIHL;
	uint8_t DSCPAndECN;
	uint8_t totalLength[2];
	uint16_t identification;
	uint16_t flagsAndFragmentOffset;
	uint8_t ttl;
	uint8_t protocol;
	uint16_t headerChecksum;
	uint8_t dstIPAddr[4];
	uint8_t srcIPAddr[4];

	TIP4Header():
		versionAndIHL(0x45),
		DSCPAndECN(0),
		totalLength{0},
		identification(0),
		flagsAndFragmentOffset(0x40), // do not fragment
		ttl(0x40), // TTL 64
		protocol(0x11), // protokol na dalsej vrstve, UDP
		headerChecksum(0xc939),
		dstIPAddr{0xff, 0xff, 0xff, 0xff}, // L3 broadcast addr
		srcIPAddr{0x00}
	{
	}

	std::vector<uint8_t> raw() const
	{
		std::vector<uint8_t> raw;

		raw.push_back(versionAndIHL);
		raw.push_back(DSCPAndECN);
		raw.push_back(totalLength[0]);
		raw.push_back(totalLength[1]);

		raw.push_back(identification);
		raw.push_back(identification >> 8);

		raw.push_back(flagsAndFragmentOffset);
		raw.push_back(flagsAndFragmentOffset >> 8);

		raw.push_back(ttl);
		raw.push_back(protocol);

		raw.push_back(headerChecksum & 0xff);
		raw.push_back(headerChecksum >> 8);

		for (auto i : srcIPAddr)
			raw.push_back(i);

		for (auto i : dstIPAddr)
			raw.push_back(i);

		return raw;
	}

	std::string toString(const std::string &separator) const;
};

/**
 * @see https://en.wikipedia.org/wiki/User_Datagram_Protocol
 * size: 8B
 */
struct __attribute__((__packed__)) TUDPHeader {
	uint16_t udpSourcePort;
	uint16_t udpDestinationPort;
	uint8_t udpLength[2];
	uint16_t udpChecksum;

	TUDPHeader():
		udpSourcePort(0x4400), // 67
		udpDestinationPort(0x4300), // 68
		udpLength{0x12, 0x13},
		udpChecksum(0)
	{
	}

	std::vector<uint8_t> raw() const
	{
		std::vector<uint8_t> raw;

		raw.push_back(udpSourcePort);
		raw.push_back(udpSourcePort >> 8);

		raw.push_back(udpDestinationPort);
		raw.push_back(udpDestinationPort >> 8);

		raw.push_back(udpLength[0]);
		raw.push_back(udpLength[1]);

		raw.push_back(udpChecksum);
		raw.push_back(udpChecksum >> 8);

		return raw;
	}

	std::string toString(const std::string &separator) const;
};

/**
 * @see http://www.networksorcery.com/enp/protocol/dhcp.htm
 * size: 28B
 */
struct __attribute__((__packed__)) TDHCPHeader {
	uint8_t opCode;
	uint8_t hardwareType;
	uint8_t hardwareAddrLength;
	uint8_t hopCount;
	uint32_t transactionID;
	uint16_t numberOfSeconds;
	uint16_t flags;

	uint8_t clientIPAddr[4];
	uint8_t yourIPAddr[4];
	uint8_t serverIPAddr[4];
	uint8_t gatewayIPAddr[4];

	TDHCPHeader():
		opCode(0x01), // DHCP request
		hardwareType(0x01), // ethernet type
		hardwareAddrLength(0x06),
		hopCount(0),
		transactionID(0),
		numberOfSeconds(0),
		flags(0x0080),
		clientIPAddr{0x00},
		yourIPAddr{0x00},
		serverIPAddr{0x00},
		gatewayIPAddr{0x00}
	{
	}

	std::vector<uint8_t> raw() const
	{
		std::vector<uint8_t> raw;

		raw.push_back(opCode);
		raw.push_back(hardwareType);
		raw.push_back(hardwareAddrLength);
		raw.push_back(hopCount);

		//TODO spravne zarovnat
		raw.push_back(transactionID);
		raw.push_back(transactionID >> 8);
		raw.push_back(transactionID >> 16);
		raw.push_back(transactionID >> 24);

		raw.push_back(numberOfSeconds);
		raw.push_back(numberOfSeconds >> 8);

		raw.push_back(flags);
		raw.push_back(flags >> 8);

		for (auto i : clientIPAddr)
			raw.push_back(i);

		for (auto i : yourIPAddr)
			raw.push_back(i);

		for (auto i : serverIPAddr)
			raw.push_back(i);

		for (auto i : gatewayIPAddr)
			raw.push_back(i);

		return raw;
	}

	std::string toString(const std::string &separator) const;
};

/**
 * @see http://www.networksorcery.com/enp/protocol/dhcp.htm
 * size: 214
 */
struct __attribute__((__packed__)) TDHCPData {
	uint8_t clientHardwareAddress[16]; // 6B mac + 10B padding
	uint8_t serverHostName[64];
	uint8_t bootFilename[128];
	uint8_t magicCookie[4];

	// option: DHCP Message type
	uint8_t op1Type;
	uint8_t op1Length;
	uint8_t op1payload;

	// option: Client identifier
	uint8_t op2Type;
	uint8_t op2Length;
	uint8_t op2HardwareType;
	uint8_t op2payload[6];

	// option: Requested IP Address
	uint8_t op3Type;
	uint8_t op3Length;
	uint8_t op3payload[4];

	// option: Parameter Request List
	uint8_t op4Type;
	uint8_t op4Length;
	uint8_t op4payload[4];

	uint8_t end;

	TDHCPData():
		clientHardwareAddress{0},
		serverHostName{0},
		bootFilename{0},
		magicCookie{0x63, 0x82, 0x53, 0x63},
		op1Type(0x35), // DHCP Message type
		op1Length(0x01),
		op1payload{0x01}, // Discover
		op2Type(0x3d), // Client identifier
		op2Length(0x07),
		op2HardwareType(0x01), // Ethernet
		op2payload{0x00},
		op3Type(0x32), // Request IP Address
		op3Length(0x04),
		op3payload{0x00},
		op4Type(0x37), // Parameter Request List
		op4Length(0x04),
		op4payload{0x01, 0x03, 0x06, 0x2a}, //subnet mask, router, dns, ntp server
		end(0xff)
	{
	}

	std::vector<uint8_t> raw() const
	{
		std::vector<uint8_t> raw;

		for (auto i : clientHardwareAddress)
			raw.push_back(i);

		for (auto i : serverHostName)
			raw.push_back(i);

		for (auto i : bootFilename)
			raw.push_back(i);

		for (auto i : magicCookie)
			raw.push_back(i);

		raw.push_back(op1Type);
		raw.push_back(op1Length);
		raw.push_back(op1payload);

		raw.push_back(op2Type);
		raw.push_back(op2Length);
		raw.push_back(op2HardwareType);
		for (auto i : op2payload)
			raw.push_back(i);

		raw.push_back(op3Type);
		raw.push_back(op3Length);
		for (auto i : op3payload)
			raw.push_back(i);

		raw.push_back(op4Type);
		raw.push_back(op4Length);
		for (auto i : op4payload)
			raw.push_back(i);

		raw.push_back(end);

		return raw;
	}

	std::string toString(const std::string &separator) const;
};

struct __attribute__((__packed__)) DHCPMessage {
	TEthHeader ethHeader;
	TIP4Header ipHeader;
	TUDPHeader udpHeader;
	TDHCPHeader dhcpHeader;
	TDHCPData dhcpData;

	DHCPMessage()
	{
		ipHeader.totalLength[0] = uint8_t(l3Size() >> 8);
		ipHeader.totalLength[1] = uint8_t(l3Size() & 0xff);

		udpHeader.udpLength[0] = uint8_t(l4Size() >> 8);
		udpHeader.udpLength[1] = uint8_t(l4Size() & 0xff);
	}

	void setEthMAC(const std::vector<uint8_t> &mac)
	{
		for (size_t i = 0; i < 6; i++)
			ethHeader.srcMACAddr[i] = mac[i];
	}

	void setFakeClientMAC(const std::vector<uint8_t> &mac)
	{
		for (size_t i = 0; i < 6; i++)
			dhcpData.clientHardwareAddress[i] = mac[i];
	}

	void setTransactionID(uint32_t id)
	{
		dhcpHeader.transactionID = id;
	}

	uint16_t l3Size() const
	{
		return ipHeader.raw().size()
			+ udpHeader.raw().size()
			+ dhcpHeader.raw().size()
			+ dhcpData.raw().size();
	}

	uint16_t l4Size() const
	{
		return udpHeader.raw().size()
			   + dhcpHeader.raw().size()
			   + dhcpData.raw().size();
	}

	size_t raw(uint8_t *buffer) const
	{
		size_t index = 0;

		for (auto i : ethHeader.raw())
			buffer[index++] = i;

		for (auto i : ipHeader.raw())
			buffer[index++] = i;

		for (auto i : udpHeader.raw())
			buffer[index++] = i;

		for (auto i : dhcpHeader.raw())
			buffer[index++] = i;

		for (auto i : dhcpData.raw())
			buffer[index++] = i;

		return index;
	}


};

