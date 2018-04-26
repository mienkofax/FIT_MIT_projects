#pragma once

#include <string>

#include <cstdint>
#include <vector>
#include <iostream>
#include <stdexcept>
#include "PcapUtil.h"

struct DHCPMsgInfo {
	uint64_t fakeMACAddr;
	uint64_t firstPacket;
	uint64_t change;
	uint64_t lastLacket;
	uint8_t opCode;


};

struct PacketInfo {
	uint8_t dhcpMessageType;
	uint8_t dhcpServerIdentifier[4];


	std::string toString(const std::string &separator) const
	{
		std::string repr;

		repr += "messageType: ";
		repr += PcapUtil::intToHex(dhcpMessageType, "0x");
		repr += separator;

		repr += "identifier: ";
		for (auto i : dhcpServerIdentifier)
			repr += std::to_string(i) + ".";
		repr.pop_back();
		repr += separator;

		return repr;
	}

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

	std::string toString(const std::string &separator) const;
};

/**
 * @see https://en.wikipedia.org/wiki/IPv4
 * size: 20
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
	uint8_t srcIPAddr[4];
	uint8_t dstIPAddr[4];

	TIP4Header():
		versionAndIHL(0x45),
		DSCPAndECN(0),
		totalLength{0},
		identification(0),
		flagsAndFragmentOffset(0x40), // do not fragment
		ttl(0x40), // TTL 64
		protocol(0x11), // protokol na dalsej vrstve, UDP
		headerChecksum(0xd839),
		srcIPAddr{0x00},
		dstIPAddr{0xff, 0xff, 0xff, 0xff} // L3 broadcast addr
	{
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

	TDHCPData():
		clientHardwareAddress{0},
		serverHostName{0},
		bootFilename{0},
		magicCookie{0x63, 0x82, 0x53, 0x63}
	{
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
		recalculareHeadersSize();
	}

	void recalculareHeadersSize()
	{
		ipHeader.totalLength[0] = uint8_t(l3Size() >> 8);
		ipHeader.totalLength[1] = uint8_t(l3Size() & 0xff);

		udpHeader.udpLength[0] = uint8_t(l4Size() >> 8);
		udpHeader.udpLength[1] = uint8_t(l4Size() & 0xff);
	}

	std::string toString(const std::string &separator) const
	{
		std::string repr;

		repr += ethHeader.toString(separator);
		repr += ipHeader.toString(separator);
		repr += udpHeader.toString(separator);
		repr += dhcpHeader.toString(separator);
		repr += dhcpData.toString(separator);

		return repr;
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
		return sizeof(TIP4Header)
			+ sizeof(TUDPHeader)
			+ sizeof(TDHCPHeader)
			+ sizeof(TDHCPData);
	}

	uint16_t l4Size() const
	{
		return sizeof(TUDPHeader)
			+ sizeof(TDHCPHeader)
			+ sizeof(TDHCPData);
	}

	static DHCPMessage* fromRaw(uint8_t *buffer, size_t size)
	{
		if (size < sizeof(DHCPMessage))
			throw std::invalid_argument("nespravna velkost struktury");

		return (DHCPMessage *) buffer;
	}
};

struct __attribute__((__packed__)) DHCPDiscovery {
	TEthHeader ethHeader;
	TIP4Header ipHeader;
	TUDPHeader udpHeader;
	TDHCPHeader dhcpHeader;
	TDHCPData dhcpData;

	// option: DHCP Message type
	uint8_t op1Type;
	uint8_t op1Length;
	uint8_t op1payload;

	// option: Parameter Request List
	uint8_t op4Type;
	uint8_t op4Length;
	uint8_t op4payload[4];

	uint8_t end;

	DHCPDiscovery():
		op1Type(0x35), // DHCP Message type
		op1Length(0x01),
		op1payload{0x01}, // Discover
		op4Type(0x37), // Parameter Request List
		op4Length(0x04),
		op4payload{0x01, 0x03, 0x06, 0x2a}, //subnet mask, router, dns, ntp server
		end(0xff)
	{
		recalculareHeadersSize();
		ipHeader.headerChecksum = 0xd839;
	}

	void recalculareHeadersSize()
	{
		ipHeader.totalLength[0] = uint8_t(l3Size() >> 8);
		ipHeader.totalLength[1] = uint8_t(l3Size() & 0xff);

		udpHeader.udpLength[0] = uint8_t(l4Size() >> 8);
		udpHeader.udpLength[1] = uint8_t(l4Size() & 0xff);
	}

	std::string toString(const std::string &separator) const
	{
		std::string repr;

		repr += ethHeader.toString(separator);
		repr += ipHeader.toString(separator);
		repr += udpHeader.toString(separator);
		repr += dhcpHeader.toString(separator);
		repr += dhcpData.toString(separator);

		return repr;
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
		return sizeof(DHCPDiscovery)
			   - sizeof(TEthHeader);
	}

	uint16_t l4Size() const
	{
		return sizeof(DHCPDiscovery)
			   - sizeof(TEthHeader)
				- sizeof(TIP4Header);
	}

	static DHCPMessage* fromRaw(uint8_t *buffer, size_t size)
	{
		if (size < sizeof(DHCPMessage))
			throw std::invalid_argument("nespravna velkost struktury");

		return (DHCPMessage *) buffer;
	}
};

struct __attribute__((__packed__)) DHCPRequest {
	TEthHeader ethHeader;
	TIP4Header ipHeader;
	TUDPHeader udpHeader;
	TDHCPHeader dhcpHeader;
	TDHCPData dhcpData;

	// option: DHCP Message type
	uint8_t opType;
	uint8_t opLength;
	uint8_t opPayload;

	// option: Requested ip address
	uint8_t op1Type;
	uint8_t op1Length;
	uint8_t op1payload[4];

	// option: DHCP server identifier
	uint8_t op2Type;
	uint8_t op2Length;
	uint8_t op2payload[4];

	uint8_t end;

	DHCPRequest():
		opType(0x35), // DHCP Message type
		opLength(0x01),
		opPayload{0x03}, // Request
		op1Type(0x32),
		op1Length(4),
		op1payload{0x00},
		op2Type(0x36),
		op2Length(4),
		op2payload{0x00},
		end(0xff)
	{
		recalculareHeadersSize();
		ipHeader.headerChecksum = 0xd239;
	}

	void recalculareHeadersSize()
	{
		ipHeader.totalLength[0] = uint8_t(l3Size() >> 8);
		ipHeader.totalLength[1] = uint8_t(l3Size() & 0xff);

		udpHeader.udpLength[0] = uint8_t(l4Size() >> 8);
		udpHeader.udpLength[1] = uint8_t(l4Size() & 0xff);
	}

	std::string toString(const std::string &separator) const
	{
		std::string repr;

		repr += ethHeader.toString(separator);
		repr += ipHeader.toString(separator);
		repr += udpHeader.toString(separator);
		repr += dhcpHeader.toString(separator);
		repr += dhcpData.toString(separator);

		return repr;
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
		return sizeof(DHCPRequest)
			   - sizeof(TEthHeader);
	}

	uint16_t l4Size() const
	{
		return sizeof(DHCPRequest)
			   - sizeof(TEthHeader)
			   - sizeof(TIP4Header);
	}

	static DHCPMessage* fromRaw(uint8_t *buffer, size_t size)
	{
		if (size < sizeof(DHCPMessage))
			throw std::invalid_argument("nespravna velkost struktury");

		return (DHCPMessage *) buffer;
	}
};
