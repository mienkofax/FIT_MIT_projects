#pragma once

#include "Common.h"


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

	uint8_t raw[6];

	uint8_t end;

	DHCPDiscovery():
		op1Type(0x35), // DHCP Message type
		op1Length(0x01),
		op1payload{0x01}, // Discover
		op4Type(0x37), // Parameter Request List
		op4Length(0x04),
		op4payload{0x01, 0x03, 0x06, 0x2a}, //subnet mask, router, dns, ntp server
		raw{0x37, 0x04, 0x01, 0x03, 0x06, 0x2a},
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

	// client identifier - max
	uint8_t op3Type;
	uint8_t op3Length;
	uint8_t op3EthType;
	uint8_t op3payload[6];

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
		op3Type(0x3d),
		op3Length(7),
		op3payload{0x00},
		op3EthType(0x01),
		end(0xff)
	{
		recalculareHeadersSize();
		ipHeader.headerChecksum = 0xc939;
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
		for (size_t i = 0; i < 6; i++) {
			dhcpData.clientHardwareAddress[i] = mac[i];
			op3payload[i] = mac[i];
		}
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