#pragma once

#include <stdint.h>

#define FINGERPRINT_STARTCODE 0xEF01

#define FINGERPRINT_OK 0x00

#define FINGERPRINT_COMMANDPACKET 0x1
#define FINGERPRINT_ACKPACKET 0x7

#define FINGERPRINT_GETIMAGE 0x01
#define FINGERPRINT_IMAGE2TZ 0x02
#define FINGERPRINT_REGMODEL 0x05
#define FINGERPRINT_STORE 0x06
#define FINGERPRINT_VERIFYPASSWORD 0x13
#define FINGERPRINT_HISPEEDSEARCH 0x1B

#define MIN_PACKET_SIZE 9

enum ERR {
	PACKET_IS_SHORT,
	PACKET_IS_BAD,
	PACKET_OK,
	PACKET_IS_NOT_ACK,
};

struct AdafruitConfig {
	uint32_t address;
	uint32_t password;
};

struct AdafruitPacket {
	uint8_t data[100];
	uint8_t len;
};

struct AdafruitPayload {
	uint8_t data[100];
	uint16_t len;
};

enum CharBuffer {
	BUFFER_1 = 0x01,
	BUFFER_2 = 0x02,
};

uint8_t rightShift(uint32_t n, uint8_t x)
{
	return n >> x & 0xff;
}

struct AdafruitPacket preparePacket(struct AdafruitConfig *conf, uint8_t packetType, struct AdafruitPayload *payload)
{
	// index
	uint8_t i = 0;
	struct AdafruitPacket packet;

	// Write start of header
	packet.data[i++] = FINGERPRINT_STARTCODE >> 8;
	packet.data[i++] = (uint8_t) FINGERPRINT_STARTCODE;

	// Write address
	packet.data[i++] = (uint8_t) (conf->address >> 24);
	packet.data[i++] = (uint8_t) (conf->address >> 16);
	packet.data[i++] = (uint8_t) (conf->address >> 8);
	packet.data[i++] = (uint8_t) (conf->address >> 0);

	// Write packet type
	packet.data[i++] = packetType;

	// Write packet length = payload (n bytes) + checksum (2 bytes)
	uint16_t len = payload->len + (uint8_t) 2;
	packet.data[i++] = (uint8_t) len >> 8;
	packet.data[i++] = (uint8_t) len >> 0;

	// Checksum = packet type (1 byte) + packet len (2 bytes) + payload (n bytes)
	uint16_t checksum = packetType + ((uint8_t) len) + ((uint8_t) len >> 8);

	// Write payload and calculate payload checksum
	for (size_t j = 0; j < payload->len; j++) {
		packet.data[i++] = payload->data[j];
		checksum += payload->data[j];
	}

	// Write checksum
	packet.data[i++] = (uint8_t) checksum >> 8;
	packet.data[i++] = (uint8_t) checksum >> 0;

	packet.len = i;
	return packet;
}

int parsePacket(struct AdafruitPacket *packet, struct AdafruitPayload *payload)
{
	if (packet->len < MIN_PACKET_SIZE)
		return PACKET_IS_SHORT;

	if (packet->data[0] != (FINGERPRINT_STARTCODE >> 8) ||
			packet->data[1] != (FINGERPRINT_STARTCODE & 0xff))
		return PACKET_IS_BAD;

	uint8_t packetType = packet->data[6];
	uint16_t len = (packet->data[7] << 8) | packet->data[8];

	payload->data[0] = packetType;
	len++;

	if (packetType != FINGERPRINT_ACKPACKET)
		return PACKET_IS_NOT_ACK;

	for (size_t i = 0; i < len; i++)
		payload->data[1 + i] = packet->data[9 + i];

	payload->len = len;

	return PACKET_OK;
}

struct AdafruitPacket verifyPassword(struct AdafruitConfig *conf)
{
	struct AdafruitPayload payload = {
		{
			FINGERPRINT_VERIFYPASSWORD,
			(uint8_t) (conf->password >> 24),
			(uint8_t) (conf->password >> 16),
			(uint8_t) (conf->password >> 8),
			(uint8_t) (conf->password >> 0),
		},
		5};

	return preparePacket(conf, FINGERPRINT_COMMANDPACKET, &payload);
}

int parseVerifyPasswordReply(struct AdafruitPacket *packet, struct AdafruitPayload *payload)
{
	parsePacket(packet, payload);

	uint8_t packetTypePayload = payload->data[1];

	if (packetTypePayload == FINGERPRINT_OK)
		return PACKET_OK;

	return -1;
}

struct AdafruitPacket readImage(struct AdafruitConfig *config)
{
	struct AdafruitPayload payload = {{FINGERPRINT_GETIMAGE}, 1};

	return preparePacket(config, FINGERPRINT_COMMANDPACKET, &payload);
}

struct AdafruitPacket convertImage(struct AdafruitConfig *config, enum CharBuffer buffer)
{
	struct AdafruitPayload payload = {{FINGERPRINT_IMAGE2TZ, buffer}, 2};

	return preparePacket(config, FINGERPRINT_COMMANDPACKET, &payload);
}

struct AdafruitPacket createModel(struct AdafruitConfig *config)
{
	struct AdafruitPayload payload = {{FINGERPRINT_REGMODEL}, 1};

	return preparePacket(config, FINGERPRINT_COMMANDPACKET, &payload);
}

struct AdafruitPacket storeModel(struct AdafruitConfig *config, enum CharBuffer buffer, uint16_t index)
{
	struct AdafruitPayload payload = {{FINGERPRINT_STORE, buffer, (uint8_t) (index >> 8), (uint8_t) (index &0xff)}, 4};

	return preparePacket(config, FINGERPRINT_COMMANDPACKET, &payload);
}

struct AdafruitPacket fastSearch(struct AdafruitConfig *config)
{
	struct AdafruitPayload payload = {{FINGERPRINT_HISPEEDSEARCH, 0x01, 0x00, 0x00, 0x00, 0xa3}, 6};

	return preparePacket(config, FINGERPRINT_COMMANDPACKET, &payload);
}
