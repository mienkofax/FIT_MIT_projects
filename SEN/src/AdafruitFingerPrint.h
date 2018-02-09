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

uint8_t rightShift(uint32_t n, uint8_t x);

struct AdafruitPacket preparePacket(struct AdafruitConfig *conf, uint8_t packetType, struct AdafruitPayload *payload);
int parsePacket(struct AdafruitPacket *packet, struct AdafruitPayload *payload);

int parseVerifyPasswordReply(struct AdafruitPacket *packet, struct AdafruitPayload *payload);
struct AdafruitPacket verifyPassword(struct AdafruitConfig *conf);

struct AdafruitPacket readImage(struct AdafruitConfig *config);
struct AdafruitPacket convertImage(struct AdafruitConfig *config, enum CharBuffer buffer);
struct AdafruitPacket createModel(struct AdafruitConfig *config);
struct AdafruitPacket storeModel(struct AdafruitConfig *config, enum CharBuffer buffer, uint16_t index);
struct AdafruitPacket fastSearch(struct AdafruitConfig *config);
