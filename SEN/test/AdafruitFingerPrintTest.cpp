#include <cppunit/extensions/HelperMacros.h>

#include <cmath>
#include <unistd.h>
#include "AdafruitFingerPrint.h"

using namespace std;

class AdafruitFingerPrintTest : public CppUnit::TestFixture {
	CPPUNIT_TEST_SUITE(AdafruitFingerPrintTest);
	CPPUNIT_TEST(testShift);
	CPPUNIT_TEST(testPreparePacket);
	CPPUNIT_TEST(testParsePacket);
	CPPUNIT_TEST(testVerifyPassword);
	CPPUNIT_TEST(testParseVerityPasswordR);
	CPPUNIT_TEST(testReadImage);
	CPPUNIT_TEST(testConvertImage);
	CPPUNIT_TEST(testCreateModel);
	CPPUNIT_TEST(testStoreModel);
	CPPUNIT_TEST(testFastSearch);
	CPPUNIT_TEST_SUITE_END();

public:
	void testShift();
	void testPreparePacket();
	void testParsePacket();
	void testVerifyPassword();
	void testParseVerityPasswordR();
	void testReadImage();
	void testConvertImage();
	void testCreateModel();
	void testStoreModel();
	void testFastSearch();
};

CPPUNIT_TEST_SUITE_REGISTRATION(AdafruitFingerPrintTest);

void AdafruitFingerPrintTest::testShift()
{
	const uint32_t number = 0xff00;

	CPPUNIT_ASSERT_EQUAL((uint8_t) 0xff, rightShift(number, 8));
	CPPUNIT_ASSERT_EQUAL((uint8_t) 0xf0, rightShift(number, 4));

	CPPUNIT_ASSERT_EQUAL((uint8_t) 0xff, rightShift(number, 8));
	CPPUNIT_ASSERT_EQUAL((uint8_t) 0xf0, rightShift(number, 4));
}

void AdafruitFingerPrintTest::testPreparePacket()
{
	AdafruitConfig conf = {0xffeeddcc, 0};

	// random data
	AdafruitPayload data = {{1,2,3,4,5,6,7,8}, 8};

	AdafruitPacket packet =
		preparePacket(&conf, FINGERPRINT_VERIFYPASSWORD, &data);

	CPPUNIT_ASSERT_EQUAL((uint8_t) 19, packet.len);

	// Packet identification
	CPPUNIT_ASSERT_EQUAL((uint8_t) (FINGERPRINT_STARTCODE >> 8), (uint8_t) packet.data[0]);
	CPPUNIT_ASSERT_EQUAL((uint8_t) (FINGERPRINT_STARTCODE & 0xff), (uint8_t) packet.data[1]);

	// Address
	CPPUNIT_ASSERT_EQUAL((uint8_t) (0xff), (uint8_t) packet.data[2]);
	CPPUNIT_ASSERT_EQUAL((uint8_t) (0xee), (uint8_t) packet.data[3]);
	CPPUNIT_ASSERT_EQUAL((uint8_t) (0xdd), (uint8_t) packet.data[4]);
	CPPUNIT_ASSERT_EQUAL((uint8_t) (0xcc), (uint8_t) packet.data[5]);

	// Packet type
	CPPUNIT_ASSERT_EQUAL((uint8_t) (FINGERPRINT_VERIFYPASSWORD), (uint8_t) packet.data[6]);

	// Length
	CPPUNIT_ASSERT_EQUAL((uint8_t) (0x00), (uint8_t) packet.data[7]);
	CPPUNIT_ASSERT_EQUAL((uint8_t) (0x0a), (uint8_t) packet.data[8]);

	// Payload, skip 8 values
	for (size_t i = 0; i < 8; i++)
		CPPUNIT_ASSERT_EQUAL((uint8_t) (1 + i), (uint8_t) packet.data[9 + i]);

	// Checksum
	CPPUNIT_ASSERT_EQUAL((uint8_t) (0x00), (uint8_t) packet.data[17]);
	CPPUNIT_ASSERT_EQUAL((uint8_t) (65), (uint8_t) packet.data[18]);
}

void AdafruitFingerPrintTest::testParsePacket()
{
	AdafruitPacket packet = {{239, 1, 255, 255, 255, 255, 7, 0, 3, 0, 0, 10}, 12};
	AdafruitPayload payload = {{0}, 0};

	parsePacket(&packet, &payload);

	CPPUNIT_ASSERT_EQUAL((uint8_t) 4, (uint8_t) payload.len);
	CPPUNIT_ASSERT_EQUAL((uint8_t) 7, (uint8_t) payload.data[0]);
	CPPUNIT_ASSERT_EQUAL((uint8_t) 0, (uint8_t) payload.data[1]);
	CPPUNIT_ASSERT_EQUAL((uint8_t) 0, (uint8_t) payload.data[2]);
	CPPUNIT_ASSERT_EQUAL((uint8_t) 10, (uint8_t) payload.data[3]);
}

/**
 * 239, 1, 255, 255, 255, 255, 1, 0, 7, 19, 0, 0, 0, 0, 0, 27
 */
void AdafruitFingerPrintTest::testVerifyPassword()
{
	AdafruitConfig conf = {0xffffffff, 0};

	AdafruitPacket packet = verifyPassword(&conf);

	CPPUNIT_ASSERT_EQUAL((uint8_t) 16, packet.len);

	// Packet type
	CPPUNIT_ASSERT_EQUAL((uint8_t) (FINGERPRINT_COMMANDPACKET), (uint8_t) packet.data[6]);

	// Length
	CPPUNIT_ASSERT_EQUAL((uint8_t) (0x00), (uint8_t) packet.data[7]);
	CPPUNIT_ASSERT_EQUAL((uint8_t) (0x07), (uint8_t) packet.data[8]);

	// Data - packet type
	CPPUNIT_ASSERT_EQUAL((uint8_t) FINGERPRINT_VERIFYPASSWORD, (uint8_t) packet.data[9]);

	CPPUNIT_ASSERT_EQUAL((uint8_t) (0x00), (uint8_t) packet.data[10]);
	CPPUNIT_ASSERT_EQUAL((uint8_t) (0x00), (uint8_t) packet.data[11]);
	CPPUNIT_ASSERT_EQUAL((uint8_t) (0x00), (uint8_t) packet.data[12]);
	CPPUNIT_ASSERT_EQUAL((uint8_t) (0x00), (uint8_t) packet.data[13]);

	// Checksum
	CPPUNIT_ASSERT_EQUAL((uint8_t) (0x00), (uint8_t) packet.data[14]);
	CPPUNIT_ASSERT_EQUAL((uint8_t) (27), (uint8_t) packet.data[15]);
}

/**
 * 239, 1, 255, 255, 255, 255, 7, 0, 3, 0, 0, 10
 */
void AdafruitFingerPrintTest::testParseVerityPasswordR()
{
	AdafruitPacket packet = {{239, 1, 255, 255, 255, 255, 7, 0, 3, 0, 0, 10}, 12};
	AdafruitPayload payload = {{0}, 0};

	int ret = parseVerifyPasswordReply(&packet, &payload);

	CPPUNIT_ASSERT_EQUAL((uint8_t) PACKET_OK, (uint8_t) ret);
}

/**
 *
 */
void AdafruitFingerPrintTest::testReadImage()
{
	AdafruitConfig conf = {0,0};

	AdafruitPacket packet = readImage(&conf);

	CPPUNIT_ASSERT_EQUAL((uint8_t) 12, packet.len);

	// Packet type
	CPPUNIT_ASSERT_EQUAL((uint8_t) (FINGERPRINT_COMMANDPACKET), (uint8_t) packet.data[6]);

	// Length
	CPPUNIT_ASSERT_EQUAL((uint8_t) (0x00), (uint8_t) packet.data[7]);
	CPPUNIT_ASSERT_EQUAL((uint8_t) (0x03), (uint8_t) packet.data[8]);

	// Data - only type
	CPPUNIT_ASSERT_EQUAL((uint8_t) FINGERPRINT_GETIMAGE, (uint8_t) packet.data[9]);

	// Checksum
	CPPUNIT_ASSERT_EQUAL((uint8_t) (0x00), (uint8_t) packet.data[10]);
	CPPUNIT_ASSERT_EQUAL((uint8_t) (0x05), (uint8_t) packet.data[11]);
}

/**
 *
 */
void AdafruitFingerPrintTest::testConvertImage()
{
	AdafruitConfig conf = {0,0};

	AdafruitPacket packet = convertImage(&conf, CharBuffer::BUFFER_1);

	CPPUNIT_ASSERT_EQUAL((uint8_t) 13, packet.len);
}

/**
 *
 */
void AdafruitFingerPrintTest::testCreateModel()
{
	AdafruitConfig conf = {0,0};

	AdafruitPacket packet = createModel(&conf);

	CPPUNIT_ASSERT_EQUAL((uint8_t) 12, packet.len);
}

/**
 *
 */
void AdafruitFingerPrintTest::testStoreModel()
{
	AdafruitConfig conf = {0,0};

	AdafruitPacket packet = storeModel(&conf, CharBuffer::BUFFER_1, 1);

	CPPUNIT_ASSERT_EQUAL((uint8_t) 15, packet.len);
}

/**
 *
 */
void AdafruitFingerPrintTest::testFastSearch()
{
	AdafruitConfig conf = {0,0};

	AdafruitPacket packet = fastSearch(&conf);

	CPPUNIT_ASSERT_EQUAL((uint8_t) 17, packet.len);
}
