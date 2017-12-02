#!/usr/bin/env python3

import serial
import struct
import time

if __name__ == '__main__':
	ser = None

	ser = serial.Serial("/dev/ttyUSB0", 57600, serial.EIGHTBITS, serial.PARITY_NONE, serial.STOPBITS_ONE, timeout=5)
	if ser.isOpen() == True:
		print("port is open")
	else:
		print("port is not open")

	# Verify passowrd
	print("verify password")
	ser.write([239, 1, 255, 255, 255, 255, 1, 0, 7, 19, 0, 0, 0, 0, 0, 27])

	print("verify password reply: ", end='')
	for i in range(0, 12):
		data = ser.read()
		print(struct.unpack('@B', data)[0], end=', ')

	print("\n")

	# Read Image
	end = False
	while not end:
		print("readImage")
		ser.write([239, 1, 255, 255, 255, 255, 1, 0, 3, 1, 0, 5])

		print("read image reply: ", end='')
		for i in range(0, 12):
			data = ser.read()

			if i == 9 and struct.unpack('@B', data)[0] == 0:
				end = True

			print(struct.unpack('@B', data)[0], end=', ')
	print(" read image successfully\n")

	# Convert image
	end = False
	while not end:
		print("convertImage")
		ser.write([239, 1, 255, 255, 255, 255, 1, 0, 4, 2, 1, 0, 8])

		print("convert image reply: ", end='', flush=True)
		for i in range(0, 12):
			data = ser.read()

			if i == 9 and struct.unpack('@B', data)[0] == 0:
				print("hure", end='')
				end = True

			print(struct.unpack('@B', data)[0], end=', ')
	print("\n")

	# Create model
	print("create model")
	ser.write([239, 1, 255, 255, 255, 255, 1, 0, 3, 5, 0, 9])

	print("create model reply: ", end='')
	for i in range(0, 12):
		data = ser.read()
		print(struct.unpack('@B', data)[0], end=', ')
	print("\n")

	# Store model
	print("store model")
	ser.write([239, 1, 255, 255, 255, 255, 1, 0, 6, 6, 1, 0, 1, 0, 15])

	print("store model reply: ", end='')
	for i in range(0, 14):
		data = ser.read()
		print(struct.unpack('@B', data)[0], end=', ')
	print("\n")

	# Find finger
	print("find finger")
	time.sleep(3)
	print("start finding: ")
	ser.write([239, 1, 255, 255, 255, 255, 1, 0, 8, 27, 1, 0, 0, 0, 163, 0, 200])

	print("finding reply: ", end='')
	for i in range(0, 16):
		data = ser.read()
		print(struct.unpack('@B', data)[0], end=', ')
	print("\n")
