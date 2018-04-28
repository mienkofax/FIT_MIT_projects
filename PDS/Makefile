GCC=g++
CFLAGS=-std=c++11
LIB=-lpcap

all:
	$(GCC) pds-dhcpstarve.cpp Common.cpp ClientMessage.cpp PcapUtil.cpp\
	 -o pds-dhcpstarve $(CFLAGS) $(LIB)
	#$(GCC) $(CFLAGS) pds-dhcprouge.c -o pds-dhcprouge $(LIB)

clean:
	/bin/rm -rf pds-dhcpstarve pds-dhcprouge

