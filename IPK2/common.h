#ifndef COMMON
#define COMMON

#include <string.h>
#include <stdlib.h>
#include <getopt.h>
#include <sstream>
#include <fstream>
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <unistd.h>

#include <iostream>

using namespace std;

#define BUFFER_SIZE 1024

/* Argumenty Programu */
typedef struct Data {
	int port;
	string hostname;
	bool download;
	bool upload;
	string filename;
} TUrlData;

void printErrMsg(int err_code);
int parseURL(TUrlData *args, string host);
int msg_type(string data);
string read_msg(int tmp_sock);
string create_msg_text(string name, int data);
string create_msg_text(string name, string data);
int msg_size(string msg);


enum typeOfMsg {
	DOWNLOAD,
	UPLOAD,
	DW_LEN,
	ACK,
	DATA,
	ERROR
};

/* Navratove kody */
enum code {
	OK,
	E_SOCK,
	E_HOST,
	E_CONN,
	E_BIND,

	E_SERVER,
	E_LINK,
	E_ARGV,
	E_UNKNOWN,
	E_LISTEN,

	E_ACCEPT,
	E_FORK,
	E_SEND,
	E_FILE,
	E_ACK,

	E_SAVE
};

const string errMsg[] = {
	"",
	"socket",
	"hostname",
	"connect",
	"bind",

	"server error",
	"bad link",
	"bad argument",
	"unknown err",
	"can't listen",

	"not accepter, ",
	"fork failed",
	"failed to send",
	"read file",
	"problem with ACK",

	"save file"
};



#endif
