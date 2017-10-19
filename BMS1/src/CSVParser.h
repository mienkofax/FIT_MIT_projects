#pragma once

#include <fstream>
#include <string>
#include <vector>

/**
 * CSV parser with support:
 *
 *  separators: comma (,), semicolon (;)
 *  end line: CRLF, LF
 *
 *
 * @see https://tools.ietf.org/html/rfc4180
 */
class CSVParser final {
public:
	CSVParser();

	bool open(const std::string &fileName, char separator = ',');
	void close();

	bool eof() const;

	std::vector<std::string> nextRow();

private:
	std::ifstream m_stream;
	char m_separator;
	unsigned long m_numberOfColumn;
	bool m_setNumberOfColumn;
};
