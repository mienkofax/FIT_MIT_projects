#include <iostream>

#include "CSVParser.h"

using namespace std;

CSVParser::CSVParser():
	m_numberOfColumn(0),
	m_setNumberOfColumn(false)
{
}

vector<string> CSVParser::nextRow()
{
	vector<string> row;
	string column;

	bool modeQuote = false;
	char c;

	while (m_stream.get(c)) {
		// detect separator, if is in mode add symbol to column otherwise new column
		if (m_separator == c) {
			if (modeQuote) {
				column += c;
			}
			else {
				row.push_back(column);
				column.clear();
			}
		}
		// detect quote, start or end quote mode - toggle
		else if ('"' == c) {
			modeQuote = !modeQuote;
		}
		// end line (CRLF, LF) , if is quote mode add symbol otherwise return row
		else if ('\n' == c || '\r' == c) {
			// hack for LF
			if ('\r' == c)
				continue;

			if (modeQuote) {
				column += c;
			}
			else {
				row.push_back(column);

				// set number of column in first line
				if (!m_setNumberOfColumn) {
					m_numberOfColumn = row.size();
					m_setNumberOfColumn = true;
				}

				// check number of column
				if (m_numberOfColumn != row.size())
					throw std::exception();

				return row;
			}
		}
		else {
			column += c;
		}
	}

	if (!column.empty())
		row.push_back(column);

	close();
	return row;
}

bool CSVParser::open(const string &fileName, char separator)
{
	m_stream.open(fileName.c_str());
	m_separator = separator;

	return m_stream.is_open();
}

void CSVParser::close()
{
	m_stream.close();
}

bool CSVParser::eof() const
{
	return m_stream.eof();
}
