/**
 * @file gif2bmp.c
 * @Author Peter Tisovčík (xtisov00) <xtisov00@stud.fit.vutbr.cz>
 * @date 18 February, 2018
 * @brief Subor s implementacioumetod pre prevedenie gif obrazku do bmp.
 */

#include "gif2bmp.h"

using namespace std;

static map<Interlace, size_t> interlaceMap = {
	{EVERY_8TH_LINE, 8},
	{EVERY_MISSING_4TH_LINE, 8},
	{EVERY_MISSING_EVEN_LINE, 4},
	{EVERY_MISSING_ODD_LINE, 2},
};

GIF::GIF()
{
}

GIF::GIF(vector <uint8_t> &rawData, FILE *output)
{
	auto index = rawData.begin();
	m_gifSize = rawData.size();

	// Header block - gif signature and version
	m_header = parseHeader({index, index + GIF_HEADER_SIZE});
	index += GIF_HEADER_SIZE;

	// check signature and version of gif file
	if (m_header.signature != GIF_SIGNATURE
		|| (m_header.version != GIF_VERSION_87 && m_header.version != GIF_VERSION_89))
		throw CustomException("Invalid/unsupported gif signature or version.");

	// Logical screen descriptor
	m_screenDescriptor = parseLogicalScreenDescriptor({index, index + GIF_LOGICAL_SCREEN_DESCRIPTOR_SIZE});
	index += GIF_LOGICAL_SCREEN_DESCRIPTOR_SIZE;

	// Global color table
	if (m_screenDescriptor.isGlobalColorTable) {
		parseColors({index, index + m_screenDescriptor.globalColorTableSize * 3}, m_globalTable);
		index += m_screenDescriptor.globalColorTableSize * 3;
	}

	// Parse blocks
	while (*index != GIF_TRAILER) {
		if (*index == GIF_EXTENSION) {
			parseExtensions(++index);
		}
		else if (*index == GIF_IMAGE_DESCRIPTOR) {
			doLWZ(output, index);

			if (m_graphicsControl.delayTime > 0) {
				cerr << "input gif is animated, output bmp file contains first image from gif" << endl;
				break;
			}
		}
		else {
			throw CustomException("unknown error in parse gif file");
		}
	}
}

void GIF::parseExtensions(vector<uint8_t>::iterator &index)
{
	switch (*index) {
	case GIF_GRAPHICS_CONTROL_EXTENSION:
		m_graphicsControl = parseGraphicsControl({index, index + GIF_GRAPHICS_CONTROL_EXTENSION_SIZE});
		index += GIF_GRAPHICS_CONTROL_EXTENSION_SIZE;
		break;
	case GIF_PLAIN_TEXT_EXTENSION:
		throw CustomException("unsupported plain text extension");
	case GIF_COMMENT_EXTENSION:
	case GIF_APPLICATION_EXTENSION:
		index++;
		while (true) {
			uint8_t blockSize = *index;
			index++;

			index += blockSize;
			if (blockSize == 0)
				break;
		}
		break;
	default:
		throw CustomException("unknown extension");
	}
}

void GIF::doLWZ(FILE *output, vector<uint8_t>::iterator &index)
{
	string inputBits;
	map<int64_t, vector<Rgb>> hashColorIndexes;

	size_t lzwCodeSize = 0;

	// Image descriptor
	m_imageDescriptor = parseImageDescriptor({index, index + GIF_IMAGE_DESCRIPTOR_SIZE});
	index += GIF_IMAGE_DESCRIPTOR_SIZE;

	// Create color table form global/local table of colors
	fillLocalColorTable(index);

	// LWZ minumum code size
	const size_t constMinimumLZWCodeSize = size_t(*index) + 1;
	index++;

	lzwCodeSize = constMinimumLZWCodeSize;
	while (true) {
		size_t blockSize = *index;
		index++; // skip size of block

		if (blockSize == 0)
			break;

		LOG cout << "read new sub-block" << endl;

		// Image sub block
		const vector<uint8_t> tre = {index, index + blockSize};
		index += blockSize;

		// Creates hash table with index and color/colors
		static bool init = false;
		for (size_t i = 0; i < m_localColorTable.size() && init == false; i++)
			hashColorIndexes.emplace(i, vector<Rgb>({m_localColorTable.at(i)}));

		init = true;
		uint64_t pW, cW; //TODO pW - set default value
		vector<Rgb> strpW, strcW;

		// Convert uint8_t to binary string and append to string
		for (auto it : tre)
			inputBits.insert(0, bitset<8>(it).to_string());

		// has table last empty index (2 - Clear code and End Of information code)
		size_t hasTableLastEmptyIndex = hashColorIndexes.size() + 2;
		const auto endOfLZW = pow(2, constMinimumLZWCodeSize - 1) + 1;
		const auto clearCodeLZW = pow(2, constMinimumLZWCodeSize - 1);

		for (size_t i = 0; inputBits.size() >= lzwCodeSize; i++) {
			// new code from string
			string newCode = inputBits.substr(inputBits.size() - lzwCodeSize, lzwCodeSize);
			inputBits.erase(inputBits.size() - lzwCodeSize, lzwCodeSize);

			// convert string code to number (index to has table)
			cW = bitset<64>(newCode).to_ulong();
			LOG cout << dec << unsigned(cW) << endl;

			// end of LWZ algorithm
			if (endOfLZW == cW)
				break;

			// Clear code LWZ algorithm
			if (clearCodeLZW == cW) {
				lzwCodeSize = constMinimumLZWCodeSize;
				LOG cout << "reinit color table" << endl;

				// Reinit color table
				hashColorIndexes.clear();
				for (size_t j = 0; j < m_localColorTable.size(); j++)
					hashColorIndexes.emplace(j, vector<Rgb>({m_localColorTable.at(j)}));

				// set transparent
				if (m_graphicsControl.isTransparentColor) {
					LOG cout << "transparency flag is set" << endl;
					auto it = hashColorIndexes.find(m_graphicsControl.transparentColorIndex);

					if (it != hashColorIndexes.end()) {
						it->second.at(0).isAlpha = true;
						LOG cout << "transparency color is set" << endl;
					}
					else {
						LOG cout << "transparency color is not in color table" << endl;
					}
				}

				pW = UINT64_MAX;
				strpW.clear();
				hasTableLastEmptyIndex = hashColorIndexes.size() + 2;

				continue;
			}

			auto pf = hashColorIndexes.find(pW);
			if (pf != hashColorIndexes.end())
				strpW = pf->second;

			auto cf = hashColorIndexes.find(cW);
			if (cf != hashColorIndexes.end()) {
				strcW = cf->second;
				appendColor(strcW, m_outputColors);

				if (!strpW.empty()) {
					strpW.push_back(strcW.front());

					hashColorIndexes.emplace(hasTableLastEmptyIndex, strpW);
					hasTableLastEmptyIndex++;
				}
			}
			else {
				strcW.push_back(pf->second.front());

				strpW.push_back(strcW.front());
				hashColorIndexes.emplace(hasTableLastEmptyIndex, strpW);

				appendColor(strpW, m_outputColors);
				hasTableLastEmptyIndex++;
			}

			// check minimum size of LWZ code
			if (pow(2, lzwCodeSize) == hasTableLastEmptyIndex) {
				LOG cout << "increate minimum size of LWZ code" << endl;
				lzwCodeSize++;
			}

			if (lzwCodeSize >= 13)
				lzwCodeSize = 12;

			// move codes
			pW = cW;
			strpW = strcW;
		}

	}

	exportToBMP(output);
}

void GIF::fillLocalColorTable(vector<uint8_t>::iterator &index)
{
	if (m_imageDescriptor.localColorTable) {
		parseColors({index, index + m_imageDescriptor.localColorTableSize * 3}, m_localColorTable);
		index += m_imageDescriptor.localColorTableSize * 3;
	}
	else {
		m_localColorTable = m_globalTable;
	}
}

void GIF::exportToBMP(FILE *output)
{
	if (m_outputColors.size() < size_t(m_screenDescriptor.width * m_screenDescriptor.height))
		throw CustomException("problem with parse gif file, invalid count of colors");

	if (!m_imageDescriptor.interlace) {
		toBMP(m_outputColors, output, m_screenDescriptor.width, m_screenDescriptor.height);
		return;
	}

	// init output vector with default value
	vector<Rgb> interlaceOut(m_screenDescriptor.width * m_screenDescriptor.height, {0, 0, 0});

	doInterlaceStep(m_outputColors, interlaceOut, EVERY_8TH_LINE);
	doInterlaceStep(m_outputColors, interlaceOut, EVERY_MISSING_4TH_LINE);
	doInterlaceStep(m_outputColors, interlaceOut, EVERY_MISSING_EVEN_LINE);
	doInterlaceStep(m_outputColors, interlaceOut, EVERY_MISSING_ODD_LINE);

	toBMP(interlaceOut, output, m_screenDescriptor.width, m_screenDescriptor.height);
}

void GIF::doInterlaceStep(vector<Rgb> &in, vector<Rgb> &out, Interlace interlace)
{
	static size_t j = 0;

	uint32_t width = m_imageDescriptor.width;
	uint32_t size = m_imageDescriptor.height * width;
	size_t newIndex, oldIndex;
	size_t start = 0;

	if (EVERY_8TH_LINE == interlace)
		j = 0;
	else
		start = interlaceMap[interlace] / 2 * width;

	for (; start < size; start += interlaceMap[interlace] * width, j += width) {
		newIndex = start;
		oldIndex = j;

		for (uint32_t m = 0; m < width; m++, newIndex++, oldIndex++)
			out.at(newIndex) = in.at(oldIndex);
	}
}

void GIF::appendColor(const vector<Rgb> &colors, vector<Rgb> &outputColors)
{
	for (const Rgb &it : colors) {
		outputColors.push_back(it);

		LOG cout << " \t" << hex
				 << unsigned(it.red) << " "
				 << unsigned(it.green) << " "
				 << unsigned(it.blue) << " "
				 << endl;
	}
}

void GIF::toBMP(const vector<Rgb> &colors, FILE *output,
	uint16_t width, uint16_t height)
{
	BMPHeader header;
	header.bfSize = bmpSize();

	BMPInfoHeader infoHeader;
	infoHeader.biWidth = width;
	infoHeader.biHeight = height;

	// write BMP header
	fwrite(&header.bfType, sizeof(header.bfType), 1, output);
	fwrite(&header.bfSize, sizeof(header.bfSize), 1, output);
	fwrite(&header.bfReserved1, sizeof(header.bfReserved1), 1, output);
	fwrite(&header.bfReserved2, sizeof(header.bfReserved2), 1, output);
	fwrite(&header.bfOffBits, sizeof(header.bfOffBits), 1, output);

	// write BMP information
	fwrite(&infoHeader, sizeof(infoHeader), 1, output);

	// write color data
	// 4 is BMP color resolution
	BMPWriteColors({colors.begin(), colors.begin()+width*height}, output, width, 4);
}

void GIF::BMPWriteColors(const vector<Rgb> &colors, FILE *output,
	uint16_t width, size_t colorResolution)
{
	const size_t alignCount = calculateAlign(width, colorResolution);
	const uint8_t blank = 0;

	size_t k = colors.size();
	for (long int i = k - width; i >= 0; i -= width, k -= width) {
		for (size_t j = i; j < k; j++) {
			fwrite(&colors.at(j).blue, sizeof(colors.at(j).blue), 1, output);
			fwrite(&colors.at(j).green, sizeof(colors.at(j).green), 1, output);
			fwrite(&colors.at(j).red, sizeof(colors.at(j).red), 1, output);

			uint8_t tmp;
			if (colors.at(j).isAlpha)
				tmp = 0;
			else
				tmp = 0xff;

			fwrite(&tmp, sizeof(tmp), 1, output);
		}

		for (size_t j = 0; j < alignCount; j++)
			fwrite(&blank, sizeof(blank), 1, output);
	}
}

size_t GIF::bmpSize() const
{
	return BMP_HEADER_SIZE + BMP_INFO_HEADER_SIZE
		   + m_screenDescriptor.width * m_screenDescriptor.height * 4;
}

size_t GIF::gifSize() const
{
	return m_gifSize;
}

GifHeader GIF::parseHeader(const vector<uint8_t> &data)
{
	if (data.size() != GIF_HEADER_SIZE)
		throw CustomException("gif header size is too short");

	GifHeader header;
	for (size_t i = 0; i < 3; i++)
		header.signature += char(data.at(i));

	for (size_t i = 0; i < 3; i++)
		header.version += char(data.at(i + 3));

	return header;
}

GifScreenDescriptor GIF::parseLogicalScreenDescriptor(const vector<uint8_t> &data)
{
	if (data.size() != GIF_LOGICAL_SCREEN_DESCRIPTOR_SIZE)
		throw CustomException("gif logical screen descriptor is too short");

	GifScreenDescriptor desc;
	desc.width = data.at(1) << 8 | data.at(0);
	desc.height = data.at(3) << 8 | data.at(2);
	desc.isGlobalColorTable = (data.at(4) & 0x80) >> 7;
	desc.colorResolution = ((data.at(4) & 0x70) >> 4) + 1;
	desc.tableIsSorted = (data.at(4) & 0x08) >> 3;
	desc.globalColorTableSize = pow(2, (data.at(4) & 0x07) + 1);
	desc.bgColorIndex = data.at(5);
	desc.pixelAspectRatio = (data.at(6) == 0) ? 0 : (data.at(6) + 15) / 64;

	return desc;
}

void GIF::parseColors(const vector<uint8_t> &data, vector<Rgb> &colors)
{
	if (data.size() % 3 != 0)
		throw CustomException("colors data has invalid size");

	for (size_t i = 0; i < data.size(); i += 3) {
		colors.push_back({
			data.at(i),
			data.at(i + 1),
			data.at(i + 2)
		});
	}
}

GifImageDescriptor GIF::parseImageDescriptor(const vector<uint8_t> &data)
{
	if (data.size() != GIF_IMAGE_DESCRIPTOR_SIZE)
		throw CustomException("image descriptor has invalid size");

	if (data.at(0) != GIF_IMAGE_DESCRIPTOR)
		throw CustomException("invalid image descriptor separator");

	GifImageDescriptor desc;
	desc.left = data.at(2) << 8 | data.at(1);
	desc.top = data.at(4) << 8 | data.at(3);
	desc.width = data.at(6) << 8 | data.at(5);
	desc.height = data.at(8) << 8 | data.at(7);
	desc.localColorTable = (data.at(9) & 0x80) >> 7;
	desc.interlace = (data.at(9) & 0x40) >> 6;
	desc.isSorted = (data.at(9) & 0x20) >> 5;
	desc.localColorTableSize = ((data.at(9) & 0x07) == 0) ? 0 : pow(2, (data.at(9) & 0x07) + 1);

	return desc;
}

GifGraphicsControl GIF::parseGraphicsControl(const vector<uint8_t> &data)
{
	GifGraphicsControl control;

	if (data.size() != GIF_GRAPHICS_CONTROL_EXTENSION_SIZE)
		throw CustomException("graphics control extension has invalid size");

	if (data.at(0) != GIF_GRAPHICS_CONTROL_EXTENSION)
		throw CustomException("invalid extension introducer");

	control.blockSize = data.at(1);
	control.isTransparentColor = data.at(2) & 0x01;
	control.delayTime = data.at(4) << 8 | data.at(3);
	control.transparentColorIndex = data.at(5);

	return control;
}

size_t GIF::calculateAlign(size_t width, size_t coloResolution) const
{
	return ((-1 * coloResolution * width) % 4);
}

int gif2bmp(tGIF2BMP *gif2bmp, FILE *inputFile, FILE *outputFile)
{
	if (inputFile == nullptr || outputFile == nullptr)
		return -1;

	vector<uint8_t> rawData;

	uint8_t tmp;
	while (!feof(inputFile)) {
		fread(&tmp, sizeof(tmp), 1, inputFile);
		rawData.push_back(tmp);
	}

	// remove last duplicate element
	rawData.pop_back();

	try {
		GIF gif(rawData, outputFile);

		gif2bmp->bmpSize = gif.bmpSize();
		gif2bmp->gifSize = gif.gifSize();
	}
	catch (const CustomException &ex) {
		cerr << ex.message() << endl;
		return -1;
	}
	catch (...) {
		cerr << "unknown error" << endl;
		return -1;
	}

	return 0;
}
