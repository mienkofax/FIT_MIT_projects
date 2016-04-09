#! /usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse
import sys, codecs
import re
import lxml.etree as ET
import _csv as csv_module
import copy

from xml.dom import minidom

#nahradenie povodnej chybovej spravy pre parsovanie v argparse
class customArgumentParser(argparse.ArgumentParser):
	def error(self, _):
		sys.stderr.write("Zly prepinac.\n")
		sys.exit(1)


#hlavna trieda
class csv2xml:

	_substVal = False

	def __init__(self):
		argv = self.parseArg()

		spamreader = csv_module.reader(argv.inputFile, delimiter = argv.separator,  quotechar='"')

		#try:

		#-r=root-element
		xmlData = []
		if argv.root:
			xmlData.append(ET.Element(argv.root))
		else:
			xmlData.append(ET.Element("root"))

		header = True
		colName = []
		substData = ""
		for row in spamreader:
			#bez -h=subst pouzije sa hodnota v -c=column-element
			if argv.subst == None and header:
				col = "col"
				#priradenie nazvu stlpca -- -c=column-element
				if argv.colElem != None:
					col = argv.colElem

				argv.colElem = col
				#zapis default nazvu stlpca
				for i in range(0,len(row)):
					colName.append(col)
				header = False

				if self._substVal:
					continue
			#-h=subst -- nacita sahlavicka, -h --nacita sa hlavicka a nevypise sa
			elif argv.subst != None and header:
				col = "col"
				#priradenie nazvu stlpca -- -c=column-element
				if argv.colElem != None:
					col = argv.colElem

				argv.colElem = col
				#prevod nepovolenych znakov
				for item in row:
					colName.append(self.replaceElement(item, argv.subst))
				header = False
				if self._substVal:
					continue

			#tag oznacujuci jeden riadok v csv subore
			#-l=line-element
			xmlData.append(ET.SubElement(xmlData[0], argv.lineElem))
			lineElementIndex = len(xmlData)-1

			#-i -- pridanie indexu ako atributu do line-elementu
			if argv.index:
				xmlData[lineElementIndex].set("index", str(argv.start))
				argv.start +=  1

			if argv.errRecovery:
				val = ""
				#--missing-field=val
				if argv.missingField != None:
					val = argv.missingField

				#pouzitie missingField ak je definovane
				#-e,--error-recovery, vypise sa len pozadovany pocet stlpcov
				if len(colName) >= len(row):
					#prechod jednotlivymi stlpcami a ulozenie ich hodnot
					for i in range(0, len(colName)):
						#pridame -c=column-elementX
						if argv.subst == None:
							col = colName[i] + str(i+1)
						else:
							col = colName[i]

						if i+1 > len(row):
							xmlData.append(ET.SubElement(xmlData[lineElementIndex], col))
							xmlData[len(xmlData)-1].text = val
						else:
							xmlData.append(ET.SubElement(xmlData[lineElementIndex], col))
							xmlData[len(xmlData)-1].text = self.convertData(row[i])

				#osetrit situaciu, ked je hlavicka kratsia
				if len(colName) < len(row):
					#if argv.allColumns:
						#self.printErr("Vasci pocet zaznamov ako je dany v hlavicke.", 32)

					if argv.allColumns:
						count = len(row)
					else:
						count = len(colName)

					#prechod jednotlivymi stlpcami a ulozenie ich hodnot
					for i in range(0, count):
						#pridame -c=column-elementX

						if i+1 > len(colName):
							if len(colName) < len(row):
								val = row[i]
							else:
								val = ""
							xmlData.append(ET.SubElement(xmlData[lineElementIndex], argv.colElem + str(i+1)))
							xmlData[len(xmlData)-1].text = val
						else:
							if argv.subst == None:
								col = colName[i] + str(i+1)
							else:
								col = colName[i]

							xmlData.append(ET.SubElement(xmlData[lineElementIndex], col))
							xmlData[len(xmlData)-1].text = self.convertData(row[i])

			else:
				if len(colName) != len(row):
					self.printErr("Chybajuci alebo prebyvajuci stlpec.", 32)
				#prechod jednotlivymi stlpcami a ulozenie ich hodnot
				for i in range(0, len(colName)):
					if argv.subst == None:
						col = colName[i] + str(i+1)
					else:
						col = colName[i]

					xmlData.append(ET.SubElement(xmlData[lineElementIndex], col))
					xmlData[len(xmlData)-1].text = self.convertData(row[i])
		#except:
		#	self.printErr("Neznaman chyba.", 222)


		#data = ET.SubElement(xmlData[0], "data")
		self.generateXML(xmlData[0], argv)

	def generateXML(self, root, argv):
		if argv.header:
			output = ET.tostring(root, xml_declaration=False, encoding="UTF-8", pretty_print=True)
		else:
			output = ET.tostring(root, xml_declaration=True, encoding="UTF-8", pretty_print=True)

		#prevod bytes -> str
		output = output.decode("utf-8")

		#odstranenie root elementu ak je to potrebne
		lineByLine = output.split("\n")
		lineByLine[0] = lineByLine[0].replace("\'", "\"")

		#odstranenie posledneho prazdneho prvku
		lineByLine.pop()

		if not argv.root:
			if argv.header:
				output = lineByLine[:0] + lineByLine[1:]
			else:
				output = lineByLine[:1] + lineByLine[2:]
			#odstranenie ukoncovacieho root elementu
			if len(output) != 0:
				output.pop()
			#spojenie vysledneho retazca
			output = "\n".join(output)
			output += "\n"

		argv.outputFile.write(output)

	#vypis chybovej spravy
	def printErr(self, msg, exitCode):
		sys.stderr.write(msg + "\n")
		sys.exit(exitCode)

	#kontrola validnych znakov
	def validData(self, line):
		valid = re.compile(u'[\x00-\x08\x0b\x0c\x0e-\x1F\uD800-\uDFFF\uFFFE\uFFFF]')
		if None != valid.match(line):
			self.printErr("Nepodporovany unicode znak. ", 111)

	#validacia xml elementu row/col alebo root element
	def validElement(self, elem, exitCode):
		firstChar = True
		for char in elem:
			char = ord(char)

			#osetrenie pre prvy znak elementu
			if firstChar:
				if (char <= 64 or char in range(91,95) or char == 96 or
				char in range(123,192) or char == 215  or char == 247 or
				char in range (768, 880) or char == 894 or char in range(8192,8204) or
				char in range(8206,8304) or char in range (8592, 11264) or
				char in range(12272,12289) or char in range(55296, 63744) or
				char in range(64976,65008) or char in range(65534,1114111)):
					self.printErr("Nevalidny znak. ", exitCode)
				firstChar = False
				continue

			#osetrenie pre druhy a dalsie znaky v elemente
			if (char <= 44 or char == 47 or char in range(59,65) or
			char in range(91,95) or char == 96 or char in range(123,183) or
			char in range(184,192) or char == 215 or char == 247 or char == 894 or
			char in range(8192,8204) or char in range(8206,8255) or
			char in range(8257,8304) or char in range (8592, 11264) or
			char in range(12272,12289) or char in range(55296, 63744) or
			char in range(64976,65008) or char in range(65534,1114111)):
				self.printErr("Nevalidny znak. ", exitCode)

	#konvertuje problematicke znaky
	def convertData(self, elem):
		elem = elem.replace("&quot;", "\"")
		elem = elem.replace("&amp;", "&")
		elem = elem.replace("&apos;", "\'")
		elem = elem.replace("&lt;", "<")
		elem = elem.replace("&gt;", ">")
		return elem

	#konvertuje problematicke znaky
	def convertElementMis(self, elem):
		return elem

	#nahradi problematicke znaky pri
	#nahradenie pri row/col alebo root elemente
	def replaceElement(self, elem, pattern):
		elem = elem.replace(" ", pattern)
		elem = elem.replace("\n", pattern)
		elem = elem.replace("\r", pattern)
		elem = elem.replace("\t", pattern)
		elem = elem.replace(",", pattern)
		elem = elem.replace(".", pattern)

		#kontrola ci aj po nahradeni nezostal nepovoleny znak
		self.validElement(elem, 31)
		return elem

	def parseArg(self):
		parser = customArgumentParser(add_help = False)
		parser.add_argument("--help", action = "store_true", default = False, dest = "help", help = "Zobrazenie help spravy.")
		parser.add_argument("--input", action = "store", dest = "inputFile", help = "Vstupny CSV subor.")
		parser.add_argument("--output", action = "store", dest = "outputFile", help = "Vystupny XML subor")
		parser.add_argument("-n", action = "store_true", default = False, dest = "header", help = "Zakazanie vypisu hlavicky XML vystupu.")
		parser.add_argument("-r", action = "store", dest = "root", help = "Element obalujuci vysledok XML vystupu.")
		parser.add_argument("-s", action = "store", dest = "separator", help = "Jeden znak/idetifikat TAB, na zaklade, ktoreho sa delia jednotlive stlpce v CSV.")
		parser.add_argument("-h", action = "store", dest = "subst", nargs = "?", const = "-",help = "Prvy riadok CSV suboru bude sluzit ako hlavicka pre elementy v XML. Nepovoleny znak sa nahradi zadanym retazcom.")
		parser.add_argument("-c", action = "store", dest = "colElem", help = "Urcuje prefix mena elementu, ktory bude obalovat nepomenovane bunky.")
		parser.add_argument("-l", action = "store", dest = "lineElem", help = "Meno elementu, ktory obaluje zvlast kazdy riadok.")
		parser.add_argument("-i", action = "store_true", dest = "index", default = False, help = "Vlozi atribut index s ciselnou hodnotou do line-elementu.")
		parser.add_argument("--start", action = "store", type = int, dest = "start", help = "Cislo pre pocitadlo v prepinaci -i.")
		parser.add_argument("-e", "--error-recovery", action = "store_true", default = False, dest = "errRecovery", help = "Zotavenie z chyby.")
		parser.add_argument("--missing-field", action = "store", dest = "missingField", help = "Pokial nejaky stlpec chyba je doplneny touto hodnotou.")
		parser.add_argument("--all-columns", action = "store_true", dest = "allColumns", default=False, help = "Zamedzi ignorovaniu nekorektnych hodnot.")

		argv = parser.parse_args()
		err = False

		#help nemoze byt s inym prepinacom
		if (argv.help and len(sys.argv) != 2):
			self.printErr("Prepinac help nemoze byt pouzity s inymi prepinacmi.", 1)
		elif argv.help:
			parser.print_help()
			sys.exit(0)

		#kontrola: -r=root-element
		if argv.root != None:
			self.validElement(argv.root, 30)

		#kontrola separatoru - len jeden znak
		if argv.separator:
			if len(argv.separator) > 1 and argv.separator != "TAB":
				self.printErr("Prepinac -s moze obsahovat len jeden znak.", 1)

		#definovanie oddelovaca
		if argv.separator == None:
			argv.separator = ","
		elif argv.separator == "TAB":
			argv.separator = "\t"
		elif len(argv.separator) == 0:
			self.printErr("Oddelovac musi mat jeden znak.", 1);

		#kombinacia parametrov -i a -l
		if argv.index and argv.lineElem == None:
			self.printErr("Tento parametrer musi byt spolocne s prepinacom -l.",1)

		#nahradenie nevalidnych znakov v hlavicke vstupneho suboru -h=subst
		if argv.subst != None:
			pass
			#TODO dorobit validaciu
			#self.validElement(argv.subst, 31)

		#kontrola ci bola hodnota zadana alebo nie
		for item in sys.argv:
			if "-h" in item:
				self._substVal = True;

		#kontrola: -c=column-element
		if argv.colElem != None:
			self.validElement(argv.colElem, 30)


		#kontrola: --start=n
		if argv.start == None:
			argv.start = 1
		else:
			if argv.start < 0:
				self.printErr("Zaporne cislo.", 1)
			#kombinacia len s -i a -l, chyba
			if not argv.index or argv.lineElem == None:
				self.printErr("Prepinac musi byt kombinovany s prepinacmi: -i a -l.", 1)
		#kontrola: -i
		if argv.index and argv.lineElem == None:
			self.printErr("Zla kombinacia prepinacov.", 1)

		#kontrola: -l=line-element
		if argv.lineElem == None:
			argv.lineElem = "row"
		else:
			self.validElement(argv.lineElem, 30)

		#kontrola: --missing-field=val
		if argv.missingField != None:
			if argv.errRecovery:
				argv.missingField = self.convertElementMis(argv.missingField)
			else:
				self.printErr("Nie je zadany prepinac -e, --error-recovery.", 1);

		#kontrola: -e,--error-recovery
		if argv.errRecovery and argv.missingField == None:
			argv.missingField = ""

		#kontrola: --all-columns
		if argv.allColumns and not argv.errRecovery:
			self.printErr("Nie je zadany prepinac -e, --error-recovery.", 1);

		#kontrola vstupneho suboru, ci existuje a ci sa da otvorit
		if argv.inputFile:
			try:
				argv.inputFile = open(argv.inputFile, "r", newline = "", encoding = "utf-8")
			except:
				sys.stderr.write("Nelze nacist vstypny subor.\n")
				sys.exit(2)
		else:
			argv.inputFile = sys.stdin

		#kontrola vystupneho suboru, ci sa da vytvorit
		if argv.outputFile:
			try:
				argv.outputFile = open(argv.outputFile, "w", newline = "", encoding = "utf-8")
			except:
				sys.stderr.write("Nelze vytvorit vystypny subor.\n")
				sys.exit(3)
		else:
			argv.outputFile = sys.stdout

		#print(sys.argv)
		#print(argv)
		return argv

if __name__ == "__main__":
	csv2 = csv2xml()
