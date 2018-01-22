#apt-get install python-bs4
#coding=utf-8

#
# @author Peter Tisovcik <xtisov00@fit.vutbr.cz
#

from bs4 import BeautifulSoup
import lxml.etree as ET
import urllib2
import sys
import time
import sqlite3

reload(sys)
sys.setdefaultencoding('utf8')

pocet_tem_na_stranku = 35
pocet_prispevkov_na_stranku = 20
datum_text = ""
nazov_db = "hojko.db"

#funkcia na upravu datumu a jeho formatu
def check_date(from_date):

	#nahradenie za anglicke nazvy
	from_date = from_date.replace("Máj", "May")
	from_date = from_date.replace("Jún", "Jun")
	from_date = from_date.replace("Júl", "Jul")
	from_date = from_date.replace("Okt", "Oct")

	#konverzia
	conv=time.strptime(from_date,"%d %b %Y, %H:%M")
	datum_text = time.strftime("%Y/%m/%d %H:%M",conv)
	return datum_text

#funkcia pre vygenerovanie XML pre jednotlive subfora
def generate_xml(id_subfora):
	root = ET.Element("forum")
	x_subforum = ET.SubElement(root, "subforum")
	x_subforum_title = ET.SubElement(x_subforum, "title")
	x_subforum_title.text = str(id_subfora)

	db.execute("SELECT id,nazov_temy FROM tema WHERE id_subfora=%d"%id_subfora)
	result = db.fetchall()
	for t_tema in result:
		x_article = ET.SubElement(x_subforum, "article")
		x_article_title = ET.SubElement(x_article, "title")
		x_article_title.text = t_tema[1]

		db.execute("SELECT * FROM prispevok WHERE id_temy=%d"%t_tema[0])
		result2 = db.fetchall()
		for t_prispevok in result2:
			x_post = ET.SubElement(x_article, "post")

			x_author = ET.SubElement(x_post, "author")
			x_author.text = t_prispevok[2]
 			x_create_date = ET.SubElement(x_post, "create")
			x_create_date.text = t_prispevok[4]
			x_post_text = ET.SubElement(x_post, "text")
			x_post_text.text = t_prispevok[3]

	tree = ET.ElementTree(root)
	tree.write("dw__%s.xml"%id_subfora)
	print "SAVE: Subforum ulozene do xml suboru s nazvom: dw__%s.xml"%id_subfora
#####################################################################
conn = sqlite3.connect(nazov_db)
db = conn.cursor()

# Create table
db.execute('''CREATE TABLE IF NOT EXISTS subforum
             (id INTEGER PRIMARY KEY NOT NULL, nazov_subfora TEXT NOT NULL)''')

db.execute('''CREATE TABLE IF NOT EXISTS tema
             (id INTEGER PRIMARY KEY NOT NULL, id_subfora INT NOT NULL, nazov_temy TEXT , datum TEXT )''')

db.execute('''CREATE TABLE IF NOT EXISTS prispevok
             (id INTEGER PRIMARY KEY NOT NULL, id_temy INT NOT NULL, prispevok TEXT, autor TEXT, datum TEXT)''')


try:
	response = urllib2.urlopen('https://www.hojko.com')
except:
	print "ERR: Zadana adresa fora neexistuje."
	sys.exit(1)

data = response.read()
response.close()
soup = BeautifulSoup(data)

pocitadlo = 0

#stiahnutie dat z kazdeho subfora
for link in soup.findAll("a", { "class" : "forumlink" }):
	try:
		response = urllib2.urlopen(link['href'])
	except:
		print "ERR: Zadana adresa subfora neexistuje."
		continue

	data = response.read()
	response.close()
	soup2 = BeautifulSoup(data)

	#overenie ci uz existuje subforum
	db.execute("SELECT id FROM subforum WHERE nazov_subfora='%s'"%link.string)
	id_subfora=db.fetchall()

	#ulozenie nazvu subfora do db ak neexistuje
	if not id_subfora:
		db.execute("INSERT INTO subforum VALUES (NULL, '%s')"%(link.string))
		conn.commit()

	#vyber ID subfora a ulozenie
	db.execute("SELECT id FROM subforum WHERE nazov_subfora='%s'"%link.string)
	id_subfora=int(db.fetchone()[0])

	print "\n----------------------------------------"
	print "---------- Subforum: %s"% link.string
	print "----------------------------------------"

	#pocet stranok, ktore sa maju prehladat v jednotlivych subforach
	pocet_odkazov = len(soup2.find("div", { "id" : "pagecontent" }).find('table').findAll('a'))
	pocet_stranok = 0

	#ak esxistuje viacej stranok v danom subfore
	if pocet_odkazov != 1:
		links = soup2.find("div", { "id" : "pagecontent" }).find('table').findAll('a')
		pocet_stranok = int(links[-3].string) - 1

	pocet_stranok = int(pocet_stranok)
	link_subfora = "%s&start="% link['href']

	#stiahnutie a prehladavanie jednotlivych stranok subfora
	while pocet_stranok >= 0:
		subforum =  "%s%s"% (link_subfora, pocet_tem_na_stranku * pocet_stranok)

		try:
			response = urllib2.urlopen(subforum)
		except:
			print "ERR: Neexistuje stranka subfora."
			pocet_stranok -= 1
			continue

		data = response.read()
		response.close()
		soup3 = BeautifulSoup(data)

		#pole datumov v jednotlivej stranky subfor
		datum_temy2 = soup3.find("div", {"class":"block-start"}).find("table", { "class" : "tablebg"}).findAll("p", {"class", "topicdetails"})[2::]
		pocitadlo = 0

		#ziskanie a stiahnutie jednotlivych tem z jednotlivych stranok subfor
		for articles in soup3.findAll("a", { "class" : "topictitle" }):
			try:
				response = urllib2.urlopen(articles['href'])
			except:
				print "ERR: Neexistuje zadana stranka subfora."
				continue

			data = response.read()
			response.close()
			soup4 = BeautifulSoup(data)

			#overenie ci uz existuje subforum
			articles.string = articles.string.replace("\'", "\"")
			db.execute("SELECT id FROM tema WHERE nazov_temy='%s'"%(articles.string))
			id_temy=db.fetchall()

			#ulozenie nazvu temy do db ak neexistuje
			if not id_temy:
				db.execute("INSERT INTO tema VALUES (NULL, %d,'%s', '')"%(id_subfora, articles.string))
				conn.commit()

			#vyber ID temy
			db.execute("SELECT id, datum FROM tema WHERE nazov_temy='%s'"%articles.string)
			temp = db.fetchone()

			#priradenie datumu poslednej zmeny a id temy z db
			id_temy=int(temp[0])
			datum_poslednej_zmeny = temp[1]

			#prevod datum poslednej temy zo zoznamu tem na datum vo vhodnom formate
			datum_temy2_datum = check_date(datum_temy2[pocitadlo].string)

			if datum_temy2_datum==datum_poslednej_zmeny:
				pocitadlo +=4
				print "Old: ",
				print articles.string
				continue
			else:
				print "New: ",
				print articles.string
				pocitadlo += 4

			#pocet stranok temy, ktore sa maju prehladavat
			pocet_odkazov2 = len(soup4.find("div", { "id" : "pagecontent" }).find('table').findAll('a'))
			pocet_stranok2 = 0

			if pocet_odkazov2 != 1:
				links2 = soup4.find("div", { "id" : "pagecontent" }).find('table').findAll('a')
				pocet_stranok2 = int(links2[-3].string) - 1

			link_clanku = "%s&start="% articles['href']

			#spracovanie jednotlivych tem
			m = 0
			while m <=pocet_stranok2:
				article =  "%s%s"% (link_clanku, pocet_prispevkov_na_stranku * m)

				try:
					response = urllib2.urlopen(article)
				except:
					print "ERR: Neexistuje zadana tema."
					m += 1
					continue

				data = response.read()
				response.close()
				soup5 = BeautifulSoup(data)

				autor = soup5.findAll("div", { "class" : "postauthor"})
				text = soup5.findAll("div", { "class" : "postbody"})
				datum = soup5.findAll("td", {"class" : "postbottom"})

				k = 0
				while k < len(autor):
					try:
						autor_text = autor[k].find('a').getText()
					except:
						autor_text = "Host"

					#text prispevku
					prispevok = text[k].getText()

					#pripravenie stringu na konverziu, len kazdy druhy div
					from_date = datum[k*2].getText()
					datum_text=check_date(from_date)

					#osetrenie apostrofov
					prispevok = prispevok.replace("\'", "\"")
					autor_text = autor_text.replace("\'", "\"")

					#overenie ci uz existuje prispevok
					db.execute("SELECT id FROM prispevok WHERE prispevok='%s' AND autor='%s'"%(prispevok, autor_text))
					id_prispevku=db.fetchall()

					#ulozenie nazvu temy do db ak neexistuje
					if not id_prispevku:
						db.execute("INSERT INTO prispevok VALUES (NULL, %d, '%s', '%s', '%s')"%(id_temy, prispevok, autor_text, datum_text))
						conn.commit()

					k += 1

				m += 1
				#update datumu poslednej zmeny v teme
				db.execute("UPDATE tema SET datum='%s' WHERE id=%d"%(datum_text, id_temy))
				conn.commit()

		pocet_stranok -= 1

	generate_xml(id_subfora)
