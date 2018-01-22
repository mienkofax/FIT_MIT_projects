#apt-get install python-dev libffi-dev libssl-dev
#pip install requests[security]

#
# @author Peter Tisovcik <xtisov00@fit.vutbr.cz
#

import tweepy
import sqlite3
import urllib2
import lxml.etree as ET

nazov_db = "twitter.db"
nazov_uctu = 'miuirom'
pocet_tweetov = 10

#funkcia vyexportuje do suboru databazu tweetov
def generate_xml():
	root = ET.Element("tweety")

	db.execute("SELECT * FROM tweety ORDER BY id DESC")
	result = db.fetchall()
	for tweet in result:
		x_tweet = ET.SubElement(root, "tweet")

		x_id = ET.SubElement(x_tweet, "id")
		x_id.text = tweet[0]

		x_tweet2 = ET.SubElement(x_tweet, "text")
		x_tweet2.text = tweet[1]

		x_datum = ET.SubElement(x_tweet, "datum")
		x_datum.text = tweet[2]

		x_odkazy = ET.SubElement(x_tweet, "odkazy")

		db.execute("SELECT odkaz FROM odkazy WHERE id_tweetu='%s'"%tweet[0])
		result = db.fetchall()
		for url in result:
			x_odkaz = ET.SubElement(x_odkazy, "odkaz")
			x_odkaz.text = url[0]

	tree = ET.ElementTree(root)
	tree.write("tweety.xml")
	print "SAVE: Tweety ulozene do xml suboru s nazvom: tweety.xml"

conn = sqlite3.connect(nazov_db)
db = conn.cursor()

# Create table
db.execute('''CREATE TABLE IF NOT EXISTS tweety
             (id TEXT PRIMARY KEY NOT NULL, tweet TEXT NOT NULL, datum TEXT NOT NULL)''')
db.execute('''CREATE TABLE IF NOT EXISTS odkazy
             (id INTEGER PRIMARY KEY NOT NULL, id_tweetu TEXT NOT NULL, odkaz TEXT NOT NULL)''')

#pristupove kody
CONSUMER_KEY = 'TkAgkV2DNvCsEagr8sosGxGEN'
CONSUMER_SECRET = 'pSkRu3315XUcfYWZwI7ziJLi0DoeTIagVOlMabti6Ah3EGgzP3'
ACCESS_TOKEN = '3240339293-OcEZ2kjcYDy6sKxE0PRH3I5AXu0AQ3tEmfkQLib'
ACCESS_TOKEN_SECRET = 'qmwhKhj37VRJS6ubJDTQhCmKRAZb9nZMe311IAgfP8p4t'

#autentifikacia
auth = tweepy.OAuthHandler(CONSUMER_KEY, CONSUMER_SECRET)
auth.set_access_token(ACCESS_TOKEN, ACCESS_TOKEN_SECRET)
api = tweepy.API(auth)

#vyber posledneho tweetu
db.execute("SELECT id FROM tweety ORDER BY datum DESC")
id_posdneho_tweetu = db.fetchone()

#overenie ci existuje zaznam v db ak ano urobi sa aktualizacia od posledneho tweetu
#ak neexistuje stiahne sa nastaveny pocet tweetov
if not id_posdneho_tweetu:
	public_tweets = api.user_timeline(screen_name=nazov_uctu, count=pocet_tweetov)
else:
	public_tweets = api.user_timeline(screen_name=nazov_uctu, since_id=int(id_posdneho_tweetu[0]))
	print "Od poslednej aktualizacie neboli pridane nove twety."

#stahovanie tweetov
for tweet in public_tweets:

	#vlozenie udajov o tweete do db
	db.execute("INSERT INTO tweety VALUES ('%s', '%s', '%s')"%(tweet.id, tweet.text.replace("\'", "\""), tweet.created_at))
	conn.commit()
	print "-------------Tweet-----------"
	print "ID: %s"%tweet.id
	print "Datum: %s"%tweet.created_at
	print "Nazov: %s"%tweet.text

	#vlozenie udajov o adresach do db a ich nasledovne stiahnutie
	for url in tweet.entities['urls']:

		#ulozenie odkazov do db
		url_string = str(url['expanded_url'])
		db.execute("INSERT INTO odkazy VALUES (NULL, '%s', '%s')"%(tweet.id, url_string))
		conn.commit()

		#vyber id odkazu aby sme mohli vytvorit subor s danym nazvom
		db.execute("SELECT id FROM odkazy WHERE id_tweetu='%s' AND odkaz='%s'"%(tweet.id, url_string))
		id_odkazu = db.fetchone()[0]

		#stiahnutie stranky z tweetu a jej ulozenie do suboru
		try:
			response = urllib2.urlopen(url_string)
		except:
			print "ERR: Zadana adresa neexistuje."
			continue

		data = response.read()
		response.close()

		nazov_suboru = "%s.html"%id_odkazu
		with open(nazov_suboru, 'w') as fid:
			fid.write(data)

	print ""
generate_xml()
