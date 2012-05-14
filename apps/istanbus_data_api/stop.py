#!/usr/bin/python
# -*- coding: utf-8 -*-

import lxml.html
import urlparse
import sys
from time import strftime
from db import MongoInstance
from client import Client
from char_replacer import CharReplacer
import urllib
import re


# connect to db
db_name = "istanbus_" + strftime("%Y-%m-%d") # i.e istanbus_2012-02-09
mongo_instance = MongoInstance(db_name)

# ensure index for search
mongo_instance.ensure_index_stop("words")

#replacer
replacer = CharReplacer()

# parse stop.html with xpath
try:
	html = lxml.html.parse("output/stop.html")
except IOError:
	print "stop.html not found."
	print "execute producestophtml.sh bash script before run this"
	print "exit"
	sys.exit()

stops = html.xpath("//td/a/@href")

# http client
headers = {"Host" : "harita.iett.gov.tr", "Referer" : "http://harita.iett.gov.tr/XML/"}
client = Client("harita.iett.gov.tr:80")

# parse stops
# output format -> stop code::stopname
stopskeyword = "gecenhatlar.php?"
busofstopkeyword ="dgecenhatlar.php?"
busresultkeyword = 'hatdetay.php?'

busofstopurl = "/durak_hat_listesi_v3.php?"

for stop in stops:
	if stop.startswith(stopskeyword):
		qs = urlparse.parse_qs(stop[len(stopskeyword):])
		stopcode = qs["durak"][0].encode("utf8")
		stopname = qs["durakname"][0].encode("utf8")

		# request bus list that goes through this stop
		param = urllib.urlencode({'durak': stopcode})
		response = client.get(busofstopurl + param, "", headers)
		busofstophtml = lxml.html.parse(response, lxml.html.HTMLParser(encoding="utf-8"))

		# query bus code
		busresults = busofstophtml.xpath("//table//td[1]/text()")[1:]

		if len(busresults) > 0:
			# text search improvement
			words = re.compile('\W+',re.U).split(stopname.decode('utf-8'))
			lower_words = []
			for word in words:
				if (word != ''):
					lower_words.append(word.lower())

			stop = { "_id" : stopcode, "name" : stopname, "bus_list" : busresults, "words" : lower_words }
			mongo_instance.insert_stop(stop)
			print stop['_id'], " inserted"
		else:
			print stopcode, " has no bus"

client.close()
print "done."
