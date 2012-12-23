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

# connect to db
db_name = "istanbus_" + strftime("%Y-%m-%d") # i.e istanbus_2012-02-09
mongo_instance = MongoInstance(db_name)

# ensure index for search
mongo_instance.ensure_index_stop([("id", True), ("words", False)])

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
        busofstophtml = lxml.html.parse(response)

        # query bus code
        bus_codes = busofstophtml.xpath("//table//td[1]/text()")[1:]

        if len(bus_codes) > 0:
            bus_names = busofstophtml.xpath("//table//td[2]/span/text()")
            bus_list = []
            index = 0
            for bus_code in bus_codes:
                bus = {"id" : bus_code.encode("ISO-8859-1"), "name" : bus_names[index].encode('ISO-8859-1')}
                bus_list.append(bus)
                index += 1
        
            stop = { "id" : stopcode, "name" : stopname, "bus_list" : bus_list }
            mongo_instance.insert_stop(stop)
            print stop['id'], " inserted"
        else:
            print stopcode, " has no bus"

client.close()
print "done."
