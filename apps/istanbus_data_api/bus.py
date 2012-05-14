#!/usr/bin/python
#-*-coding: utf-8-*-

import urllib
import codecs
import lxml.html
import sys
import urlparse
from time import strftime
from db import MongoInstance
from client import Client
from char_replacer import CharReplacer

# connect to db
db_name = "istanbus_" + strftime("%Y-%m-%d") # i.e istanbus_2012-02-09
mongo_instance = MongoInstance(db_name)
# replacer
replacer = CharReplacer()

bus_stops_url = "/hatdetay.php"
bus_time_url = "/hatsaat.php"
stop_keyword = 'hatsaat.php?'

# prepare headers for .xml files
headers = {"Host" : "harita.iett.gov.tr", "Referer" : "http://harita.iett.gov.tr/XML/"}
client = Client("harita.iett.gov.tr:80")
client_mobile = Client("mobil.iett.gov.tr:80")

# output/bus.txt must be produced, check it!
f = codecs.open('output/bus.txt', encoding='utf-8')
for line in f:
    bus_list = line.split('|')
bus_summary_list = []
if (bus_list):
	for row in bus_list:
		if(row.strip() != ''):
			values = row.split(':')
			bus_code = values[0].strip()
			bus_name = values[1].strip()

			# replace x_chars
			x_bus_code = replacer.replace_x(bus_code)

			# get stops of bus from xml
			xml_name = "/XML/" + x_bus_code + "hatDurak.xml"
			response = client.get(xml_name, "", headers)
			parsed_xml = lxml.html.parse(response)
			for item in parsed_xml.getiterator('item'):
				stop = {}
				for child in item.getchildren():
					if (child.tag == 'description'):
						vals = child.text.split('aaa')
						stop['_id'] = vals[0]
						stop['code'] = vals[1]
						stop['u_desc'] = vals[2]
					stop[child.tag] = child.text
				mongo_instance.insert_stop2(stop)

			# encode turkish chars
			encoded_bus_code = replacer.encode(bus_code)

			# parse timesheet
			params = "hatcode=" + encoded_bus_code + "&ara=SAAT"
			response = client_mobile.post(bus_time_url, params, {"Content-Type" : "application/x-www-form-urlencoded"})
			bus_html = lxml.html.parse(response)

			# query (I will take this line out of the for loop some time. Hard job :D)
			time_xpath_query = '//table[@class="text"]//td[{0}]//font//text()'
			# time lists
			go_workday_time_list = bus_html.xpath(time_xpath_query.format('1'))
			go_saturday_time_list = bus_html.xpath(time_xpath_query.format('2'))
			go_sunday_time_list = bus_html.xpath(time_xpath_query.format('3'))
			come_workday_time_list = bus_html.xpath(time_xpath_query.format('4'))
			come_saturday_time_list = bus_html.xpath(time_xpath_query.format('5'))
			come_sunday_time_list = bus_html.xpath(time_xpath_query.format('6'))

			# here i am doing interesting things
			# normalize notes like […, 'hel', 'lo', 'wor', 'ld']
			notes = bus_html.xpath('//p//text()')
			#notes[len(notes)-2:len(notes)] = [' '.join(notes[len(notes)-2:len(notes)])]
			#notes[len(notes)-3:len(notes)-1] = [''.join(notes[len(notes)-3:len(notes)-1])]

			# this url is to get stops of the bus
			#request_url = bus_stops_url + encoded_bus_code
			# parse html in url

			params = "hatcode=" + encoded_bus_code + "&ara=DETAY"
			response = client_mobile.post(bus_stops_url, params, {"Content-Type" : "application/x-www-form-urlencoded"})
			bus_html = lxml.html.parse(response)
			# extract stop list
			stop_list = bus_html.xpath("//td/a/@href")[2:]

			go_stop_list = []
			turn_stop_list = []

			# get all stops
			for stop_url in stop_list:
				# exaple stop_url : hatsaat.php?hatcode=9ÜD&durak_kodu=A2387A&yon=G
				# we will substring after ? mark and parse the query string
				qs = stop_url[len(stop_keyword):]
				qs_params = urlparse.parse_qs(qs)
				stop_code = qs_params['durak_kodu'][0]
				direction = qs_params['yon'][0]
				if (direction == 'G'):
					go_stop_list.append(stop_code)
				elif (direction == 'D'):
					turn_stop_list.append(stop_code)

			bus = { "_id" : bus_code, "x_id" : x_bus_code,"encoded_id" : encoded_bus_code, "name" : bus_name, "stops_go" : go_stop_list, "stops_come" : turn_stop_list,
			"time" : {"workday_go" : go_workday_time_list, "saturday_go" : go_saturday_time_list, "sunday_go" : go_sunday_time_list,
			"workday_come" : come_workday_time_list, "saturday_come" : come_saturday_time_list, "sunday_come" : come_sunday_time_list
			}, "notes" : notes[2:]}
			bus_summary_list.append({"_id" : bus_code, "name" : bus_name})
			mongo_instance.insert_bus(bus)
			print bus_code.encode("utf-8"), ' inserted'

mongo_instance.insert_bus_list(bus_summary_list)

client.close()
client_mobile.close()

print 'done.'
