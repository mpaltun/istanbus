#!/usr/bin/python
#-*-coding: utf-8-*-

import urllib
import codecs
import lxml.html
import sys
import urlparse
import re
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

mongo_instance.ensure_index_bus('id')

def parse_href(href):
    # href is like JavaScript:hattahmin('Ş0026','ŞİŞHANE 6')
    result = re.search("JavaScript:hattahmin\('(.*)','(.*)'\)", href)
    stop_id = result.group(1)
    stop_name = result.group(2)
    return {"id" : stop_id, "name": stop_name.encode('utf-8')}

def parse_stop_id_from_href(href):
    # href is like javascript:top.ajaxGet('durak_hat_listesi_v3.php?dadi=ATATU:RK HAVALI:MANI&durak=U0008','durak');haritagoster('durak');
    return re.search("(.*)&durak=(.*)','(.*)", href).group(2)
    
# output/bus.txt must be produced, check it!
f = codecs.open('output/bus.txt', encoding='utf-8')
for line in f:
    bus_list = line.split('|')
if (bus_list):
    for row in bus_list:
        if(row.strip() != ''):
            values = row.split(':')
            bus_code = values[0].strip()
            bus_name = values[1].strip()
            
            # if (bus_code != "_M1"):
            #    continue 
            
            # replace x_chars
            x_bus_code = replacer.replace_x(bus_code)

            # get stops of bus from xml
            xml_name = "/XML/" + x_bus_code + "hatDurak.xml"
            response = client.get(xml_name, "", headers)
            parsed_xml = lxml.html.parse(response)
            stop2_list = []
            for item in parsed_xml.getiterator('item'):
                stop = {}
                for child in item.getchildren():
                    if (child.tag == 'description'):
                        vals = child.text.split('aaa')
                        stop['id'] = vals[0]
                        stop['code'] = vals[1]
                        stop['u_desc'] = vals[2]
                    stop[child.tag] = child.text
                stop2_list.append(stop)
            if (stop2_list != []):
                mongo_instance.insert_bulk_stop2(stop2_list)
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
            stop_list = bus_html.xpath("//td/a")[3:]
            
            go_stop_list = []
            turn_stop_list = []

            # get all stops
            for stop_element in stop_list:
                # exaple stop_url : hatsaat.php?hatcode=9ÜD&durak_kodu=A2387A&yon=G
                # we will substring after ? mark and parse the query string
                stop_url = stop_element.get('href')
                stop_name = stop_element.text
                
                # district ie Cekmekoy -> stop_element.getchildren()[0].text.encode('utf8')
                qs = stop_url[len(stop_keyword):]
                qs_params = urlparse.parse_qs(qs)
                stop_code = qs_params['durak_kodu'][0]
                # A2387A -> A2387
                stop_code = stop_code[:-1]
                direction = qs_params['yon'][0]
                stop_summary = {"id" : stop_code, "name" : stop_name.strip()}
                if (direction == 'G'):
                    go_stop_list.append(stop_summary)
                elif (direction == 'D'):
                    turn_stop_list.append(stop_summary)

            if (len(stop_list) == 0):
                params = "sorgu=durak&hat=" + encoded_bus_code
                response = client.get("/hat_sorgula_v3.php3?" + params,"" , {})
                bus_html = lxml.html.parse(response, lxml.html.HTMLParser(encoding="utf-8"))
                hrefs_go = bus_html.xpath('//table//td[1]/table//table/tr/td[3]/a[1]/@href')
                hrefs_come = bus_html.xpath('//table//td[2]/table//table/tr/td[3]/a[1]/@href')
                i = 2 
                for href_go in hrefs_go:
                    stop_summary = parse_href(href_go)
                    if (len(stop_summary['id']) == 0):
                        stop_summary['id'] = parse_stop_id_from_href(bus_html.xpath('//table//td[1]/table//table/tr[' + str(i) + ']/td[3]/a[5]/@href')[0])
                        stop_summary['name'] = bus_html.xpath('//table//td[1]/table//table/tr[' + str(i) +']/td[2]/span[1]/text()')[0]
                    go_stop_list.append(stop_summary)
                    i+=1
                i = 2 
                for href_come in hrefs_come:
                    stop_summary = parse_href(href_come)
                    if (len(stop_summary['id']) == 0):
                        stop_summary['id'] = parse_stop_id_from_href(bus_html.xpath('//table//td[2]/table//table/tr[' + str(i) + ']/td[3]/a[5]/@href')[0])
                        stop_summary['name'] = bus_html.xpath('//table//td[2]/table//table/tr[' + str(i) +']/td[2]/span[1]/text()')[0]
                    turn_stop_list.append(stop_summary)
                    i+=1

            bus = { "id" : bus_code, "x_id" : x_bus_code,"encoded_id" : encoded_bus_code, "name" : bus_name, "stops_go" : go_stop_list, "stops_come" : turn_stop_list,
            "time" : {"workday_go" : go_workday_time_list, "saturday_go" : go_saturday_time_list, "sunday_go" : go_sunday_time_list,
            "workday_come" : come_workday_time_list, "saturday_come" : come_saturday_time_list, "sunday_come" : come_sunday_time_list
            }, "notes" : notes[2:]}
            mongo_instance.insert_bus(bus)
            print bus_code.encode("utf-8"), ' inserted'

client.close()
client_mobile.close()

print 'done.'
