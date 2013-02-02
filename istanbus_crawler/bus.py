#!/usr/bin/python
#-*-coding: utf-8-*-

import codecs
import urlparse
import re
from time import strftime

import lxml.html
from slugify import slugify

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
headers = {"Host": "harita.iett.gov.tr", "Referer": "http://harita.iett.gov.tr/XML/"}
client = Client("harita.iett.gov.tr:80")
client_mobile = Client("mobil.iett.gov.tr:80")

mongo_instance.ensure_index_bus('id')
def parse_href(href):
    # href is like JavaScript:hattahmin('Ş0026','ŞİŞHANE 6')
    result = re.search("JavaScript:hattahmin\('(.*)','(.*)'\)", href)
    # stop_id = result.group(1)
    stop_name = result.group(2)
    stop_id = slugify(stop_name)
    return {"id": stop_id, "name": stop_name.encode('utf-8')}


def parse_stop_id_from_href(href):
    # href is like javascript:top.ajaxGet('durak_hat_listesi_v3.php?dadi=ATATU:RK HAVALI:MANI&durak=U0008','durak');haritagoster('durak');
    return re.search("(.*)&durak=(.*)','(.*)", href).group(2)

def append_to_stop_list(stop_list, stop):
    not_found = True;
    for s in stop_list:
        if s['id'] == stop["id"]:
            not_found = False
            break
    if not_found:
        stop_list.append(stop)


def process_stop_xml(bus_code, stop_ids):
    # replace x_chars
    x_bus_code = replacer.replace_x(bus_code)

    # get stops of bus from xml
    xml_name = '/XML/' + x_bus_code + "hatDurak.xml"
    response = client.get(xml_name, "", headers)
    parsed_xml = lxml.html.parse(response)
    stop_list = []
    for item in parsed_xml.getiterator('item'):
        stop = {}
        for child in item.getchildren():
            if (child.tag == 'description'):
                vals = child.text.split('aaa')
                #stop['id2'] = vals[1]
                #stop['u_desc'] = vals[2]
            tag = child.tag
            if (tag == 'long'):
                tag = 'longitude'
            elif (tag == 'lat'):
                tag = 'latitude'
            elif (tag == 'title'):
                tag = 'name'
            if (tag != 'description'):
                stop[tag] = child.text

        location_data = [float(stop['latitude']), float(stop['longitude'])]
        del stop['longitude'];
        del stop['latitude']

        stop_name = stop['name']
        del stop['name']
        stop_id = slugify(unicode(stop_name))

        stop['id'] = stop_ids[stop_id]
        stop['location'] = location_data
        stop_list.append(stop)
    if stop_list:
        mongo_instance.insert_bulk_stop2(stop_list)
    else:
        print 'warning: ', bus_code.encode("utf-8"), ' has no stops'

# output/bus.txt must be produced, check it!
f = codecs.open('output/bus.txt', encoding='utf-8')
for line in f:
    bus_list = line.split('|')
if bus_list:
    for row in bus_list:
        if row.strip() != '':
            values = row.split(':')
            bus_code = values[0].strip()
            bus_name = values[1].strip()

            # encode turkish chars
            encoded_bus_code = replacer.encode(bus_code)

            # parse timesheet
            params = "hatcode=" + encoded_bus_code + "&ara=SAAT"
            response = client_mobile.post(bus_time_url, params, {"Content-Type": "application/x-www-form-urlencoded"})
            bus_html = lxml.html.parse(response)

            # query (I will take this line out of the for loop some time. Hard job :D)
            time_xpath_query = '//table[@class="text"]//td[{0}]//font//text()'
            # time lists
            go_weekdays_timesheet = bus_html.xpath(time_xpath_query.format('1'))
            go_saturday_timesheet = bus_html.xpath(time_xpath_query.format('2'))
            go_sunday_timesheet = bus_html.xpath(time_xpath_query.format('3'))
            turn_weekdays_timesheet = bus_html.xpath(time_xpath_query.format('4'))
            turn_saturday_timesheet = bus_html.xpath(time_xpath_query.format('5'))
            turn_sunday_timesheet = bus_html.xpath(time_xpath_query.format('6'))

            # here i am doing interesting things
            # normalize notes like […, 'hel', 'lo', 'wor', 'ld']
            notes = bus_html.xpath('//p//text()')

            # this url is to get stops of the bus
            #request_url = bus_stops_url + encoded_bus_code
            # parse html in url

            params = "hatcode=" + encoded_bus_code + "&ara=DETAY"
            response = client_mobile.post(bus_stops_url, params, {"Content-Type": "application/x-www-form-urlencoded"})
            bus_html = lxml.html.parse(response)
            # extract stop list
            stop_list = bus_html.xpath("//td/a")[3:]

            go_stop_list = []
            turn_stop_list = []

            # get all stops
            stop_ids = {}
            for stop_element in stop_list:
                # example stop_url : hatsaat.php?hatcode=9ÜD&durak_kodu=A2387A&yon=G
                # we will substring after ? mark and parse the query string
                stop_url = stop_element.get('href')
                stop_name = stop_element.text.strip()

                district = stop_element.getchildren()[0].text
                if not district:
                    district = stop_name
                district = district.replace(u'I', u'ı').title()

                qs = stop_url[len(stop_keyword):]
                qs_params = urlparse.parse_qs(qs)

                direction = qs_params['yon'][0]

                slugified_stop_name = slugify(unicode(stop_name))
                stop_id = slugified_stop_name + '-' + slugify(unicode(district).replace(u'ı', u'i'))
                stop_ids[slugified_stop_name] = stop_id

                stop_summary = {"id": stop_id, "name": stop_name, "district": district}
                if (direction == 'G'):
                    append_to_stop_list(go_stop_list, stop_summary)
                elif (direction == 'D'):
                    append_to_stop_list(turn_stop_list, stop_summary)

            # some bus have zero turn stops
            if not turn_stop_list:
                # if this is one of them, then reverse go list
                turn_stop_list = go_stop_list[::-1]

            process_stop_xml(bus_code, stop_ids)

            weekdays_timesheet = { "go" : go_weekdays_timesheet, "turn" : turn_weekdays_timesheet};
            sunday_timesheet = { "go" : go_sunday_timesheet, "turn" : turn_sunday_timesheet};
            saturday_timesheet = { "go" : go_saturday_timesheet, "turn" : turn_saturday_timesheet};

            stops = { "go" : go_stop_list, "turn" : turn_stop_list}
            bus = {
                    "id" : bus_code,
                   "name": bus_name,
                   "stops": stops,
                   "timesheet": {
                       "weekdays": weekdays_timesheet,
                       "saturday" : saturday_timesheet,
                       "sunday" : sunday_timesheet
                    },
                   "notes": notes[2:]
            }
            mongo_instance.insert_bus(bus)
            print bus_code.encode("utf-8"), ' inserted'

client.close()
client_mobile.close()

print 'done.'
