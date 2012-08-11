# -*- coding: utf-8 -*-
import lxml.html
from client import Client
import re

class Closest_stops_finder:
    def __init__(self):
        self.client = Client("harita.iett.gov.tr:80")
        self.headers = {}
        self.params = "spot={},{},undefined"
        self.url = '/AdresdenDurakBul.php?'

    def find(self, latitude, longitude):
        response = self.client.get(self.url + self.params.format(longitude, latitude), "" , self.headers)
        parsed_xml = lxml.html.parse(response, lxml.html.HTMLParser(encoding="utf-8"))

        a_links = parsed_xml.xpath('//a[not(img)]/@href')
        stops = []
        for a in a_links:
            results = re.compile("'",re.U).split(a)
            stop =  [results[1].encode('utf-8'), results[3].encode('utf-8'),
                        results[5].encode('utf-8'), results[7].encode('utf-8')]
            stops.append(stop)
        return stops
