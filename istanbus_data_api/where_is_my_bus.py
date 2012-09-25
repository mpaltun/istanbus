# -*- coding: utf-8 -*-
import lxml.html
from client import Client
import re

class Where_is_my_bus:
    def __init__(self):
        self.client = Client("3n.iett.gov.tr:80")
        self.headers = {"Referer" : "http://3n.iett.gov.tr/", "User-Agent" : "Mozilla/5.0 (iPhone; U; CPU iPhone OS 3_0 like Mac OS X; en-us) AppleWebKit/528.18 (KHTML, like Gecko) Version/4.0 Mobile/7A341 Safari/528.16"}
        self.params = "hat={}&sira={}"
        self.url = '/?'

    def find(self, bus_id, order=""):
        response = self.client.get(self.url + self.params.format(bus_id, order), "" , self.headers)
        parsed_xml = lxml.html.parse(response, lxml.html.HTMLParser(encoding="utf-8"))
        info_list = parsed_xml.xpath("/html/body//text()")
        search_result = []
        for info in info_list:
            result = re.search("^(.*) numaral. otob.s (.*) olup (.*) saniye .nce (.*) y.n.nde (.*)nolu (.*) dura..n(.*)", info)
            if result is not None:
                bus_info = result.groups()
                distance_result = re.search("a (.*) metre  mesafedeydi", bus_info[6])
                distance = 0
                if distance_result is not None:
                    distance = int(distance_result.group(1))
                bus = [
                    int(bus_info[0]),   # order
                    bus_info[1].strip(),# type
                    int(bus_info[2]),   # seconds
                    bus_info[3],        # destination
                    bus_info[5].strip(),# stop name
                    distance            # distance to stop (as meter)
                ]
                search_result.append(bus)
        return search_result
