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
        for info in info_list:
            result = re.search("^(.*) numaral. otob.s (.*) olup (.*) saniye .nce (.*) y.n.nde (.*) s.ra nolu (.*) dura..(.*)", info)
            if result is not None:
                print result.group(7) 

module = Where_is_my_bus()
module.find('110');
