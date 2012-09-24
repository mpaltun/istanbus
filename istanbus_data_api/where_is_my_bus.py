# -*- coding: utf-8 -*-
import lxml.html
from client import Client
import re

class Where_is_my_bus:
    def __init__(self):
        self.client = Client("3n.iett.gov.tr:80")
        self.headers = {"Referer" : "http://3n.iett.gov.tr/", "User-Agent" : "curl/7.22.0 (i686-pc-linux-gnu) libcurl/7.22.0 OpenSSL/1.0.1 zlib/1.2.3.4 libidn/1.23 librtmp/2.3"}
        self.params = "hat={}&sira={}"
        self.url = '/?'

    def find(self, bus_id, order=""):
        response = self.client.get(self.url + self.params.format(bus_id, order), "" , self.headers)
        parsed_xml = lxml.html.parse(response, lxml.html.HTMLParser(encoding="utf-8"))
        info_list = parsed_xml.xpath('//*[contains(text(),"otob")]/text()')
        for info in info_list:
          print info

module = Where_is_my_bus()
module.find('103');