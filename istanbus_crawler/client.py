# -*- coding: utf-8 -*-

import httplib

class Client:
    def __init__(self, url):
        self.url = url

    def get(self, sub_url, params, headers):
        try:
            return self.get_request(sub_url, params, headers)
        except Exception:
            #try once again
            return self.get_request(sub_url, params, headers)

    def get_request(self, sub_url, params, headers):
        connection = httplib.HTTPConnection(self.url)
        connection.request("GET", sub_url, params, headers)
        return connection.getresponse()

    def post(self, sub_url, params, headers):
        connection = httplib.HTTPConnection(self.url)
        connection.request("POST", sub_url, params, headers)
        return connection.getresponse()

    def close(self):
        pass