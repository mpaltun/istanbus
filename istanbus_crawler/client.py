# -*- coding: utf-8 -*-

import httplib

class Client:
    def __init__(self, url):
        self.url = url

    def get(self, sub_url, params, headers):
        return self.request("GET", sub_url, params, headers)

    def request(self, method, sub_url, params, headers):
        retry = 3
        while (retry > 0):
            try:
                connection = httplib.HTTPConnection(self.url)
                connection.request(method, sub_url, params, headers)
                return connection.getresponse()
            except Exception:
                retry -= 1
        raise Exception("request failed after 3 retries")

    def post(self, sub_url, params, headers):
        return self.request("POST", sub_url, params, headers)

    def close(self):
        pass