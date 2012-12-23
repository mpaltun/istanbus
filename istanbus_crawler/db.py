# -*- coding: utf-8 -*-

from pymongo import Connection, GEO2D
from pymongo import ASCENDING

class MongoInstance:
    def __init__(self, db_name):
        print "trying to connect mongo at localhost"
        connection = Connection('linode', 27017)
        print "connected"
        self.db = connection[db_name]

    def insert_bus(self, bus):
        self.db.bus.insert(bus)
        # print 'bus: ', bus['_id'], ' inserted'

    def insert_stop(self, stop):
        self.db.stop.insert(stop)
        # print 'stop: ', stop['name'], ' inserted'

    def insert_stop2(self, stop):
        self.db.stop2.insert(stop)
        # print 'stop: ', stop['name'], ' inserted'

    def insert_bulk_stop2(self, stop_list):
        self.db.stop2.insert(stop_list)

    def ensure_index_stop(self, column_names):
        for (column_name, uniq) in column_names:
            self.db.stop.ensure_index(column_name, ASCENDING, unique=uniq)

    def ensure_index_bus(self, column_name, uniq=True):
        self.db.bus.ensure_index(column_name, ASCENDING, unique=uniq)

    def create_index_stop_location(self, col):
        self.db.stop2.create_index([(col, GEO2D)])

    def ensure_index_stop2(self, column_name):
        self.db.stop2.ensure_index(column_name, ASCENDING, unique=True)