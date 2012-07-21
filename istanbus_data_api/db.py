# -*- coding: utf-8 -*-

from pymongo import Connection
from pymongo import ASCENDING

class MongoInstance:
    def __init__(self, db_name):
        print "trying to connect mongo at localhost"
        connection = Connection()
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

    def ensure_index_bus(self, column_name):
        self.db.bus.ensure_index(column_name, ASCENDING, unique=True)

    def ensure_index_recommend(self, column_name):
        self.db.howtogo.ensure_index(column_name, ASCENDING, unique=True)
    
    def insert_recommendation(self, from_stop, to_stop, recommendations):
        self.db.howtogo.insert({"id" : from_stop + "_" + to_stop, "recommendations" : recommendations})
