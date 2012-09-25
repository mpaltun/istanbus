#!/usr/bin/env python
#-*-coding: utf-8-*-

import sys
sys.path.append('./gen-py')

from istanbus import IstanbusService
from istanbus.ttypes import *

from thrift.transport import TSocket
from thrift.transport import TTransport
from thrift.protocol import TBinaryProtocol
from thrift.server import TServer

from recommender import Recommender
from closest_stops_finder import Closest_stops_finder
from where_is_my_bus import Where_is_my_bus
from db import MongoInstance

recommender = Recommender()
closest_stops_finder = Closest_stops_finder()
where_is_my_bus_module = Where_is_my_bus()
#closest_stops_finder.find(29.007735,41.0993)

db_name = "istanbus_2012-08-15"
mongo_instance = MongoInstance(db_name)
mongo_instance.ensure_index_recommend("id")
class IstanbusServiceHandler:
    def __init__(self, recommender, closest_stops_finder, where_is_my_bus_module):
        self.recommender = recommender
        self.closest_stops_finder = closest_stops_finder
        self.where_is_my_bus_module = where_is_my_bus_module

    def recommend(self, from_stop, to_stop):
        print 'request received: ', from_stop + ' -> ' + to_stop
        results = self.recommender.recommend(from_stop, to_stop)
        mongo_instance.insert_recommendation(from_stop, to_stop, results)
        return "ok"

    def get_closest_stops(self, latitude, longitude):
        return self.closest_stops_finder.find(latitude, longitude)

    def where_is_my_bus(self, bus_name):
        return self.where_is_my_bus_module.find(bus_name)



handler = IstanbusServiceHandler(recommender, closest_stops_finder, where_is_my_bus_module)
processor = IstanbusService.Processor(handler)
transport = TSocket.TServerSocket(port=9090)
tfactory = TTransport.TBufferedTransportFactory()
pfactory = TBinaryProtocol.TBinaryProtocolFactory()

#server = TServer.TSimpleServer(processor, transport, tfactory, pfactory)

# You could do one of these for a multithreaded server
#server = TServer.TThreadedServer(processor, transport, tfactory, pfactory)
server = TServer.TThreadPoolServer(processor, transport, tfactory, pfactory)

print 'Starting the server...'
server.serve()
print 'done.'
client.close()
