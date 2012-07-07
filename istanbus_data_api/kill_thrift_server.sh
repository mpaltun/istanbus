#!/bin/bash

ps aux | grep "python ./istanbus_thrift_server.py" | awk '{print $2}' | xargs kill -9
