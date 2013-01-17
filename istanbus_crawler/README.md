istanbus crawler
==

dependencies
====
```
sudo apt-get install mongodb  
sudo pip install slugify  
sudo pip install pymongo
```

start crawling
====
```
cd istanbus_crawler  
mkdir output  
./producebustext.sh  
./bus.py
```

extract stops (this part will be better)
==

```
mongo  
use istanbus_2013-01-17  
```
copy paste js code from ```mongo_stop_insert_query.js``` to mongo shell

done.
