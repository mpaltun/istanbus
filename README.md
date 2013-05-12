istanbus
========
istanbus rest api

Routes
-----

### Bus

```
GET /bus/{id}
GET /bus/{id}/stops/go
GET /bus/{id}/stops/turn
GET /bus/{id}/timesheet
```

### Stop

```
GET /stop/{id}
```


### Search

```
GET /search/bus/{id or keyword}
GET /search/stop/{keyword}
GET /place/stop/{keyword}
```


### Path Finding
```
GET /path/from/{stop_id}/to/{stop_id}
GET /path/from/lat/{from_latitude}/lon/{from_longitude}/to/lat/{to_latitude}/lon/{to_longitude}
```


### Closest stops
```
GET /closest/lat/{latitude}/lon/{longitude}
```

### Places
```
GET /place/{id}
```
