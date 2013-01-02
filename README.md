istanbus
========
istanbus rest api

Routes
-----

### Bus

```
GET /bus/{id}
GET /bus/{id}/stops/go
GET /bus/{id}/stops/come
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
```


### Solutions
```
GET /howtogo/from/{stop_id}/to/{stop_id}
```


### Closest stops
```
GET /closest/lat/{latitude}/lon/{longitude}
```
