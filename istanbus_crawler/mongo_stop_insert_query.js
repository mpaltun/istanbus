busList = db.bus.find({}, {_id: 0, stops_go: 1, stops_turn: 1});
busList.forEach(function (bus) {
    bus.stops_go.forEach(function (stop) {
        db.stop.insert(stop)
    });
    bus.stops_turn.forEach(function (stop) {
        db.stop.insert(stop)
    });
});
db.stop.ensureIndex({id: 1}, {unique: true, dropDups: true});