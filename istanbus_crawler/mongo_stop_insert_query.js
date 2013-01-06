db.bus.find({}, {_id: 0, stops_go: 1, stops_turn: 1}).forEach(function (bus) {
    var stops = bus.stops_go.concat(bus.stops_turn);
    stops.forEach(function (stop) {
        db.stop.insert(stop);
    });
});
db.stop.ensureIndex({id: 1}, {unique: true, dropDups: true});

db.stop2.find().forEach(function (stop2) {
    db.stop.update({id : stop2.id}, { $set: { 'location': stop2.location } });
});
db.stop.ensureIndex( { location : "2d" } );
db.stop2.drop();