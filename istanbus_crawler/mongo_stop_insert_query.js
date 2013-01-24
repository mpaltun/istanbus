db.bus.find({}, {_id: 0, stops: 1}).forEach(function (bus) {
    var stops = bus.stops.go.concat(bus.stops.turn);
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