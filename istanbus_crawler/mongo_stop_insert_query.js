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

db.stop.find().forEach(function (stop) {
	var bus_list = db.bus.find({ $or: [ { "stops.turn.id" : stop.id }, { "stops.go.id" : stop.id } ]},
		{id : 1, _id : 0, name : 1}).toArray();
	stop.bus = [];
	for (var i = 0; i < bus_list.length; i++) {
		var bus = bus_list[i];
		stop.bus.push(bus);
	};
	db.stop.update({_id : stop._id}, stop);
});