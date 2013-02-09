// extract stops
db.bus.find({}, {_id: 0, stops: 1}).forEach(function (bus) {
    var stops = bus.stops.go.concat(bus.stops.turn);
    stops.forEach(function (stop) {
        db.stop.insert(stop);
    });
});

// ensure stop id index
db.stop.ensureIndex({id: 1}, {unique: true, dropDups: true});

// set locations to stops
db.stop2.find().forEach(function (stop2) {
    db.stop.update({id : stop2.id}, { $set: { 'location': stop2.location } });
});

// add location index to stops
db.stop.ensureIndex( { location : "2d" } );

// remove stop location collection
db.stop2.drop();

// set bus list of stops
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

// set locations of stops inside bus collection
db.bus.find().forEach(function(bus) {
	var stops = bus.stops;
	
	var stops_go = stops.go;
	var stops_turn = stops.turn;

	// iterate over outgoing stops
	for (var i in stops_go) {
		var stop = stops_go[i];
		var stop_with_location = db.stop.findOne({id : stop.id});
		if (stop_with_location) {
			stop.location = stop_with_location.location;
		}
	}

	// same here
	for (var i in stops_turn) {
		var stop = stops_turn[i];
		var stop_with_location = db.stop.findOne({id : stop.id});
		if (stop_with_location) {
			stop.location = stop_with_location.location;
		}
	}

	// update bus
	db.bus.save(bus);

});