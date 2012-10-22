// run this before execute
// mongoexport  --jsonArray -d istanbus_2012-08-15 -c stop -f 'name,id' -o stops.json

var request = require('request');
var stops = require('./stops.json')

var reps = {
  'ü': "u",
  'ı': "i",
  'ş': "s",
  'ç': "c",
  'ö': "o",
  'ğ': "g"
};

function replaceTurkishChars(lowercaseStop) {
  var result = [];
  for (var i in lowercaseStop) {
    var c = lowercaseStop[i];
    result.push(reps[c] || c);
  }
  return result.join('');
};

for(var i in stops) {
  var stop = stops[i];
  delete stop._id;

  stop.keyword = stop.name + ' ' + replaceTurkishChars(stop.name.toLowerCase());

  var options = {
    uri: 'http://localhost:9200/istanbus/stop/' + stop.id,
    method: 'PUT',
    json: stop
  };
  
  request(options, function (error, response, body) {
    if (!error && response.statusCode == 200) {
      console.log(body) // Print the shortened url.
    }
  });

}

