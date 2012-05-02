/*struct Stop {
  1: string name,
  2: string code,
  3: string latitude,
  4: string longitude,
}*/

service IstanbusService {
  list<list<list<string>>> recommend(1:string from_stop, 2:string to_stop),
  list<list<string>> get_closest_stops(1:string latitude, 2:string longitude),
}
