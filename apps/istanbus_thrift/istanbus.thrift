service IstanbusService {
  list<list<list<string>>> recommend(1:string from_stop, 2:string to_stop),
  string get_closest_stops(1:string latitude, 2:string longitude),
}
