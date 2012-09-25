service IstanbusService {
  list<list<list<string>>> recommend(1:string from_stop, 2:string to_stop),
  list<list<string>> get_closest_stops(1:string latitude, 2:string longitude),
  list<list<string>> where_is_my_bus(1:string bus_name),
}
