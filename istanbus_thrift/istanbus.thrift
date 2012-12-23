service IstanbusJavaService {
  string recommend(1:string from_stop, 2:string to_stop),
  string stop_search(1:string keyword),
}