service IstanbusJavaService {
  string recommend(1:string from_stop, 2:string to_stop),
  string search(1:string index, 2:string keyword),
}