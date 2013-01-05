package org.istanbus.core.util;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;

import org.istanbus.core.model.node.Bus;
import org.istanbus.core.model.node.Stop;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

public class BusJsonParser
{
    public List<Bus> parse(String path)
    {
        FileReader fileReader = null;
        try
        {
            fileReader = new FileReader(path);
        }
        catch(FileNotFoundException e)
        {
            e.printStackTrace();
        }
        JsonArray busList = (JsonArray) new JsonParser().parse(fileReader);
        List<Bus> list = new ArrayList<Bus>();
        for(JsonElement element :  busList)
        {
            Bus b = new Bus();
            JsonObject bus = (JsonObject) element;
            String code = bus.get("id").getAsString();
            String name = bus.get("name").getAsString();
            JsonArray stopsTurnJson = (JsonArray) bus.get("stops_turn");
            List<Stop> stopsTurn = parseStops(stopsTurnJson);
            JsonArray stopsGoJson = (JsonArray) bus.get("stops_go");
            List<Stop> stopsGo = parseStops(stopsGoJson);
            
            b.setCode(code);
            b.setName(name);
            b.setStopsGo(stopsGo);
            b.setStopsTurn(stopsTurn);
            list.add(b);
        }
        return list; 
    }
    
    private List<Stop> parseStops(JsonArray stopList)
    {
        ArrayList<Stop> list = new ArrayList<Stop>();
        for(JsonElement jsonElement : stopList)
        {
            JsonObject o = (JsonObject) jsonElement;
            String code = o.get("id").getAsString();
            String name = o.get("name").getAsString();
            list.add(new Stop(code, name));
        }
        return list;
    }
    
    /*public static void main(String[] args) throws Exception
    {
        String jsonPath = "/Users/mustafa/bus.json";
        Type type = new TypeToken<List<Bus>>() {}.getType();
        long start = System.currentTimeMillis();
        List<Bus> list1 = new Gson().fromJson(new FileReader(jsonPath), type);
        System.out.println("loaded in :" + (System.currentTimeMillis() - start));
        start = System.currentTimeMillis();
        List<Bus> list = new BusJsonParser().parse(jsonPath);
        System.out.println("loaded in :" + (System.currentTimeMillis() - start));
    }*/

}
