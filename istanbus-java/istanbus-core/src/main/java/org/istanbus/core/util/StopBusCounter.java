package org.istanbus.core.util;

import org.istanbus.core.model.node.Bus;
import org.istanbus.core.model.node.Stop;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class StopBusCounter {
    private List<Bus> busList;
    private Map<String, Integer> counts;

    public StopBusCounter(List<Bus> busList) {
        this.busList = busList;
        this.counts = new HashMap<String, Integer>();
    }


    public void index() {
        for (Bus bus : busList) {
            Set<String> ids = getUniqueStopIds(bus);
            for (String id : ids) {
                Integer count = counts.get(id);
                if (count == null) {
                    count = 0;
                }
                counts.put(id, count + 1);
            }
        }
    }

    public int getBusCount(Stop stop) {
        return counts.get(stop.getId());
    }

    private Set<String> getUniqueStopIds(Bus bus) {
        Set<String> ids = new HashSet<String>();
        for (Stop stop : bus.getStopsGo()) {
            ids.add(stop.getId());
        }

        for (Stop stop : bus.getStopsTurn()) {
            ids.add(stop.getId());
        }

        return ids;
    }

}
