package org.istanbus.core.dao;

import org.istanbus.core.model.node.Bus;

import java.util.List;

public interface BusDAO {
    List<Bus> loadAllBuses();
}
