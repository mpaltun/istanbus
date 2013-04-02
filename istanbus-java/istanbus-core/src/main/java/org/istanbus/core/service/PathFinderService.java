package org.istanbus.core.service;

import org.istanbus.core.model.PathResult;
import org.istanbus.core.model.node.Bus;

import java.util.List;

public interface PathFinderService {
    List<Bus> find(String fromBus, String toBus);
}
