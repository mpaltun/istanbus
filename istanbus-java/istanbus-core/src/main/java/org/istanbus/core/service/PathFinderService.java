package org.istanbus.core.service;

import org.istanbus.core.model.PathResult;

public interface PathFinderService {
    PathResult find(String fromBus, String toBus);
}
