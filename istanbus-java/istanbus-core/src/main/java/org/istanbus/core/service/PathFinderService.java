package org.istanbus.core.service;

import org.istanbus.core.model.Route;
import org.istanbus.core.model.SuggestedRoute;

import java.util.List;

public interface PathFinderService {
    List<SuggestedRoute> find(String fromBus, String toBus);
}
