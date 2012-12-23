package org.istanbus.core.service;

import org.istanbus.core.model.Transport;

import java.util.List;

public interface PathFinderService {
    List<Transport> find(String fromStop, String toStop);
}
