package org.istanbus.core.service;

import org.istanbus.core.model.PathResult;

import java.util.List;

public interface PathFinderService {
    PathResult find(String fromStop, String toStop);
}
