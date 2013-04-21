package org.istanbus.core.dao;

import org.istanbus.core.model.node.Stop;

import java.util.List;

public interface StopDAO
{
    List<Stop> loadAll();

    Stop loadById(String id);
}
