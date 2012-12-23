package org.istanbus.core.service;

import org.istanbus.core.model.node.Stop;

public interface StopService
{
    Stop load(String stopCode);
}
