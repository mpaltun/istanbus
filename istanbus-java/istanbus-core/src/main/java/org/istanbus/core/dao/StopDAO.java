package org.istanbus.core.dao;

import org.istanbus.core.model.node.Stop;

public interface StopDAO
{
    Stop loadByCode(String code);
}
