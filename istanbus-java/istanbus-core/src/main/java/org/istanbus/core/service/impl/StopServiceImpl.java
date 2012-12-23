package org.istanbus.core.service.impl;

import com.google.inject.Inject;

import org.istanbus.core.dao.StopDAO;
import org.istanbus.core.model.node.Stop;
import org.istanbus.core.service.StopService;

public class StopServiceImpl implements StopService
{
    private StopDAO stopDAO;

    @Inject
    public StopServiceImpl(StopDAO stopDAO)
    {
        this.stopDAO = stopDAO;
    }
    
    @Override
    public Stop load(String stopCode)
    {
        return stopDAO.loadByCode(stopCode);
    }
    
    
}
