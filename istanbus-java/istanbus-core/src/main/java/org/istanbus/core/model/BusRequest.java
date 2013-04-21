package org.istanbus.core.model;

import org.istanbus.core.dao.StopDAO;

/**
 * Created with IntelliJ IDEA.
 * User: mustafa
 */
public class BusRequest
{
    private String fromBusId;
    private String toBusId;

    private StopDAO stopDAO;

    public BusRequest(String fromBusId, String toBusId, StopDAO stopDAO)
    {
        this.fromBusId = fromBusId;
        this.toBusId = toBusId;
        this.stopDAO = stopDAO;
    }

    public String getFromBusId()
    {
        return fromBusId;
    }

    public String getToBusId()
    {
        return toBusId;
    }

    public StopDAO getStopDAO()
    {
        return stopDAO;
    }

}
