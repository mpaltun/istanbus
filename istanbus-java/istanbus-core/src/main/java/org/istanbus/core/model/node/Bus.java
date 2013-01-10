package org.istanbus.core.model.node;

import java.util.List;

public class Bus
{
    private String id;
    private String name;
    private List<Stop> stopsGo;
    private List<Stop> stopsTurn;

    public String getId()
    {
        return id;
    }

    public void setId(String id)
    {
        this.id = id;
    }

    public String getName()
    {
        return name;
    }

    public void setName(String name)
    {
        this.name = name;
    }

    public List<Stop> getStopsGo()
    {
        return stopsGo;
    }

    public void setStopsGo(List<Stop> stopsGo)
    {
        this.stopsGo = stopsGo;
    }

    public List<Stop> getStopsTurn()
    {
        return stopsTurn;
    }

    public void setStopsTurn(List<Stop> stopsTurn)
    {
        this.stopsTurn = stopsTurn;
    }

    @Override
    public String toString()
    {
        StringBuilder builder = new StringBuilder();
        builder
            .append("Bus [id : ").append(id).append(", name : ").append(name).append(", stopsGo : ").append(stopsGo).append(", stopsTurn : ")
            .append(stopsTurn).append("]");
        return builder.toString();
    }

}
