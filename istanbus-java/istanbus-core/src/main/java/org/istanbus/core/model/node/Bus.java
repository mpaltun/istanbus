package org.istanbus.core.model.node;

import java.util.List;

public class Bus
{
    private String code;
    private String name;
    private List<Stop> stopsGo;
    private List<Stop> stopsCome;

    public String getCode()
    {
        return code;
    }

    public void setCode(String code)
    {
        this.code = code;
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

    public List<Stop> getStopsCome()
    {
        return stopsCome;
    }

    public void setStopsCome(List<Stop> stopsCome)
    {
        this.stopsCome = stopsCome;
    }

    @Override
    public String toString()
    {
        StringBuilder builder = new StringBuilder();
        builder
            .append("Bus [code : ").append(code).append(", name : ").append(name).append(", stopsGo : ").append(stopsGo).append(", stopsCome : ")
            .append(stopsCome).append("]");
        return builder.toString();
    }

}
