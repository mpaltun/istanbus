package org.istanbus.core.model;

import org.istanbus.core.model.node.Bus;
import org.istanbus.core.model.node.Stop;

import java.util.List;

public class Route
{
    private String[] fromStop;
    private String[] toStop;
    private Bus bus;

    public String[] getToStop()
    {
        return toStop;
    }

    public void setToStop(String[] toStop)
    {
        this.toStop = toStop;
    }

    public String[] getFromStop()
    {
        return fromStop;
    }

    public void setFromStop(String[] fromStop)
    {
        this.fromStop = fromStop;
    }

    public Bus getBus()
    {
        return bus;
    }

    public void setBus(Bus bus)
    {
        this.bus = bus;
    }

    @Override
    public String toString()
    {
        final StringBuilder sb = new StringBuilder("Route{");
        sb.append("fromStop=").append(fromStop);
        sb.append(", toStop=").append(toStop);
        sb.append(", bus=").append(bus);
        sb.append('}');
        return sb.toString();
    }
}
