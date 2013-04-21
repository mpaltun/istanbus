package org.istanbus.core.model;

import org.istanbus.core.model.node.Bus;
import org.istanbus.core.model.node.Stop;

import java.util.List;

public class Route
{
    private Stop fromStop;
    private Stop toStop;
    private Bus bus;

    public Stop getFromStop()
    {
        return fromStop;
    }

    public void setFromStop(Stop fromStop)
    {
        this.fromStop = fromStop;
    }

    public Stop getToStop()
    {
        return toStop;
    }

    public void setToStop(Stop toStop)
    {
        this.toStop = toStop;
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
