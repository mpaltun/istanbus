package org.istanbus.core.model;

import org.istanbus.core.model.node.Stop;

public class Transport {
    private Stop from;
    private Stop to;
    private String bus;
    private int stopCount;

    public Stop getFrom() {
        return from;
    }

    public void setFrom(Stop from) {
        this.from = from;
    }

    public Stop getTo() {
        return to;
    }

    public void setTo(Stop to) {
        this.to = to;
    }

    public String getBus() {
        return bus;
    }

    public void setBus(String bus) {
        this.bus = bus;
    }

    public int getStopCount() {
        return stopCount;
    }

    public void setStopCount(int stopCount) {
        this.stopCount = stopCount;
    }

    @Override
    public String toString() {
        return "Transport{" +
                "from=" + from +
                ", to=" + to +
                ", bus='" + bus + '\'' +
                ", stopCount=" + stopCount +
                '}';
    }
}
