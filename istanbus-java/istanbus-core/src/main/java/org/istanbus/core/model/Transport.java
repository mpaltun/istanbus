package org.istanbus.core.model;

import org.istanbus.core.model.node.Stop;

import java.util.List;

public class Transport {
    private Stop from;
    private Stop to;
    private List<String> busList;
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

    public int getStopCount() {
        return stopCount;
    }

    public void setStopCount(int stopCount) {
        this.stopCount = stopCount;
    }

    public List<String> getBusList() {
        return busList;
    }

    public void setBusList(List<String> busList) {
        this.busList = busList;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();
        sb.append("Transport");
        sb.append("{from=").append(from);
        sb.append(", to=").append(to);
        sb.append(", busList=").append(busList);
        sb.append(", stopCount=").append(stopCount);
        sb.append('}');
        return sb.toString();
    }
}
