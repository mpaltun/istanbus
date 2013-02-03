package org.istanbus.core.model;

import java.util.List;

public class TransportSolution {
    private List<Transport> transports;

    public TransportSolution(List<Transport> transports) {
        this.transports = transports;
    }

    public List<Transport> getTransports() {
        return transports;
    }

    public void setTransports(List<Transport> transports) {
        this.transports = transports;
    }
}
