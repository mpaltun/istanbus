package org.istanbus.core.model;

import java.util.List;

public class PathResult {
    List<TransportSolution> solutions;

    public List<TransportSolution> getSolutions() {
        return solutions;
    }

    public void setSolutions(List<TransportSolution> solutions) {
        this.solutions = solutions;
    }
}
