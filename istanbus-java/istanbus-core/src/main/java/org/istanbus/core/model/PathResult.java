package org.istanbus.core.model;

import java.util.List;

public class PathResult {
    List<SuggestedRoute> solutions;

    public List<SuggestedRoute> getSolutions() {
        return solutions;
    }

    public void setSolutions(List<SuggestedRoute> solutions) {
        this.solutions = solutions;
    }
}
