package org.istanbus.core.model;

import java.util.List;

public class SuggestedRoute
{
    private List<Route> routes;

    public SuggestedRoute(List<Route> routes) {
        this.routes = routes;
    }

    public List<Route> getRoutes()
    {
        return routes;
    }

    public void setRoutes(List<Route> routes)
    {
        this.routes = routes;
    }
}
