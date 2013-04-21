package org.istanbus.core.service.impl;

import com.google.inject.Inject;
import org.istanbus.core.dao.StopDAO;
import org.istanbus.core.db.GraphDB;
import org.istanbus.core.model.PathResult;
import org.istanbus.core.model.Route;
import org.istanbus.core.model.SuggestedRoute;
import org.istanbus.core.model.node.Bus;
import org.istanbus.core.model.node.Stop;
import org.istanbus.core.service.PathFinderService;
import org.neo4j.graphalgo.GraphAlgoFactory;
import org.neo4j.graphalgo.PathFinder;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.Expander;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Path;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.Index;
import org.neo4j.kernel.Traversal;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class PathFinderServiceImpl implements PathFinderService {

    private static final Logger logger = LoggerFactory.getLogger(PathFinderServiceImpl.class);

    private static final int MAX_SUGGESTED_ROUTES = 2;

    private final GraphDatabaseService db;
    private final Index<Node> busIndex;
    private final PathFinder<? extends Path> finder;

    private StopDAO stopDAO;

    @Inject
    public PathFinderServiceImpl(GraphDB graphDB, StopDAO stopDAO) {

        this.stopDAO = stopDAO;
        db = graphDB.getInstance();
        busIndex = db.index().forNodes("bus");

        Expander expander = Traversal.expanderForAllTypes(Direction.BOTH);
        finder = GraphAlgoFactory.shortestPath(expander, MAX_SUGGESTED_ROUTES);
    }

    @Override
    public PathResult find(String fromStopId, String toStopId) {

        logger.info("finding path from {} to {}", fromStopId, toStopId);

        Stop fromStop = stopDAO.loadById(fromStopId);
        Stop toStop = stopDAO.loadById(toStopId);

        List<Bus> fromBuses = fromStop.getBus();
        List<Bus> toBuses = toStop.getBus();

        Set<Bus> commons = getCommonBuses(fromBuses, toBuses);

        List<SuggestedRoute> allSuggestedRoutes = new ArrayList<SuggestedRoute>();
        for (Bus fromBus: fromBuses)
        {
            for (Bus toBus: toBuses)
            {
                if (commons.contains(toBus) || commons.contains(fromBus))
                {
                    continue;
                }

                List<Route> routes = findForBus(fromBus.getId(), toBus.getId());
                if (routes != null && !routes.isEmpty())
                {
                    Route firstRoute = routes.get(0);
                    firstRoute.setFromStop(new String[] { fromStopId });

                    // set last route destination
                    Route lastRoute = routes.get(routes.size() - 1);
                    lastRoute.setFromStop(routes.get(routes.size() - 2).getToStop());
                    lastRoute.setToStop(new String[] { toStopId });

                    SuggestedRoute suggestedRoute = new SuggestedRoute(routes);
                    allSuggestedRoutes.add(suggestedRoute);
                }
            }
        }

        Collections.sort(allSuggestedRoutes, new Comparator<SuggestedRoute>()
        {
            @Override
            public int compare(SuggestedRoute route1, SuggestedRoute route2)
            {
                return route1.getRoutes().size() - route2.getRoutes().size();
            }
        });

        int limit = 5;
        if (allSuggestedRoutes.size() < limit)
        {
            limit = allSuggestedRoutes.size();
        }

        List<SuggestedRoute> suggestedRoutes = allSuggestedRoutes.subList(0, limit);

        PathResult pathResult = new PathResult();
        pathResult.setPerfectRoutes(commons);
        pathResult.setSuggestions(suggestedRoutes);
        return pathResult;
    }

    private Set<Bus> getCommonBuses(List<Bus> fromBusList, List<Bus> toBusList)
    {
        HashSet<Bus> commons = new HashSet<Bus>();
        HashSet<Bus> fromBusListSet = new HashSet<Bus>(fromBusList);
        for (Bus toBus : toBusList)
        {
            if (fromBusListSet.contains(toBus))
            {
                commons.add(toBus);
            }
        }
        return commons;
    }

    private SuggestedRoute getSingleSuggestedRoute(Stop fromStop, Stop toStop, Bus toBus)
    {
        Route route = new Route();
        route.setBus(toBus);

        route.setToStop(new String[] { toStop.getId() });
        route.setFromStop(new String[] { fromStop.getId() });

        return new SuggestedRoute(Arrays.asList(route));
    }

    private List<String> getAsStringList(List<Bus> busList) {
        List<String> stringList = new ArrayList<String>();
        for (Bus bus : busList)
        {
            stringList.add(bus.getId());
        }
        return stringList;
    }

    private List<Route> findForBus(String fromBusId, String toBusId)
    {
        Node nodeA = busIndex.get("id", fromBusId).iterator().next();
        Node nodeB = busIndex.get("id", toBusId).iterator().next();

        Path path = finder.findSinglePath(nodeA, nodeB);
        return getSolutionFromPath(path, fromBusId, toBusId);
    }

    private Bus getBusFromNode(Node node)
    {
        Bus bus = new Bus();
        bus.setId((String) node.getProperty("id"));
        bus.setName((String) node.getProperty("label"));

        return bus;
    }

    private List<Route> getSolutionFromPath(Path path, String fromBusId, String toBusId)
    {
        if (path == null) {
            logger.info("no bus path found. from {}, to {}", fromBusId, toBusId);
            return null;
        }

        Route previousRoute = null;

        List<Route> result = new ArrayList<Route>();
        for (Relationship relationship : path.relationships())
        {
            Bus first = getBusFromNode(relationship.getStartNode());
            Bus second = getBusFromNode(relationship.getEndNode());

            String[] stops = (String[]) relationship.getProperty("stopList");

            if (previousRoute == null)
            {
                if (second.getId().equals(fromBusId))
                {
                    Bus temp = first;

                    first = second;
                    second = temp;
                }
            }
            else
            {
                if (previousRoute.getBus().getId().equals(first.getId()))
                {
                    result.remove(result.size() - 1);
                }
            }

            Route firstRoute = getRouteFromBus(first, stops);
            if (previousRoute != null)
            {
                firstRoute.setFromStop(previousRoute.getToStop());
            }
            result.add(firstRoute);

            Route secondRoute = getRouteFromBus(second, stops);
            result.add(secondRoute);

            previousRoute = secondRoute;

        }

        return result;

    }

    private Route getRouteFromBus(Bus bus, String[] stops)
    {
        Route route = new Route();
        route.setBus(bus);
        route.setToStop(stops);

        return route;
    }

}
