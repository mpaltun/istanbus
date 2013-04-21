package org.istanbus.core.service.impl;

import com.google.inject.Inject;
import org.istanbus.core.dao.StopDAO;
import org.istanbus.core.db.GraphDB;
import org.istanbus.core.model.BusRequest;
import org.istanbus.core.model.PathResult;
import org.istanbus.core.model.Route;
import org.istanbus.core.model.SuggestedRoute;
import org.istanbus.core.model.node.Bus;
import org.istanbus.core.model.node.Stop;
import org.istanbus.core.service.PathFinderService;
import org.istanbus.core.util.StopRequestCache;
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

        StopRequestCache cache = new StopRequestCache(stopDAO);

        Stop fromStop = cache.loadById(fromStopId, true);
        Stop toStop = cache.loadById(toStopId, true);

        List<Bus> fromBuses = fromStop.getBus();
        List<Bus> toBuses = toStop.getBus();

        // clean bus lists
        fromStop.setBus(null);
        toStop.setBus(null);

        Set<Bus> commonBusList = getCommonBuses(fromBuses, toBuses);

        List<SuggestedRoute> suggestedRoutes = new ArrayList<SuggestedRoute>();
        // find shortest path for buses that passing through each stop
        for (Bus fromBus: fromBuses)
        {
            for (Bus toBus: toBuses)
            {
                if (commonBusList.contains(toBus) || commonBusList.contains(fromBus))
                {
                    // ignore common routes
                    continue;
                }

                BusRequest request = new BusRequest(fromBus.getId(), toBus.getId(), cache);
                List<Route> routes = findForBus(request);
                handleRouteResults(fromStop, toStop, suggestedRoutes, routes);
            }
        }

        PathResult pathResult = new PathResult();
        pathResult.setSuggestions(suggestedRoutes);

        List<SuggestedRoute> perfectRoutes = getPerfectRoutes(fromStop, toStop, commonBusList);
        pathResult.setPerfectRoutes(perfectRoutes);

        return pathResult.sort().limit();
    }

    private void handleRouteResults(Stop fromStop, Stop toStop, List<SuggestedRoute> suggestedRoutes, List<Route> routes)
    {
        if (routes != null && !routes.isEmpty())
        {
            Route firstRoute = routes.get(0);
            firstRoute.setFromStop(fromStop);

            // set last route destination
            Route lastRoute = routes.get(routes.size() - 1);
            lastRoute.setFromStop(routes.get(routes.size() - 2).getToStop());
            lastRoute.setToStop(toStop);

            SuggestedRoute suggestedRoute = new SuggestedRoute(routes);
            suggestedRoutes.add(suggestedRoute);
        }
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

    private List<SuggestedRoute> getPerfectRoutes(Stop fromStop, Stop toStop, Set<Bus> commonBusList)
    {
        List<SuggestedRoute> perfectRoutes = new ArrayList<SuggestedRoute>();
        for (Bus bus : commonBusList)
        {
            Route route = new Route();
            route.setBus(bus);

            SuggestedRoute suggestedRoute = new SuggestedRoute(route);
            perfectRoutes.add(suggestedRoute);
        }
        return perfectRoutes;
    }

    private List<Route> findForBus(BusRequest request)
    {
        Node nodeA = busIndex.get("id", request.getFromBusId()).iterator().next();
        Node nodeB = busIndex.get("id", request.getToBusId()).iterator().next();

        Path path = finder.findSinglePath(nodeA, nodeB);
        return getSolutionFromPath(path, request);
    }

    private Bus getBusFromNode(Node node)
    {
        Bus bus = new Bus();
        bus.setId((String) node.getProperty("id"));
        bus.setName((String) node.getProperty("label"));

        return bus;
    }

    private List<Route> getSolutionFromPath(Path path, BusRequest request)
    {
        String fromBusId = request.getFromBusId();
        String toBusId = request.getToBusId();

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

            Route firstRoute = getRouteFromBus(first, stops, request.getStopDAO());
            if (previousRoute != null)
            {
                firstRoute.setFromStop(previousRoute.getToStop());
            }
            result.add(firstRoute);

            Route secondRoute = getRouteFromBus(second, stops, request.getStopDAO());
            result.add(secondRoute);

            previousRoute = secondRoute;

        }

        return result;

    }

    private Route getRouteFromBus(Bus bus, String[] stopIds, StopDAO stopDAO)
    {
        Route route = new Route();
        route.setBus(bus);

        Stop stop = stopDAO.loadById(stopIds[0]);
        route.setToStop(stop);

        return route;
    }

}
