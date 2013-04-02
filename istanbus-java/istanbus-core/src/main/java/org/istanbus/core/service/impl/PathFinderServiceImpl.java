package org.istanbus.core.service.impl;

import com.google.inject.Inject;
import org.istanbus.core.db.GraphDB;
import org.istanbus.core.model.PathResult;
import org.istanbus.core.model.Transport;
import org.istanbus.core.model.TransportSolution;
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
import java.util.HashSet;
import java.util.List;

public class PathFinderServiceImpl implements PathFinderService {

    private static final Logger logger = LoggerFactory.getLogger(PathFinderServiceImpl.class);

    private final GraphDatabaseService db;
    private final Index<Node> busIndex;
    private final PathFinder<? extends Path> finder;

    @Inject
    public PathFinderServiceImpl(GraphDB graphDB) {
        db = graphDB.getInstance();
        busIndex = db.index().forNodes("bus");

        Expander expander = Traversal.expanderForAllTypes(Direction.OUTGOING);
        this.finder = GraphAlgoFactory.shortestPath(expander, 5);
    }

    @Override
    public List<Bus> find(String fromBus, String toBus) {

        logger.info("finding path from {} to {}", fromBus, toBus);

        Node nodeA = busIndex.get("id", fromBus).iterator().next();
        Node nodeB = busIndex.get("id", toBus).iterator().next();

        Path path = finder.findSinglePath(nodeA, nodeB);
        return getSolutionFromPath(path);

    }

    private Bus getBusFromNode(Node node)
    {
        Bus bus = new Bus();
        bus.setId((String) node.getProperty("id"));
        bus.setName((String) node.getProperty("label"));

        return bus;
    }

    private List<Bus> getSolutionFromPath(Path path)
    {

        List<Bus> result = new ArrayList<Bus>();
        for (Node node : path.nodes())
        {
            Bus bus = getBusFromNode(node);
            result.add(bus);
        }
        return result;

    }

    private List<String> getCommonBusList(Transport transport, List<String> busList) {
        HashSet<String> set = new HashSet<String>(transport.getBusList());

        List<String> commonBusList = new ArrayList<String>();
        for (String bus : busList) {
            if (set.contains(bus)) {
                commonBusList.add(bus);
            }
        }
        return commonBusList;
    }
}
