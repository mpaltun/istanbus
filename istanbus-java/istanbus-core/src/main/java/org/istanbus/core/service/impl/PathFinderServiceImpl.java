package org.istanbus.core.service.impl;

import com.google.inject.Inject;
import org.istanbus.core.db.GraphDB;
import org.istanbus.core.model.Transport;
import org.istanbus.core.model.node.Stop;
import org.istanbus.core.service.PathFinderService;
import org.neo4j.graphalgo.GraphAlgoFactory;
import org.neo4j.graphalgo.PathFinder;
import org.neo4j.graphdb.*;
import org.neo4j.graphdb.index.Index;
import org.neo4j.kernel.Traversal;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

public class PathFinderServiceImpl implements PathFinderService {

    private static final Logger logger = LoggerFactory.getLogger(PathFinderServiceImpl.class);

    private final GraphDatabaseService db;
    private final Index<Node> stopIndex;
    private final PathFinder<Path> finder;

    @Inject
    public PathFinderServiceImpl(GraphDB graphDB) {
        db = graphDB.getInstance();
        stopIndex = db.index().forNodes("stops");

        Expander expander = Traversal.expanderForAllTypes(Direction.OUTGOING);
        finder = GraphAlgoFactory.shortestPath(expander, 10);
    }

    @Override
    public List<Transport> find(String fromStop, String toStop) {

        logger.info("finding path from {} to {}", fromStop, toStop);

        Node nodeA = stopIndex.get("code", fromStop).iterator().next();
        Node nodeB = stopIndex.get("code", toStop).iterator().next();

        Path path = finder.findSinglePath(nodeA, nodeB);
        return getTransports(path);

    }

    private Stop getStopFromNode(Node node)
    {
        Stop stop = new Stop();
        stop.setId((String) node.getProperty("code"));
        stop.setName((String) node.getProperty("label"));

        return stop;
    }

    private List<Transport> getTransports(Path path)
    {
        List<Transport> transports = new ArrayList<Transport>();

        if (path == null)
        {
            return transports;
        }
        Transport previousTransport = null;
        Iterable<Relationship> relationships = path.relationships();
        for (Relationship relationship : relationships) {
            Node startNode = relationship.getStartNode();
            Node endNode = relationship.getEndNode();

            String bus = (String) relationship.getProperty("bus");

            // if this is same bus, then just increment stop count of previous one
            // and set last stop
            if (previousTransport != null && previousTransport.getBus().equals(bus))
            {
                previousTransport.setStopCount(previousTransport.getStopCount() + 1);
                // last stop
                Stop to = getStopFromNode(endNode);
                previousTransport.setTo(to);
                continue;
            }

            Transport transport = new Transport();
            transport.setBus(bus);

            int stopCount = (Integer) relationship.getProperty("stopCount");
            transport.setStopCount(stopCount);

            Stop from = getStopFromNode(startNode);
            transport.setFrom(from);

            Stop to = getStopFromNode(endNode);
            transport.setTo(to);

            previousTransport = transport;
            transports.add(transport);
        }

        return transports;
    }
}
