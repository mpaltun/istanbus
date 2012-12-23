package org.istanbus.core.service.impl;

import com.google.inject.Inject;
import org.istanbus.core.db.GraphDB;
import org.istanbus.core.model.Transport;
import org.istanbus.core.model.graph.RelationShip;
import org.istanbus.core.model.node.Stop;
import org.istanbus.core.service.PathFinderService;
import org.neo4j.graphalgo.GraphAlgoFactory;
import org.neo4j.graphalgo.PathFinder;
import org.neo4j.graphalgo.WeightedPath;
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

        Expander expander = Traversal.expanderForTypes(RelationShip.DIRECTION_GO);
        finder = GraphAlgoFactory.shortestPath(expander, 20);
    }

    @Override
    public List<Transport> find(String fromStop, String toStop) {

        Node nodeA = stopIndex.get("code", fromStop).iterator().next();
        Node nodeB = stopIndex.get("code", toStop).iterator().next();

        Path path = finder.findSinglePath(nodeA, nodeB);
        return getTransports(path);

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
            if (previousTransport != null && previousTransport.getBus().equals(bus))
            {
                previousTransport.setStopCount(previousTransport.getStopCount() + 1);
                continue;
            }

            Transport transport = new Transport();
            transport.setBus(bus);

            int stopCount = (Integer) relationship.getProperty("stopCount");
            transport.setStopCount(stopCount);

            Stop from = new Stop();
            from.setCode(startNode.getProperty("code").toString());
            from.setName(startNode.getProperty("label").toString());
            transport.setFrom(from);

            Stop to = new Stop();
            to.setCode(endNode.getProperty("code").toString());
            to.setName(endNode.getProperty("label").toString());
            transport.setTo(to);

            previousTransport = transport;
            transports.add(transport);
        }

        return transports;
    }
}
