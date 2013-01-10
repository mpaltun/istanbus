package org.istanbus.core.service.impl;

import com.google.inject.Inject;
import org.istanbus.core.db.GraphDB;
import org.istanbus.core.model.Transport;
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
    private final Index<Node> stopIndex;
    private final PathFinder<? extends Path> finder;

    @Inject
    public PathFinderServiceImpl(GraphDB graphDB) {
        db = graphDB.getInstance();
        stopIndex = db.index().forNodes("stops");

        Expander expander = Traversal.expanderForAllTypes(Direction.OUTGOING);
        this.finder = GraphAlgoFactory.shortestPath(expander, 10);
    }

    @Override
    public List<Transport> find(String fromStop, String toStop) {

        logger.info("finding path from {} to {}", fromStop, toStop);

        Node nodeA = stopIndex.get("id", fromStop).iterator().next();
        Node nodeB = stopIndex.get("id", toStop).iterator().next();

        Path path = finder.findSinglePath(nodeA, nodeB);
        return getTransports(path);

    }

    private Stop getStopFromNode(Node node)
    {
        Stop stop = new Stop();
        stop.setId((String) node.getProperty("id"));
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
        for (Relationship relationship : path.relationships()) {
            Node startNode = relationship.getStartNode();
            Node endNode = relationship.getEndNode();
            List<String> busList = Arrays.asList((String[]) relationship.getProperty("busList"));

            // set common bus list for following transports
            if (previousTransport != null) {
                List<String> commonBusList = getCommonBusList(previousTransport, busList);
                if (!commonBusList.isEmpty()) {
                    previousTransport.setStopCount(previousTransport.getStopCount() + 1);
                    previousTransport.setBusList(commonBusList);
                    // last stop
                    Stop to = getStopFromNode(endNode);
                    previousTransport.setTo(to);
                    continue;
                }
            }

            Transport transport = new Transport();
            transport.setBusList(busList);


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
