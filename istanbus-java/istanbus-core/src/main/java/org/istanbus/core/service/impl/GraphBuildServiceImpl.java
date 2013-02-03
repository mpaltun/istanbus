package org.istanbus.core.service.impl;

import com.google.inject.Inject;
import org.istanbus.core.dao.BusDAO;
import org.istanbus.core.db.GraphDB;
import org.istanbus.core.model.graph.RelationShip;
import org.istanbus.core.model.node.Bus;
import org.istanbus.core.model.node.Stop;
import org.istanbus.core.service.GraphBuildService;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.Transaction;
import org.neo4j.graphdb.index.Index;
import org.neo4j.graphdb.index.IndexHits;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

public class GraphBuildServiceImpl implements GraphBuildService {

    private static final Logger logger = LoggerFactory.getLogger(GraphBuildService.class);

    private GraphDatabaseService db;
    private final Index<Node> stopIndex;

    private final String id = "id";
    private final String label = "label";

    private BusDAO busDAO;

    @Inject
    public GraphBuildServiceImpl(GraphDB graphDB, BusDAO busDAO) {
        this.busDAO = busDAO;
        db = graphDB.getInstance();
        stopIndex = db.index().forNodes("stops");
    }

    @Override
    public void buildFullGraph() {
        List<Bus> busList = busDAO.loadAllBuses();
        for (Bus bus : busList) {
            logger.info("Stops(go) for bus: {}", bus.getId());
            linkStops(bus.getId(), bus.getStops().getGo(), RelationShip.DIRECTION_GO);
            logger.info("Stops(turn) for bus: {}", bus.getId());
            linkStops(bus.getId(), bus.getStops().getTurn(), RelationShip.DIRECTION_GO);
            // linkStopsApacheStyle(bus.getId(), bus.getStopsGo(), RelationShip.DIRECTION_GO);
        }
    }

    @Override
    public boolean testGraph(String stopId) {
        IndexHits<Node> hits = stopIndex.get(id, stopId);
        return hits.size() > 0;
    }

    private void linkStops(String busId, List<Stop> stops, RelationShip relationShip) {
        Transaction tx = db.beginTx();
        Node previous = null;
        for (Stop stop : stops) {
            Node current = createNodeFromStop(stop);
            if (previous == null) {
                // here comes only once
                previous = current;
            }
            else {
                logger.info("Linking stop {} to stop {}", previous.getProperty(label), current.getProperty(label));

                checkAndCreateRelationship(previous, current, busId);

                tx.success();
                previous = current;
            }
        }
        tx.finish();
    }

    private Relationship getOldRelationShip(Node previous, Node current) {

        String previousId = (String) previous.getProperty(id);
        String currentId = (String) current.getProperty(id);

        Relationship oldRelationship = null;
        for (Relationship r : previous.getRelationships()) {
            String startId = (String) r.getStartNode().getProperty(id);
            String endId = (String) r.getEndNode().getProperty(id);
            if (startId.equals(previousId) && endId.equals(currentId)) {
                oldRelationship = r;
                break;
            }
        }
        return oldRelationship;
    }

    private void checkAndCreateRelationship(Node previous, Node current, String busId) {
        checkAndCreateRelationship(previous, current, busId, 1);
    }

    /**
     * Checks a relationship exists between nodes and creates if there is none
     * @param previous
     * @param current
     * @param busId
     */
    private void checkAndCreateRelationship(Node previous, Node current, String busId, int stopCount) {
        String[] busList = null;
        Relationship relationship = getOldRelationShip(previous, current);
        if (relationship == null) {
            relationship = previous.createRelationshipTo(current, RelationShip.DIRECTION_GO);
            busList = new String[] { busId };
        }
        else {

            busList = (String[]) relationship.getProperty("busList");

            boolean notFound = true;
            for (String bus : busList) {
                if (bus.equals(busId)) {
                    notFound = false;
                    break;
                }
            }
            if (notFound) {
                String[] tmpList = new String[busList.length + 1];
                System.arraycopy(busList, 0, tmpList, 0, busList.length);
                tmpList[busList.length] = busId;
                busList = tmpList;
            }
        }
        relationship.setProperty("busList", busList);
        relationship.setProperty("stopCount", stopCount);
    }

    private void linkStopsApacheStyle(String busId, List<Stop> stops, RelationShip relationShip) {
        Transaction tx = db.beginTx();
        Node[] nodes = { null, null, null };
        for (int i = 0; i < stops.size(); i++) {
            Stop stop =  stops.get(i);
            for (int j = i; j < stops.size(); j++) {
                Stop s = stops.get(j);
                if (nodes[1] == null) {
                    // here comes only once
                    nodes[1] = createNodeFromStop(stop);
                } else {
                    nodes[2] = createNodeFromStop(s);

                    logger.info("Linking stop {} to stop {}", nodes[1].getProperty(label), nodes[2].getProperty(label));

                    int stopCount = j - i;
                    checkAndCreateRelationship(nodes[1], nodes[2], busId, stopCount);

                    tx.success();
                }
            }
            // shift
            nodes[1] = null;
            nodes[2] = null;
        }
        tx.finish();
    }

    private Node createNodeFromStop(Stop stop) {

        // check existance
        IndexHits<Node> nodes = stopIndex.get(id, stop.getId());

        Node node = null;
        if (nodes.size() > 0)
        {
            // stop found on index
            node = nodes.iterator().next();
        }

        // if still null then create
        if (node == null)
        {
            // stop not found on index, so creating new one
            node = db.createNode();
            node.setProperty(label, stop.getName());
            node.setProperty(id, stop.getId());

            // add to index
            stopIndex.add(node, id, stop.getId());
        }

        return node;
    }

}
