package org.istanbus.core.service.impl;

import com.google.inject.Inject;
import org.istanbus.core.dao.StopDAO;
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
    private final Index<Node> busIndex;

    private final String id = "id";
    private final String label = "label";

    private StopDAO stopDAO;

    @Inject
    public GraphBuildServiceImpl(GraphDB graphDB, StopDAO stopDAO) {
        this.stopDAO = stopDAO;
        db = graphDB.getInstance();
        busIndex = db.index().forNodes("bus");
    }

    @Override
    public void buildFullGraph() {

        List<Stop> stops = stopDAO.loadAll();

        for (Stop stop : stops) {
            logger.info("bus list for stop: {}", stop.getId());
            linkBuses(stop);
        }
    }

    @Override
    public boolean testGraph(String busId) {
        IndexHits<Node> hits = busIndex.get(id, busId);
        return hits.size() > 0;
    }

    private void linkBuses(Stop stop)
    {
        Transaction tx = db.beginTx();
        Node previous = null;
        for (Bus bus : stop.getBus()) {
            Node current = createNodeFromBus(bus);
            if (previous == null) {
                // here comes only once
                previous = current;
            }
            else {
                logger.info("Linking bus {} to bus {}", previous.getProperty(id), current.getProperty(id));

                checkAndCreateRelationship(previous, current, stop.getId());

                tx.success();
                previous = current;
            }
        }
        tx.finish();
    }

    private Node createNodeFromBus(Bus bus)
    {
        // check existance
        IndexHits<Node> nodes = busIndex.get(id, bus.getId());

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
            node.setProperty(label, bus.getName());
            node.setProperty(id, bus.getId());

            // add to index
            busIndex.add(node, id, bus.getId());
        }

        return node;
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

    /**
     * Checks a relationship exists between nodes and creates if there is none
     * @param previous
     * @param current
     * @param stopId
     */
    private void checkAndCreateRelationship(Node previous, Node current, String stopId) {
        String[] stopList = null;
        Relationship relationship = getOldRelationShip(previous, current);
        if (relationship == null) {
            relationship = previous.createRelationshipTo(current, RelationShip.DIRECTION_GO);
            stopList = new String[] { stopId };
        }
        else {

            stopList = (String[]) relationship.getProperty("stopList");

            boolean notFound = true;
            for (String stop : stopList) {
                if (stop.equals(stopId)) {
                    notFound = false;
                    break;
                }
            }
            if (notFound) {
                String[] tmpList = new String[stopList.length + 1];
                System.arraycopy(stopList, 0, tmpList, 0, stopList.length);
                tmpList[stopList.length] = stopId;
                stopList = tmpList;
            }
        }
        relationship.setProperty("stopList", stopList);
    }

}
