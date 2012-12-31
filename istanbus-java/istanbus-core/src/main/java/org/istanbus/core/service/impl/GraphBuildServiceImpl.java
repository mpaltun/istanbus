package org.istanbus.core.service.impl;

import java.util.List;

import org.istanbus.core.db.GraphDB;
import org.istanbus.core.model.graph.RelationShip;
import org.istanbus.core.model.node.Bus;
import org.istanbus.core.model.node.Stop;
import org.istanbus.core.service.GraphBuildService;
import org.istanbus.core.util.BusJsonParser;

import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.Transaction;

import com.google.inject.Inject;
import org.neo4j.graphdb.index.Index;
import org.neo4j.graphdb.index.IndexHits;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class GraphBuildServiceImpl implements GraphBuildService {

    private static final Logger logger = LoggerFactory.getLogger(GraphBuildService.class);

    private GraphDatabaseService db;
    private final Index<Node> stopIndex;

    private final String code = "code";
    private final String label = "label";
    private BusJsonParser jsonParser;

    @Inject
    public GraphBuildServiceImpl(GraphDB graphDB, BusJsonParser jsonParser) {
        this.jsonParser = jsonParser;
        db = graphDB.getInstance();
        stopIndex = db.index().forNodes("stops");
    }

    @Override
    public void buildFullGraph(List<Bus> busList) {
        for (Bus bus : busList) {
            logger.info("Stops(go) for bus: {}", bus.getCode());
            List<Stop> stopsGo = bus.getStopsGo();
            linkStops(bus.getCode(), stopsGo, RelationShip.DIRECTION_GO);
        }
    }

    @Override
    public void buildFullGraph(String jsonPath) {
        List<Bus> busList = jsonParser.parse(jsonPath);
        buildFullGraph(busList);
    }

    @Override
    public boolean testGraph(String stopCode) {
        IndexHits<Node> hits = stopIndex.get(code, stopCode);
        return hits.size() > 0;
    }

    private void linkStops(String busCode, List<Stop> stops, RelationShip relationShip) {
        Transaction tx = db.beginTx();
        Node previousNode = null;
        for (Stop stop : stops) {
            if (previousNode == null) {
                // here comes only once
                previousNode = createNodeFromStop(stop);
            }
            else {
                Node node = createNodeFromStop(stop);
                logger.info("Linking stop {} to stop {}", previousNode.getProperty(label), node.getProperty(label));

                Relationship relationship = previousNode.createRelationshipTo(node, relationShip);
                relationship.setProperty("bus", busCode);
                relationship.setProperty("stopCount", 1);

                tx.success();
                previousNode = node;
            }
        }
        tx.finish();
    }

    private void linkStopsApacheStyle(String busCode, List<Stop> stops) {
        Transaction tx = db.beginTx();
        Node[] nodes = {null, null, null};
        for (int i = 0; i < stops.size(); i++) {
            Stop stop =  stops.get(i);
            for (int j = i; j < stops.size(); j++) {
                Stop s = stops.get(j);
                if (nodes[1] == null) {
                    // here comes only once
                    nodes[1] = createNodeFromStop(stop);
                } else {
                    nodes[2] = createNodeFromStop(s);

                    if (!stop.getId().equals(s.getId()))
                    {
                        logger.info("Linking stop {} to stop {}", nodes[1].getProperty(label), nodes[2].getProperty(label));
                        Relationship relationship = nodes[1].createRelationshipTo(nodes[2], RelationShip.DIRECTION_GO);
                        relationship.setProperty("bus", busCode);
                        relationship.setProperty("stopCount", j - i);
                    }

                    tx.success();
                }
            }
            // shift
            nodes[1] = null;
            nodes[2] = null;
        }
        tx.finish();
    }

    private double calculateCost(Node[] nodes) {
         return 0d;
    }

    private Node createNodeFromStop(Stop stop) {

        // check existance
        IndexHits<Node> nodes = stopIndex.get(code, stop.getId());

        Node node = null;
        if (nodes.size() > 0)
        {
            node = nodes.iterator().next();
            logger.info("Stop {} found on index", stop.getId());
        }

        // if still null then create
        if (node == null)
        {
            logger.info("Stop {} not found on index, so creating new one", stop.getId());
            node = db.createNode();
            node.setProperty(label, stop.getName());
            node.setProperty(code, stop.getId());

            // add to index
            stopIndex.add(node, code, stop.getId());
        }

        return node;
    }

}
