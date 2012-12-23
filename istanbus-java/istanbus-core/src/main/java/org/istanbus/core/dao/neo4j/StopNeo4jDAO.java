package org.istanbus.core.dao.neo4j;

import org.istanbus.core.dao.StopDAO;
import org.istanbus.core.db.GraphDB;
import org.istanbus.core.model.node.Stop;

import org.neo4j.graphdb.GraphDatabaseService;

import com.google.inject.Inject;

public class StopNeo4jDAO implements StopDAO
{
    
    private GraphDatabaseService instance;

    @Inject
    public StopNeo4jDAO(GraphDB graphDB)
    {
        instance = graphDB.getInstance();
    }

    @Override
    public Stop loadByCode(String code)
    {
        return null;
    }
    
}
