package org.istanbus.core.db;

import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.factory.GraphDatabaseFactory;
import org.neo4j.kernel.EmbeddedGraphDatabase;

public class GraphDB
{
    private GraphDatabaseService instance;

    public GraphDB()
    {
        instance = (EmbeddedGraphDatabase) new GraphDatabaseFactory().newEmbeddedDatabase( "data/graph.db" );
        registerShutdownHook(instance);
    }
    
    private static void registerShutdownHook( final GraphDatabaseService graphDb )
    {
        // Registers a shutdown hook for the Neo4j instance so that it
        // shuts down nicely when the VM exits (even if you "Ctrl-C" the
        // running example before it's completed)
        Runtime.getRuntime().addShutdownHook( new Thread()
        {
            @Override
            public void run()
            {
                graphDb.shutdown();
            }
        } );
    }
    
    public GraphDatabaseService getInstance()
    {
        return instance;
    }
    
    public void shutdown()
    {
        instance.shutdown();
    }
    
}
