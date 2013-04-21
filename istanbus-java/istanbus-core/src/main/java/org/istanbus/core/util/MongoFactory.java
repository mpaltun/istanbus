package org.istanbus.core.util;

import com.google.inject.Inject;
import com.google.inject.name.Named;
import com.mongodb.DB;
import com.mongodb.DBCollection;
import com.mongodb.Mongo;
import com.mongodb.MongoException;
import com.mongodb.MongoOptions;
import com.mongodb.ServerAddress;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class MongoFactory
{
    private Mongo mongo;
    private DB database;
    private String databaseName;

    private static final Logger logger = LoggerFactory.getLogger(MongoFactory.class);

    @Inject
    public MongoFactory(@Named("mongo.db") String databaseName)
    {
        try
        {
            List<ServerAddress> hosts = new ArrayList<ServerAddress>();
            String url = "127.0.0.1:27017";
            hosts.add(new ServerAddress(url));

            MongoOptions options = new MongoOptions();
            options.autoConnectRetry = true;
            options.connectionsPerHost = 10;
            options.connectTimeout = 0;
            options.maxWaitTime = 120000;
            options.socketTimeout = 0;
            options.threadsAllowedToBlockForConnectionMultiplier = 5;

            this.mongo = new Mongo(hosts, options);
            this.database = mongo.getDB(databaseName);
            this.databaseName = databaseName;
            logger.info("Mongo({},{}) has been initialized with {}", new Object[] { url, databaseName, options });
        }
        catch(UnknownHostException e)
        {
            logger.error("Unknown Host Exception caught", e);
        }
        catch(MongoException e)
        {
            logger.error("Mongo Exception caught", e);
        }
    }

    public DBCollection loadCollection(String name)
    {
        return database.getCollection(name);
    }

    public DBCollection loadCollection(String db, String name)
    {
        return mongo.getDB(db).getCollection(name);
    }

    public Collection<String> getCollectionNames()
    {
        return database.getCollectionNames();
    }

    public String getDatabaseName()
    {
        return databaseName;
    }

    public void shutdown()
    {
        logger.info("Shutting down mongo({}-{}) instance", mongo.getConnectPoint(), database.getName());
        mongo.close();
    }

}
