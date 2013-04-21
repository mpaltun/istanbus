package org.istanbus.core.dao.mongo;

import com.google.gson.FieldNamingPolicy;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.inject.Inject;
import com.google.inject.name.Named;
import com.mongodb.BasicDBObject;
import com.mongodb.DB;
import com.mongodb.DBCollection;
import com.mongodb.DBCursor;
import com.mongodb.MongoClient;
import org.istanbus.core.dao.StopDAO;
import org.istanbus.core.model.node.Bus;
import org.istanbus.core.model.node.Stop;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: mustafa
 */
public class StopMongoDAO implements StopDAO
{
    private static final Logger logger = LoggerFactory.getLogger(StopMongoDAO.class);

    private String dbName;

    @Inject
    public StopMongoDAO(@Named("mongo.db") String dbName) {
        this.dbName = dbName;
    }

    @Override
    public List<Stop> loadAll() {
        List<Stop> result = new ArrayList<Stop>();

        DBCollection collection = getCollection();

        BasicDBObject query = new BasicDBObject();

        BasicDBObject fields = new BasicDBObject();
        fields.put("_id", 0);
        fields.put("id", 1);
        fields.put("name", 1);
        fields.put("bus", 1);

        DBCursor cursor = collection.find(query, fields);
        Gson gson = new GsonBuilder().setFieldNamingPolicy(FieldNamingPolicy.LOWER_CASE_WITH_UNDERSCORES).create();

        while (cursor.hasNext()) {
            Stop stop = gson.fromJson(cursor.next().toString(), Stop.class);
            result.add(stop);
        }

        cursor.close();
        return result;

    }

    private DBCollection getCollection()
    {
        MongoClient mongoClient = null;
        try
        {
            mongoClient = new MongoClient();
        } catch (UnknownHostException e)
        {
            logger.error("", e);
        }

        DB db = mongoClient.getDB(dbName);
        return db.getCollection("stop");
    }

    @Override
    public Stop loadById(String id)
    {
        DBCollection collection = getCollection();
        DBCursor cursor = collection.find(new BasicDBObject("id", id));

        if (cursor.hasNext())
        {
            Gson gson = new GsonBuilder().setFieldNamingPolicy(FieldNamingPolicy.LOWER_CASE_WITH_UNDERSCORES).create();
            return gson.fromJson(cursor.next().toString(), Stop.class);
        }
        return null;
    }
}
