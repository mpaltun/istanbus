package org.istanbus.core.dao.mongo;

import com.google.gson.FieldNamingPolicy;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.inject.Inject;
import com.google.inject.name.Named;
import com.mongodb.BasicDBObject;
import com.mongodb.DBCollection;
import com.mongodb.DBCursor;
import org.istanbus.core.dao.StopDAO;
import org.istanbus.core.model.node.Stop;
import org.istanbus.core.util.MongoFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: mustafa
 */
public class StopMongoDAO implements StopDAO
{
    private static final Logger logger = LoggerFactory.getLogger(StopMongoDAO.class);

    private String collectionName;
    private MongoFactory mongoFactory;

    @Inject
    public StopMongoDAO(@Named("mongo.db.stop") String busCollection, MongoFactory mongoFactory) {
        this.collectionName = busCollection;
        this.mongoFactory = mongoFactory;
    }

    @Override
    public List<Stop> loadAll() {
        List<Stop> result = new ArrayList<Stop>();

        DBCollection collection = mongoFactory.loadCollection(collectionName);

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

    @Override
    public Stop loadById(String id)
    {
        return loadById(id, false);
    }

    @Override
    public Stop loadById(String id, boolean loadBus)
    {
        DBCollection collection = mongoFactory.loadCollection(collectionName);
        BasicDBObject query = new BasicDBObject("id", id);

        BasicDBObject fields = new BasicDBObject();
        fields.put("_id", 0);
        fields.put("id", 1);
        fields.put("name", 1);
        if (loadBus)
        {
            fields.put("bus", 1);
        }

        DBCursor cursor = collection.find(query, fields);

        if (cursor.hasNext())
        {
            Gson gson = new GsonBuilder().setFieldNamingPolicy(FieldNamingPolicy.LOWER_CASE_WITH_UNDERSCORES).create();
            return gson.fromJson(cursor.next().toString(), Stop.class);
        }
        return null;
    }
}
