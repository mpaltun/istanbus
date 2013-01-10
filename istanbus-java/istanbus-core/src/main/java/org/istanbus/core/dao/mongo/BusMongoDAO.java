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
import org.istanbus.core.dao.BusDAO;
import org.istanbus.core.model.node.Bus;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.List;

public class BusMongoDAO implements BusDAO {

    private static final Logger logger = LoggerFactory.getLogger(BusMongoDAO.class);

    private String dbName;

    @Inject
    public BusMongoDAO(@Named("mongo.db") String dbName) {
        this.dbName = dbName;
    }

    @Override
    public List<Bus> loadAllBuses() {
        List<Bus> result = new ArrayList<Bus>();
        MongoClient mongoClient = null;
        try {
            mongoClient = new MongoClient();
        } catch (UnknownHostException e) {
            logger.error("", e);
            return result;
        }

        DB db = mongoClient.getDB(dbName);
        DBCollection collection = db.getCollection("bus");

        BasicDBObject query = new BasicDBObject();

        BasicDBObject fields = new BasicDBObject();
        fields.put("_id", 0);
        fields.put("id", 1);
        fields.put("name", 1);
        fields.put("stops_go", 1);
        fields.put("stops_turn", 1);

        DBCursor cursor = collection.find(query, fields);
        Gson gson = new GsonBuilder().setFieldNamingPolicy(FieldNamingPolicy.LOWER_CASE_WITH_UNDERSCORES).create();

        while (cursor.hasNext()) {
            Bus bus = gson.fromJson(cursor.next().toString(), Bus.class);
            result.add(bus);
        }

        return result;

    }
}
