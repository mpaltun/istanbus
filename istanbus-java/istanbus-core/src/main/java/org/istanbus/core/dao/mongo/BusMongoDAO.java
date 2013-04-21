package org.istanbus.core.dao.mongo;

import com.google.gson.FieldNamingPolicy;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.inject.Inject;
import com.google.inject.name.Named;
import com.mongodb.BasicDBObject;
import com.mongodb.DBCollection;
import com.mongodb.DBCursor;
import org.istanbus.core.dao.BusDAO;
import org.istanbus.core.model.node.Bus;
import org.istanbus.core.util.MongoFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

public class BusMongoDAO implements BusDAO {

    private static final Logger logger = LoggerFactory.getLogger(BusMongoDAO.class);

    private String collectionName;
    private MongoFactory mongoFactory;

    @Inject
    public BusMongoDAO(@Named("mongo.db.bus") String stopCollection, MongoFactory mongoFactory) {
        this.collectionName = stopCollection;
        this.mongoFactory = mongoFactory;
    }

    @Override
    public List<Bus> loadAll() {
        List<Bus> result = new ArrayList<Bus>();

        DBCollection collection = mongoFactory.loadCollection(collectionName);

        BasicDBObject query = new BasicDBObject();

        BasicDBObject fields = new BasicDBObject();
        fields.put("_id", 0);
        fields.put("id", 1);
        fields.put("name", 1);
        fields.put("stops.go", 1);
        fields.put("stops.turn", 1);

        DBCursor cursor = collection.find(query, fields);
        Gson gson = new GsonBuilder().setFieldNamingPolicy(FieldNamingPolicy.LOWER_CASE_WITH_UNDERSCORES).create();

        while (cursor.hasNext()) {
            Bus bus = gson.fromJson(cursor.next().toString(), Bus.class);
            result.add(bus);
        }

        cursor.close();
        return result;

    }
}
