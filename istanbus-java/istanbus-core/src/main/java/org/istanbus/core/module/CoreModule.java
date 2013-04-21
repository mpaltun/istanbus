package org.istanbus.core.module;

import com.google.inject.AbstractModule;
import com.google.inject.Scopes;
import com.google.inject.name.Names;
import org.istanbus.core.dao.BusDAO;
import org.istanbus.core.dao.StopDAO;
import org.istanbus.core.dao.mongo.BusMongoDAO;
import org.istanbus.core.dao.mongo.StopMongoDAO;
import org.istanbus.core.db.GraphDB;
import org.istanbus.core.service.*;
import org.istanbus.core.service.impl.*;
import org.istanbus.core.util.MongoFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

public class CoreModule extends AbstractModule {

    private static final Logger logger = LoggerFactory.getLogger(CoreModule.class);

    @Override
    protected void configure() {
        bind(StopDAO.class).to(StopMongoDAO.class);
        bind(StopMongoDAO.class).in(Scopes.SINGLETON);

        bind(BusDAO.class).to(BusMongoDAO.class).in(Scopes.SINGLETON);

        bind(PathFinderService.class).to(PathFinderServiceImpl.class);
        bind(PathFinderServiceImpl.class).in(Scopes.SINGLETON);

        bind(GraphBuildService.class).to(GraphBuildServiceImpl.class);
        bind(GraphBuildServiceImpl.class).in(Scopes.SINGLETON);

        bind(SearchService.class).to(SearchServiceImpl.class);
        bind(SearchServiceImpl.class).in(Scopes.SINGLETON);

        bind(SearchIndexService.class).to(SearchIndexServiceImpl.class);
        bind(SearchIndexServiceImpl.class).in(Scopes.SINGLETON);

        bind(GraphDB.class).in(Scopes.SINGLETON);
        bind(MongoFactory.class).in(Scopes.SINGLETON);

        // properties
        ArrayList<String> properties = new ArrayList<String>();
        properties.add("app.properties");

        loadProperties(properties);

    }

    private void loadProperties(List<String> propertiesFiles)
    {
        for (String propertiesFile : propertiesFiles) {
            Properties searchProperties = null;
            try {
                searchProperties = getPropertiesFromClasspath(propertiesFile);
                Names.bindProperties(binder(), searchProperties);
            } catch (IOException e) {
                logger.error("Couldnt load properties file from classpath", e);
            }
            Names.bindProperties(binder(), searchProperties);
        }
    }

    private Properties getPropertiesFromClasspath(String fileName) throws IOException {
        Properties properties = new Properties();
        properties.load(ClassLoader.getSystemResourceAsStream(fileName));
        return properties;
    }

}
