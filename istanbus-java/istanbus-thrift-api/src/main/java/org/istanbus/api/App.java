package org.istanbus.api;

import com.google.inject.AbstractModule;
import com.google.inject.Guice;
import com.google.inject.Injector;
import org.istanbus.core.module.CoreModule;
import org.istanbus.core.service.PathFinderService;
import org.istanbus.core.service.SearchService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class App {
    private static final Logger logger = LoggerFactory.getLogger(App.class);

    public void init() {
        Injector injector = Guice.createInjector(new AbstractModule() {

            @Override
            protected void configure() {
                install(new CoreModule());
            }
        });
        logger.info("Guice initialized.");

        PathFinderService pathFinderService = injector.getInstance(PathFinderService.class);
        SearchService searchService = injector.getInstance(SearchService.class);
        IstanbusThriftServer istanbusThriftServer = new IstanbusThriftServer(pathFinderService, searchService);
        istanbusThriftServer.start();
    }

    public static void main(String[] args) throws Exception {
        App app = new App();
        app.init();
    }

}
