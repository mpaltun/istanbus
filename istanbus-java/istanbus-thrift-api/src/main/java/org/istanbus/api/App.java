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
                bind(IstanbusJavaService.Iface.class).to(IstanbusJavaServiceImpl.class);
                bind(IstanbusThriftServer.class);
            }
        });
        logger.info("Guice initialized.");

        IstanbusThriftServer server = injector.getInstance(IstanbusThriftServer.class);
        server.start();
    }

    public static void main(String[] args) throws Exception {
        App app = new App();
        app.init();
    }

}
