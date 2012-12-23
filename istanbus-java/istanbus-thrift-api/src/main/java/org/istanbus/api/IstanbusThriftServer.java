package org.istanbus.api;

import com.google.inject.Inject;
import org.apache.thrift.server.TNonblockingServer;
import org.apache.thrift.server.TServer;
import org.apache.thrift.transport.TNonblockingServerSocket;
import org.apache.thrift.transport.TNonblockingServerTransport;
import org.apache.thrift.transport.TTransportException;
import org.istanbus.core.service.PathFinderService;
import org.istanbus.core.service.SearchService;
import org.istanbus.core.service.impl.SearchServiceImpl;

public class IstanbusThriftServer {

    private PathFinderService pathFinderService;
    private SearchService searchService;

    public IstanbusThriftServer(PathFinderService pathFinderService, SearchService searchService) {
        this.pathFinderService = pathFinderService;
        this.searchService = searchService;
    }

    public void start() {
        try {
            TNonblockingServerTransport serverTransport = new TNonblockingServerSocket(9090);
            IstanbusJavaService.Processor processor = new IstanbusJavaService.Processor(new IstanbusJavaServiceImpl(pathFinderService, searchService));

            TServer server = new TNonblockingServer(new TNonblockingServer.Args(serverTransport).
                    processor(processor));
            System.out.println("Starting server on port 9090 ...");
            server.serve();
        } catch (TTransportException e) {
            e.printStackTrace();
        }
    }

}