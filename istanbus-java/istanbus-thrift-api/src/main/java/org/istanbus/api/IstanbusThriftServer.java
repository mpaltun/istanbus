package org.istanbus.api;

import com.google.inject.Inject;
import org.apache.thrift.server.THsHaServer;
import org.apache.thrift.server.TServer;
import org.apache.thrift.transport.TNonblockingServerSocket;
import org.apache.thrift.transport.TNonblockingServerTransport;
import org.apache.thrift.transport.TTransportException;

public class IstanbusThriftServer {

    private IstanbusJavaService.Iface istanbusJavaService;

    @Inject
    public IstanbusThriftServer(IstanbusJavaService.Iface istanbusJavaService) {
        this.istanbusJavaService = istanbusJavaService;
    }

    public void start() {
        IstanbusJavaService.Processor processor = new IstanbusJavaService.Processor(istanbusJavaService);

        try {
            TNonblockingServerTransport serverTransport = new TNonblockingServerSocket(9090);

            THsHaServer.Args args = new THsHaServer.Args(serverTransport);
            args.workerThreads(5);
            args.processor(processor);

            TServer server = new THsHaServer(args);

            System.out.println("Starting server on port 9090 ...");
            server.serve();
        } catch (TTransportException e) {
            e.printStackTrace();
        }
    }

}