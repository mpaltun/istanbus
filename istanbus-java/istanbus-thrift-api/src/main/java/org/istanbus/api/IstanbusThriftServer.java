package org.istanbus.api;

import com.google.inject.Inject;
import org.apache.thrift.server.THsHaServer;
import org.apache.thrift.server.TServer;
import org.apache.thrift.transport.TNonblockingServerSocket;
import org.apache.thrift.transport.TNonblockingServerTransport;
import org.apache.thrift.transport.TTransportException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.InetSocketAddress;

public class IstanbusThriftServer {

    private IstanbusJavaService.Iface istanbusJavaService;

    private static final Logger logger = LoggerFactory.getLogger(IstanbusThriftServer.class);

    @Inject
    public IstanbusThriftServer(IstanbusJavaService.Iface istanbusJavaService) {
        this.istanbusJavaService = istanbusJavaService;
    }

    public void start() {
        IstanbusJavaService.Processor processor = new IstanbusJavaService.Processor(istanbusJavaService);

        String host = "127.0.0.1";
        int port = 9090;

        try {
            InetSocketAddress address = new InetSocketAddress(host, port);
            TNonblockingServerTransport serverTransport = new TNonblockingServerSocket(address);

            THsHaServer.Args args = new THsHaServer.Args(serverTransport);
            args.maxReadBufferBytes = 5242880L;
            args.workerThreads(5);
            args.processor(processor);

            TServer server = new THsHaServer(args);
            server.serve();

            logger.info("Started server at {}:{} ...", host, port);
        } catch (TTransportException e) {
            e.printStackTrace();
        }
    }

}