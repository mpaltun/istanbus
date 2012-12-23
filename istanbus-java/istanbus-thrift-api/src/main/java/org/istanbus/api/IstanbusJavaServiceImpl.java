package org.istanbus.api;

import com.google.gson.Gson;
import org.apache.thrift.TException;
import org.istanbus.core.model.StopSearchResult;
import org.istanbus.core.model.Transport;
import org.istanbus.core.service.PathFinderService;
import org.istanbus.core.service.SearchService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.util.List;

public class IstanbusJavaServiceImpl implements IstanbusJavaService.Iface {

    private static final Logger logger = LoggerFactory.getLogger(IstanbusJavaServiceImpl.class);

    private PathFinderService pathFinderService;
    private SearchService searchService;

    public IstanbusJavaServiceImpl(PathFinderService pathFinderService, SearchService searchService) {
        this.pathFinderService = pathFinderService;
        this.searchService = searchService;
    }

    @Override
    public String recommend(String from_stop, String to_stop) throws TException {
        List<Transport> transports = pathFinderService.find(from_stop, to_stop);
        return new Gson().toJson(transports);
    }

    @Override
    public String stop_search(String keyword) throws TException {
        String decodedKeyword;
        try {
            decodedKeyword = URLDecoder.decode(keyword, "UTF-8");
        } catch (UnsupportedEncodingException e) {
            logger.error("exception while decoding {}", e);
            return "[]";
        }
        List<StopSearchResult> results = searchService.searchStop(decodedKeyword);
        return new Gson().toJson(results);
    }
}
