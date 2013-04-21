package org.istanbus.api;

import com.google.gson.Gson;
import com.google.inject.Inject;
import org.apache.thrift.TException;
import org.istanbus.core.model.Route;
import org.istanbus.core.model.SearchResult;
import org.istanbus.core.model.SuggestedRoute;
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

    @Inject
    public IstanbusJavaServiceImpl(PathFinderService pathFinderService, SearchService searchService) {
        this.pathFinderService = pathFinderService;
        this.searchService = searchService;
    }

    @Override
    public String recommend(String fromStop, String toStop) throws TException {
        List<SuggestedRoute> result = pathFinderService.find(fromStop, toStop);
        return new Gson().toJson(result);
    }

    @Override
    public String search(String index, String keyword) throws TException {
        String decodedKeyword;
        try {
            decodedKeyword = URLDecoder.decode(keyword, "UTF-8");
        } catch (UnsupportedEncodingException e) {
            logger.error("exception while decoding {}", e);
            return "[]";
        }
        List<SearchResult> results = searchService.search(index, decodedKeyword);
        return new Gson().toJson(results);
    }
}
