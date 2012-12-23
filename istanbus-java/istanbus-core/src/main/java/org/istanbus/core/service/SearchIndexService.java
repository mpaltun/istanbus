package org.istanbus.core.service;

import java.io.IOException;

public interface SearchIndexService {

    void indexStopFromBusJson(String jsonPath) throws IOException;

}
