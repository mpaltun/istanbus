package org.istanbus.core.service;

import java.io.IOException;

public interface SearchIndexService {

    void indexFromBusJson(String jsonPath) throws IOException;

}
