package org.istanbus.core.service;

import org.istanbus.core.model.SearchResult;

import java.util.List;

public interface SearchService {

    List<SearchResult> search(String index, String keyword);
}
