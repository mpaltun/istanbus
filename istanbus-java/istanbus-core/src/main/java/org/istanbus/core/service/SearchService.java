package org.istanbus.core.service;

import org.istanbus.core.model.StopSearchResult;

import java.util.List;

public interface SearchService {

    List<StopSearchResult> searchStop(String keyword);
}
