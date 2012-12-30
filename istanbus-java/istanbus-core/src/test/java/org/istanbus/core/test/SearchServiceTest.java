package org.istanbus.core.test;

import com.google.inject.Inject;
import org.istanbus.core.model.StopSearchResult;
import org.istanbus.core.module.CoreModule;
import org.istanbus.core.runner.GuiceJUnitRunner;
import org.istanbus.core.service.SearchService;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

@RunWith(GuiceJUnitRunner.class)
@GuiceJUnitRunner.GuiceModule(CoreModule.class)
public class SearchServiceTest {

    private SearchService searchService;

    private static final Logger logger = LoggerFactory.getLogger(SearchServiceTest.class);

    @Inject
    public void setService(SearchService searchIndexService) {
        this.searchService = searchIndexService;
    }

    @Test
    public void testSearchIndex() {
        String keyword = "tak";
        List<StopSearchResult> results = searchService.searchStop(keyword);
        logger.info("{} stops found for search: {}", results.size(), keyword);
        Assert.assertFalse(results.isEmpty());
    }

    @Test
    public void enTrKeywordTest() {
        String keywordEn = "uskudar";
        String keywordTr = "üsküdar";
        List<StopSearchResult> resultsEn = searchService.searchStop(keywordEn);
        List<StopSearchResult> resultsTr = searchService.searchStop(keywordTr);
        Assert.assertEquals(resultsEn.size(), resultsTr.size());
    }
}
