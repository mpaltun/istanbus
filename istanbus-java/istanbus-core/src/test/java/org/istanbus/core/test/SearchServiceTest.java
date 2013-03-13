package org.istanbus.core.test;

import com.google.inject.Inject;
import org.istanbus.core.model.SearchResult;
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
    public void testSearchStopIndex() {
        String keyword = "tak";
        List<SearchResult> results = searchService.search("stop", keyword);
        logger.info("{} stops found for search: {}", results.size(), keyword);
        Assert.assertFalse(results.isEmpty());
    }

    @Test
    public void searchByIdBusIndexTest() {
        String keyword = "11m";
        List<SearchResult> results = searchService.search("bus", keyword);
        logger.info("{} bus found for search: {}", results.size(), keyword);
        Assert.assertFalse(results.isEmpty());
    }

    @Test
    public void enTrBusKeywordTest() {
        String keywordEn = "9üd";
        String keywordTr = "9ud";
        List<SearchResult> resultsEn = searchService.search("bus", keywordEn);
        List<SearchResult> resultsTr = searchService.search("bus", keywordTr);
        Assert.assertEquals(resultsEn.get(0).getName(), resultsTr.get(0).getName());
    }

    @Test
    public void busSearchByIdAndNameTest() {
        // fields for 9ÜD
        String keyword = "Ş.ŞAHİNBEY- DUDULLU- ÜMRANİYE-ÜSKÜDAR";
        String id = "9ud";

        List<SearchResult> idResult = searchService.search("bus", keyword);
        List<SearchResult> keywordResult = searchService.search("bus", id);
        Assert.assertEquals(idResult.get(0).getName(), keywordResult.get(0).getName());
    }

    @Test
    public void enTrStopKeywordTest() {
        String keywordEn = "kadıköy";
        String keywordTr = "kadikoy";
        List<SearchResult> resultsEn = searchService.search("stop", keywordEn);
        List<SearchResult> resultsTr = searchService.search("stop", keywordTr);
        Assert.assertEquals(resultsEn.size(), resultsTr.size());
    }

    /**
     * <a>http://github.com/challenge/istanbus/issues/8</a>
     */
    @Test
    public void testSearchBusIndexWithHyphen() {
        String keyword = "e-10";
        List<SearchResult> results = searchService.search("bus", keyword);
        Assert.assertFalse(results.isEmpty());
    }

    @Test
    public void testSearchWithExactBusCode()
    {
        String busCode = "12";
        List<SearchResult> results = searchService.search("bus", busCode);

        boolean foundInResults = false;
        for (SearchResult result : results)
        {
            if (busCode.equals(result.getId()))
            {
                foundInResults = true;
            }
        }

        Assert.assertTrue(foundInResults);

    }

}
