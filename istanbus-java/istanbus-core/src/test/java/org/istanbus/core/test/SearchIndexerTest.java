package org.istanbus.core.test;

import com.google.inject.Inject;
import org.istanbus.core.module.CoreModule;
import org.istanbus.core.runner.GuiceJUnitRunner;
import org.istanbus.core.service.SearchIndexService;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;

import java.io.IOException;

@RunWith(GuiceJUnitRunner.class)
@GuiceJUnitRunner.GuiceModule(CoreModule.class)
public class SearchIndexerTest {

    private SearchIndexService searchIndexService;

    @Inject
    public void setService(SearchIndexService searchIndexService) {
        this.searchIndexService = searchIndexService;
    }

    @Test
    public void testSearchIndex() {
        searchIndexService.indexAll();
        Assert.assertTrue(true);
    }
}
