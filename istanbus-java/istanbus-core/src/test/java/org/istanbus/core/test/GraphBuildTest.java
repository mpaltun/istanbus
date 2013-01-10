package org.istanbus.core.test;

import com.google.inject.Inject;
import org.istanbus.core.module.CoreModule;
import org.istanbus.core.runner.GuiceJUnitRunner;
import org.istanbus.core.service.GraphBuildService;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(GuiceJUnitRunner.class)
@GuiceJUnitRunner.GuiceModule(CoreModule.class)
public class GraphBuildTest {

    private GraphBuildService graphBuildService;

    @Inject
    public void setService(GraphBuildService graphBuildService) {
        this.graphBuildService = graphBuildService;
    }

    @Test
    public void buildGraph() throws Exception {
        graphBuildService.buildFullGraph();
        Assert.assertTrue(graphBuildService.testGraph("taksim-beyoglu"));
    }

}
