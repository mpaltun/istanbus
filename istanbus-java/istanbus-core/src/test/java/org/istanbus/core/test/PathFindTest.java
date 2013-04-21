package org.istanbus.core.test;

import com.google.inject.Inject;
import org.istanbus.core.module.CoreModule;
import org.istanbus.core.runner.GuiceJUnitRunner;
import org.istanbus.core.service.PathFinderService;
import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(GuiceJUnitRunner.class)
@GuiceJUnitRunner.GuiceModule(CoreModule.class)
public class PathFindTest {

    private PathFinderService pathFinderService;

    // from taksim to kadikoy carsi
    private String from = "taksim-beyoglu";
    private String to = "kadikoy-kadikoy";

    @Inject
    public void setService(PathFinderService pathFinderService) {
        this.pathFinderService = pathFinderService;
    }

    @Test
    public void testPathFind() throws Exception {
//        PathResult result = pathFinderService.find(from, to);
//        Assert.assertNotNull(result.getSolutions());
//        Assert.assertFalse(result.getSolutions().isEmpty());
    }

    @Test
    public void testPathFindConsistency() throws Exception {
//        PathResult result = pathFinderService.find(from, to);
//
//        List<SuggestedRoute> solutions = result.getSolutions();
//        SuggestedRoute solution = solutions.get(0);
//        List<Route> transports = solution.getTransports();
//
//        Assert.assertFalse(transports.isEmpty());
//
//        Route first = transports.get(0);
//        Route last = transports.get(transports.size() - 1);
//
//        Assert.assertEquals(from, first.getFrom().getId());
//        Assert.assertEquals(to, last.getTo().getId());
    }

}
