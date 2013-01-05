package org.istanbus.core.test;

import com.google.inject.Inject;
import org.istanbus.core.model.Transport;
import org.istanbus.core.module.CoreModule;
import org.istanbus.core.runner.GuiceJUnitRunner;
import org.istanbus.core.service.PathFinderService;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;

import java.util.List;

@RunWith(GuiceJUnitRunner.class)
@GuiceJUnitRunner.GuiceModule(CoreModule.class)
public class PathFindTest {

    private PathFinderService pathFinderService;

    // from taksim to kadikoy carsi
    private String from = "Ş0015";
    private String to = "A0587";

    @Inject
    public void setService(PathFinderService pathFinderService) {
        this.pathFinderService = pathFinderService;
    }

    @Test
    public void testPathFind() throws Exception {
        List<Transport> transports = pathFinderService.find(from, to);
        Assert.assertFalse(transports.isEmpty());
    }

    @Test
    public void testPathFindConsistency() throws Exception {
        List<Transport> transports = pathFinderService.find(from, to);

        Assert.assertFalse(transports.isEmpty());

        Transport first = transports.get(0);
        Transport last = transports.get(transports.size() - 1);

        Assert.assertEquals(from, first.getFrom().getId());
        Assert.assertEquals(to, last.getTo().getId());
    }

}
