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

    @Inject
    public void setService(PathFinderService pathFinderService) {
        this.pathFinderService = pathFinderService;
    }

    @Test
    public void testPathFind() throws Exception {
        // from taksim to kadikoy carsi
        List<Transport> transports = pathFinderService.find("Ş0015", "A0587");
        Assert.assertNotEquals(transports.size(), 0);
    }
}
