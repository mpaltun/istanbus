package org.istanbus.core.util;

import org.istanbus.core.dao.StopDAO;
import org.istanbus.core.model.node.Stop;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Created with IntelliJ IDEA.
 * User: mustafa
 */
public class StopRequestCache implements StopDAO
{
    private StopDAO stopDAO;
    private Map<String, Stop> cache = new HashMap<String, Stop>();

    public StopRequestCache(StopDAO stopDAO)
    {
        this.stopDAO = stopDAO;
    }

    @Override
    public List<Stop> loadAll()
    {
        throw new UnsupportedOperationException("not supported");
    }

    @Override
    public Stop loadById(String id)
    {
        Stop stop = cache.get(id);
        if (stop == null)
        {
            stop = stopDAO.loadById(id);
            cache.put(id, stop);
        }
        return stop;
    }

    @Override
    public Stop loadById(String id, boolean loadBus)
    {
        Stop stop = cache.get(id);
        if (stop == null)
        {
            stop = stopDAO.loadById(id, loadBus);
            cache.put(id, stop);
        }
        return stop;
    }
}
