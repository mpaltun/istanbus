package org.istanbus.core.model;

import org.istanbus.core.model.node.Bus;

import java.util.List;
import java.util.Set;

public class PathResult
{
    private Set<Bus> perfectRoutes;
    private List<SuggestedRoute> suggestions;

    public Set<Bus> getPerfectRoutes()
    {
        return perfectRoutes;
    }

    public void setPerfectRoutes(Set<Bus> perfectRoutes)
    {
        this.perfectRoutes = perfectRoutes;
    }

    public List<SuggestedRoute> getSuggestions()
    {
        return suggestions;
    }

    public void setSuggestions(List<SuggestedRoute> suggestions)
    {
        this.suggestions = suggestions;
    }

    @Override
    public String toString()
    {
        final StringBuilder sb = new StringBuilder("PathResult{");
        sb.append("perfectRoutes=").append(perfectRoutes);
        sb.append(", suggestions=").append(suggestions);
        sb.append('}');
        return sb.toString();
    }
}
