package org.istanbus.core.model;

import org.istanbus.core.model.node.Bus;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Set;

public class PathResult
{
    private List<SuggestedRoute> perfectRoutes;
    private List<SuggestedRoute> suggestions;

    private static final int DEFAULT_LIMIT = 5;
    private static final Comparator<SuggestedRoute> suggestionComparator = new Comparator<SuggestedRoute>()
    {
        @Override
        public int compare(SuggestedRoute route1, SuggestedRoute route2)
        {
            return route1.getRoutes().size() - route2.getRoutes().size();
        }
    };

    public List<SuggestedRoute> getPerfectRoutes()
    {
        return perfectRoutes;
    }

    public void setPerfectRoutes(List<SuggestedRoute> perfectRoutes)
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

    public PathResult sort()
    {
        Collections.sort(suggestions, suggestionComparator);
        return this;
    }

    public PathResult limit()
    {
        return limit(0, DEFAULT_LIMIT);
    }

    public PathResult limit(int offset, int limit)
    {
        if (suggestions.size() < limit)
        {
            limit = suggestions.size();
        }

        List<SuggestedRoute> suggestions = this.suggestions.subList(offset, limit);
        setSuggestions(suggestions);

        return this;
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
