package org.istanbus.core.model.node;

import java.util.List;

public class Bus
{
    private String id;
    private String name;
    private Stops stops;

    public String getId()
    {
        return id;
    }

    public void setId(String id)
    {
        this.id = id;
    }

    public String getName()
    {
        return name;
    }

    public void setName(String name)
    {
        this.name = name;
    }

    public Stops getStops() {
        return stops;
    }

    public void setStops(Stops stops) {
        this.stops = stops;
    }

    @Override
    public boolean equals(Object o)
    {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Bus bus = (Bus) o;

        if (!id.equals(bus.id)) return false;

        return true;
    }

    @Override
    public int hashCode()
    {
        return id.hashCode();
    }

    public class Stops
    {
        private List<Stop> go;
        private List<Stop> turn;

        public List<Stop> getGo() {
            return go;
        }

        public void setGo(List<Stop> go) {
            this.go = go;
        }

        public List<Stop> getTurn() {
            return turn;
        }

        public void setTurn(List<Stop> turn) {
            this.turn = turn;
        }

        @Override
        public String toString() {
            final StringBuilder sb = new StringBuilder();
            sb.append("Stops");
            sb.append("{go=").append(go);
            sb.append(", turn=").append(turn);
            sb.append('}');
            return sb.toString();
        }
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();
        sb.append("Bus");
        sb.append("{id='").append(id).append('\'');
        sb.append(", name='").append(name).append('\'');
        sb.append(", stops=").append(stops);
        sb.append('}');
        return sb.toString();
    }
}
