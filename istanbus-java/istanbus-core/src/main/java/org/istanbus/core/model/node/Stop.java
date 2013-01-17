package org.istanbus.core.model.node;

public class Stop {

    private String id;
    private String name;
    private String district;

    public Stop(String id, String name) {
        this.id = id;
        this.name = name;
    }

    public Stop() {
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDistrict() {
        return district;
    }

    public void setDistrict(String district) {
        this.district = district;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();
        sb.append("Stop");
        sb.append("{id='").append(id).append('\'');
        sb.append(", name='").append(name).append('\'');
        sb.append(", district='").append(district).append('\'');
        sb.append('}');
        return sb.toString();
    }
}
