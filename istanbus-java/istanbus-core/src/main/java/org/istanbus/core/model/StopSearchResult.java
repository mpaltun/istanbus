package org.istanbus.core.model;

public class StopSearchResult extends SearchResult {

    private String district;

    public StopSearchResult(String id, String name, String district) {
        super(id, name);
        this.district = district;
    }

    public String getDistrict() {
        return district;
    }

    public void setDistrict(String district) {
        this.district = district;
    }

}
