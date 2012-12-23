package org.istanbus.core.model.node;

public class Stop
{
    private String code;
    private String name;

    public Stop(String code, String name)
    {
        this.code = code;
        this.name = name;
    }

    public Stop() {
    }

    public String getCode()
    {
        return code;
    }

    public void setCode(String code)
    {
        this.code = code;
    }

    public String getName()
    {
        return name;
    }

    public void setName(String name)
    {
        this.name = name;
    }

    @Override
    public String toString() {
        return "Stop{" +
                "code='" + code + '\'' +
                ", name='" + name + '\'' +
                '}';
    }
}
