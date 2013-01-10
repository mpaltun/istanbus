package org.istanbus.core.service;

import java.util.List;

import com.sun.tools.corba.se.idl.constExpr.BooleanNot;
import org.istanbus.core.model.node.Bus;

public interface GraphBuildService
{
    void buildFullGraph(List<Bus> busList);

    void buildFullGraph();

    boolean testGraph(String stopCode);

}
