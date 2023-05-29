package com.cisco.corefpcommon.planutils;

import java.util.ArrayList;
import java.util.List;

import com.tailf.ncs.ns.Ncs;

public class PlanConstants {
    public static final List<String> INIT_READY_LIST = new ArrayList<String>();
    public static final List<String> INIT_VNF_LIST = new ArrayList<String>();
    public static final String INIT_STATE_WITH_PREFIX = Ncs.prefix + ":" + Ncs._init_;
    public static final String READY_STATE_WITH_PREFIX = Ncs.prefix + ":" + Ncs._ready_;
    public static final String FAILED_STATE_WITH_PREFIX = Ncs.prefix + ":" + Ncs._failed_;
    public static final String SELF_TYPE_WITH_PREFIX = Ncs.prefix + ":" + Ncs._self_;
    public static final String PLAN = "plan";
    public static final String COMPONENT = "component";
    
    static {
        INIT_READY_LIST.add(INIT_STATE_WITH_PREFIX);
        INIT_READY_LIST.add(READY_STATE_WITH_PREFIX);  
    }
}
