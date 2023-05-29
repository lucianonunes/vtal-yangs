package com.cisco.corefpcommon.planutils;

import java.util.List;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import com.tailf.conf.ConfBuf;
import com.tailf.navu.NavuContainer;
import com.tailf.navu.NavuException;
import com.tailf.navu.NavuList;
import com.tailf.navu.NavuNode;
import com.tailf.ncs.PlanComponent;

/**
 * @author sahpatib
 *
 */
public class CoreFpPlanUtil {
    public static Logger log = LogManager.getLogger(CoreFpNameUtil.class);

    /**
     * @param service
     * @param entityName
     * @param componentType
     * @param states
     * @return
     * @throws NavuException Creating Plan components and appending states to the plan component.
     *         All the states are set to not-reached.
     */
    public static PlanComponent writePlanComponent(NavuNode service, String entityName,
        String componentType, List<String> states) throws NavuException {
        PlanComponent component = new PlanComponent(service, entityName, componentType);
        for (String state : states) {
            component.appendState(state);
        }
        return component;
    }

    /**
     * @param service
     * @param entityName
     * @param componentType
     * @param states
     * @param reachedStates
     * @return
     * @throws NavuException
     * 
     *         Creates/Updates plan component with given states. Updates given plan states to ready.
     */
    public static PlanComponent writePlanComponentReached(NavuNode service, String entityName,
        String componentType, List<String> states, List<String> reachedStates)
        throws NavuException {
        PlanComponent component = writePlanComponent(service, entityName, componentType, states);
        for (String state : reachedStates) {
            component.setReached(state);
        }
        return component;
    }

    /**
     * @param service
     * @param entityName
     * @param componentType
     * @param states
     * @param reachedStates
     * @param failedState
     * @return
     * @throws NavuException
     * 
     *         Creates/Updates plan component with given states. Sets the multiple states to reached
     *         or given state to failed.
     */
    public static PlanComponent writePlanComponentReachedorFailed(NavuNode service,
        String entityName, String componentType, List<String> states, List<String> reachedStates,
        String failedState) throws NavuException {
        PlanComponent component = writePlanComponent(service, entityName, componentType, states);
        for (String state : states) {
            if (reachedStates.contains(state)) {
                component.setReached(state);
            }
        }
        component.setFailed(failedState);
        return component;
    }

    /**
     * @param service
     * @param entityName
     * @param componentType
     * @param states
     * @param reachedStates
     * @param failedStates
     * @return
     * @throws NavuException
     * 
     *         Creates/Updates plan component with given states. It updates the given list of states
     *         to failed or reached states.
     */
    public static PlanComponent writePlanComponentReachedorFailed(NavuNode service,
        String entityName, String componentType, List<String> states, List<String> reachedStates,
        List<String> failedStates) throws NavuException {
        PlanComponent component = writePlanComponent(service, entityName, componentType, states);
        for (String state : states) {
            if (reachedStates.contains(state)) {
                component.setReached(state);
            }
        }
        for (String state : states) {
            if (failedStates.contains(state)) {
                component.setFailed(state);
            }
        }
        return component;
    }

    /**
     * @param service
     * @param entityName
     * @param componentType
     * @param states
     * @param reachedState
     * @return
     * @throws NavuException
     * 
     *         Creates/Updates plan component with given states. Sets the given state to reached.
     */
    public static PlanComponent writePlanComponentReached(NavuNode service, String entityName,
        String componentType, List<String> states, String reachedState) throws NavuException {
        PlanComponent component = writePlanComponent(service, entityName, componentType, states);
        component.setReached(reachedState);
        return component;
    }

    /**
     * @param component
     * @param notreachedStates
     * @return
     * @throws NavuException
     * 
     *         Updates the given component states to not-reached.
     */
    public static PlanComponent writePlanComponentNotReached(PlanComponent component,
        List<String> notreachedStates) throws NavuException {
        for (String state : notreachedStates) {
            component.setNotReached(state);
        }

        return component;
    }

    /**
     * @param service
     * @param componentName
     * @param field
     * @param value
     * @throws NavuException
     */
    public static void writeComponentField(NavuNode service, String componentName, String field,
        String value) throws NavuException {
        if (value == null) {
            log.warn("componentName=" + componentName + " field=" + field + " value null.");
            return;
        }
        ConfBuf bufValue = new ConfBuf(value);
        NavuContainer plan = service.container(PlanConstants.PLAN);
        NavuList components = plan.list(PlanConstants.COMPONENT);
        NavuContainer comp = components.elem(componentName);
        comp.leaf(field).set(bufValue);
    }

}
