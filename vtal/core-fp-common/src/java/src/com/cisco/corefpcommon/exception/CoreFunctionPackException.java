/*
 ***************************************************
 * Copyright (c) 2019 Cisco Systems, Inc.
 *
 * All rights reserved.
 ***************************************************
 */
package com.cisco.corefpcommon.exception;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import com.cisco.corefpcommon.statuscodes.StatusCode;
import com.cisco.corefpcommon.statuscodes.StatusCodes;

/*
 * The CoreFunctionPackException class is the superclass of all exceptions core
 * function pack can throw. This imposes that all CFP exceptions are checked
 * exceptions. Besides capturing basic information like message, cause and/or
 * suppressed exceptions, It holds a list of context information which allows to
 * enrich the exception by recording relevant context/process state data as it
 * moves up along the call stack.
 *
 * Any NSO exception should not be propagated, instead it should be wrapped with
 * CoreFunctionPackException or its subclasses. In other words the core function
 * packs should throw CoreFunctionPackException or its subclasses only.
 *
 * CoreFunctionPackException doesn't allow disabling suppressed exceptions.
 *
 * Usage:
 *      CoreFunctionPackException thr = null;
 *       try {
 *          //do something..
 *       } catch (ConfException exp) {
 *           thr = new CoreFunctionPackException("Failed to read xyz",
 *               myerrorcodes.FAILED_TO_READ_XYZ, exp)
 *                .setContext("Interface creation", "Failed to create WAN interface")
 *                  .addState("Interface", InterfaceId).addState("Device", device).finish();
 *           throw thr;
 *       } finally {
 *          try {
 *              //do something else..
 *          } catch (IOException exp) {
 *              if(thr != null)
 *                  thr.addSuppressed(exp);
 *          }
 *       }
 *
 * By convention, the subclasses should have three constructors, one that takes detail
 * message and error code, one that takes only error code and the one that takes a detail
 *  message, error code and a cause.
 */
/**
 * @author srchundu
 * @author abhatta
 */
public class CoreFunctionPackException extends Exception {

    private static final long serialVersionUID = 489227425359036082L;
    private static final String NULL = "NULL";

    /*
     * It is by design that no-parameter constructor is not provided. A clear
     * detailed message should be provided when creating an exception.
     */

    private final StatusCode statusCode;
    private final transient List<Context> contextInfo = new ArrayList<>();

    protected CoreFunctionPackException(String statusCode, String cfpName) {
        super();
        this.statusCode = StatusCodes.getStatusCode(cfpName, statusCode);
    }

    protected CoreFunctionPackException(String message, String statusCode, String cfpName) {
        super(message);
        this.statusCode = StatusCodes.getStatusCode(cfpName, statusCode);
    }

    protected CoreFunctionPackException(String message, String statusCode, Throwable cause,
        String cfpName) {
        super(message, cause);
        this.statusCode = StatusCodes.getStatusCode(cfpName, statusCode);
    }

    public Context setContext(String context) {
        Context newcontext = new Context(context);
        contextInfo.add(newcontext);
        return newcontext;
    }

    public Context setContext(String name, String contextMessage) {
        Context newcontext = new Context(name, contextMessage);
        contextInfo.add(newcontext);
        return newcontext;
    }

    public String getStatusCode() {
        return this.statusCode.getCode();
    }

    public String getReason() {
        return this.statusCode.getReason();
    }

    public String getCategory() {
        return this.statusCode.getCategory();
    }

    public String getSeverity() {
        return this.statusCode.getSeverity();
    }

    public String getRecommendedActions() {
        return this.statusCode.getRecommendedActions();
    }


    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append(super.toString());
        builder.append("\nSTATUS_CODE: ").append(getStatusCode()).append("\n");
        builder.append("REASON: ").append(getReason()).append("\n");
        builder.append("CATEGORY: ").append(getCategory()).append("\n");
        builder.append("SEVERITY: ").append(getSeverity()).append("\n");
        for (int i = contextInfo.size() - 1; i >= 0; i--) {
            builder.append(contextInfo.get(i).toString());
        }
        buildST(builder, getStackTrace());
        if (super.getCause() != null) {
            builder.append(super.getCause());
            buildST(builder, super.getCause().getStackTrace());
        }
        return builder.toString();
    }

    private void buildST(StringBuilder builder, StackTraceElement[] trace) {
        for (StackTraceElement elem : trace) {
            builder.append("\t").append(elem).append("\n");
        }
    }

    /*
     * Context captures relevant data when exception is propagated up the call
     * stack. Doing this will save you big time when trying to diagnose and
     * reproduce errors. The state HashMap captures the state information.
     * Adding context to the Exception follows a fluent interface pattern.
     *
     * throw new CoreFunctionPackException("Failed to read xyz",
     *    myerrorcodes.FAILED_TO_READ_XYZ)
     *      .setContext("Interface creation", "Failed to create WAN interface")
     *        .addState("Interface", InterfaceId).addState("Device", device).finish();
     *
     */
    public class Context {
        private String name;
        private HashMap<String, String> state = new HashMap<String, String>();
        private String message = NULL;

        public Context(String context) {
            this.name = context;
        }

        public Context(String context, String message) {
            this.name = context;
            this.message = message;
        }

        public Context addState(String key, String value) {
            state.put(key, value);
            return this;
        }

        public CoreFunctionPackException finish() {
            return CoreFunctionPackException.this;
        }

        @Override
        public String toString() {
            StringBuilder builder = new StringBuilder();
            builder.append("Context [name = ").append(name);
            if (!NULL.equals(message)) {
                builder.append(", message = ").append(message);
            }
            if (!state.isEmpty()) {
                builder.append("\n").append(" state = ").append(state);
            }
            builder.append(" ] \n");
            return builder.toString();
        }
    }
}
