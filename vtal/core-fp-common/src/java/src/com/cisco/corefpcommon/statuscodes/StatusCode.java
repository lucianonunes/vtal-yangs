/*
 ***************************************************
 * Copyright (c) 2019 Cisco Systems, Inc.
 *
 * All rights reserved.
 ***************************************************
 */
package com.cisco.corefpcommon.statuscodes;

import java.util.Objects;

/**
 * StatusCode pojo to hold status code information.
 *
 * @author abhatta
 */
public class StatusCode {
    private String code;
    private String reason;
    private String category;
    private String severity;
    private String recommendedActions;

    public StatusCode(String cfpName, String code) {
        this.code = (cfpName + "-" + code).toUpperCase();
    }

    public String getCode() {
        return code;
    }

    public String getReason() {
        return reason;
    }

    public void setReason(String reason) {
        this.reason = reason;
    }

    public String getCategory() {
        return category;
    }

    public void setCategory(String category) {
        this.category = category;
    }

    public String getSeverity() {
        return severity;
    }

    public String getRecommendedActions() {
        return recommendedActions;
    }

    public void setSeverity(String severity) {
        this.severity = severity;
    }

    public void setRecommendedAction(String recommendedActions) {
        this.recommendedActions = recommendedActions;
    }

    @Override
    public int hashCode() {
        return Objects.hash(code);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        StatusCode other = (StatusCode) obj;
        return Objects.equals(code, other.code);
    }

    @Override
    public String toString() {
        return "StatusCode [code=" + code + ", reason=" + reason + ", category=" + category
            + ", severity=" + severity + ", recommendedActions=" + recommendedActions + "]";
    }
}
