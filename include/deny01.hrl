%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2016, sFractal Consulting LLC
%%% 
%%%-------------------------------------------------------------------

%%  json for allow
-define(DENY01, <<"{
\"action\": \"deny\",
\"target\": { 
    \"type\": \"cybox:address\",
    \"specifiers\": {
        \"cybox:address_object_type\" : \"ipv4-addr\",
        \"address-value\" : \"192.168.22.33\",
        },
    },
\"actuator\": {
    \"type\": \"network-firewall\",
    \"specifiers\": \"fw01\"
    },
\"modifiers\": {
    \"response\": \"ack\",
    \"where\": \"perimeter\"
    }
}">>).
