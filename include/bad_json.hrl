%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2016, sFractal Consulting LLC
%%% 
%%%-------------------------------------------------------------------

%%  bad json  - missing closing brace
-define(BADJSON, <<"{
\"action\": \"mitigate\",
\"target\": {
    \"type\":\"cybox:hostname\",
    \"specifiers\":{\"hostname_value\":\"cdn.badco.org\"}
    }
">>).

