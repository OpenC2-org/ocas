%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2016, sFractal Consulting LLC
%%% 
%%%-------------------------------------------------------------------

%%  illegal action in json
-define(NONSENSE, <<"{
\"action\": \"nonsense\",
\"target\": {
    \"type\":\"cybox:hostname\",
    \"specifiers\":{\"hostname_value\":\"cdn.badco.org\"}
    }
}">>).

