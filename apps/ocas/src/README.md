To understand the code, here are some scenarios showing what modules/functions are used.

## Cowboy
ocas_app starts a cowboy webserver with ranch listeners.

routes are established within cowboy so that when 
http requests contain the following paths
then the following handlers are invoked:
- status    status_handler.erl
- ok        status_ok_handler.erl
- openc2    openc2_handler.erl

## status_ok_handler.erl
status_ok_handler.erl is a simple routing that only excepts GET, 
ignores any parameters,
and returns "ok".
This is used as a keepalive so you know the simulator is accepting requests.


## openc2_handler.erl

The sunny day path thru openc2_handler is documented in the following example.
For this example, it is assumed a POST to /openc2 was sent with valid JSON containg a valid SCAN action eg:

```erlang
{ "action": "scan",
              "target": { "type": "cybox:Device",
                          "specifiers": "NetworkScanner"
                         },
               "actuator": {
                  "type": "network-scanner",
                  "specifiers": "scanner01"
                           },
               "modifiers": { "response": "ack",
                              "where": "perimeter"}
             }
```


The flow is as follows:

1. init/3 initializes the handler to use REST (and therefore rest_init/2 is called
2. rest_init/2 just logs some info and sets the State variable to empty
3. cowboy calls allowed_methods/2 which passes since since a POST was sent
4. cowboy calls content_types_accepted/2 which passes control to handle_json 
since the json header was sent
5. handle_json/2 checks if the request contains a body (it does) and tail recurses to body_check/3 with the first parameter true
6. body_check(true,...) checks if body is json (it is) and tail recurses to is_body_json/3 with the first parameter true
7. is_body_json(true,...) decodes the JSON into erlang terms (and stores them in State) and then checks if action is in the JSON (it is) and tail recurses to has_action/3 with first parameter true
8. has_action(true,...) gets some info to put in State and calls actions:spawn_action/3


## actions.erl
continuing the scan example above

8. spawn_action( <<"scan">>,  Req, State ) matches. It calls act_scan:start(State) to spawn the scan_server. Note act_scan.erl is in the action_servers directory. Spawn_action  then sends the newly spawned server a keepalive. It tail recurses to action_valid with Action=scan, the Pid returned from starting the allow server, the response from the keepalive (in this case {keepalive_received, scan_server}), and Req(for http housekeeping)
9. action_valid(allow,...) stores some State (that it was a valid action, and action=scan) and tail recurses to verify_keepalive
10. verify_keepalive( {keepalive_received, scan_server} matches (because in this sunny day example the keepalive worked). verify_keepalive records keepalive true in State and calls send_response
11. send_response formats the http reply - which at this poit is just putting some State info in json

above is all independent of cybox part at this point (ie cybox stuff is yet to be coded)

