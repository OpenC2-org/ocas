#Understanding Status thru tests
This section is an attempt to explain what is implemented so far 
and what needs to be done next.
It will use tests as a way to do this.
Tests that pass show functioning code.
Work to be done is shown thru tests that are incomplete, non-existent, or failing.
Status is:
- P - tests written, code written, tests pass
- F - tests written, tests fail
- N - tests not written
- ? - some other state that needs explaining

## 1. Sanity

- P - test GET to /ok correctly processed
   * status=200, body=ok
- P - test GET to /status simply processed 
   * status=200, body=meaningless for now until real status code added
- P - test GET to /openc2 correctly processed (rejected with 405 return)
- P - test no-http-body correctly processed (rejected with 400 return)
- P - test media-type-not-json correctly processed (rejected with 415 return)
- P - test bad json correctly processed (rejected with 400 return)

This is 100% coverage of this aspect.

## 2. Simple single command validator

### 2.1 Action Sanity
- P - test action=nonsense correctly processed (rejected with 400 return)
   * note this is not exhautive test of all invalid actions
- P - test all 35 actions 'simply' processed (ie not Response and Alert)

This is 100% coverage of this aspect.

### 2.2 Target Sanity
- P - test target=hostname simply processed
- P - test target=ipv4 simply processed
- P - test target=network connection simply processed
- P - test target=network firewall simply processed
- N - test rest of 24 targets simply processed

This is 16% coverage of this aspect.

### 2.3 Actuator Sanity
- P - test actuator=network-firewall simply processed
- P - test actuator=network-router simply processed
- P - test actuator=network-scanner simply processed
- P - test works correctly when no actuator (ie demense simply processed)
- N - test rest of 39 actuators simply processed

This is 9% coverage of this aspect.

### 2.4 Modifier Sanity
- N - test modifier = ? simply processed
- N - test rest of modifiers simply processed

### 2.6 Response & Alert Sanity
- N - test response sanity
- N - test alert sanity

### 2.7 Command Specifics
- N - test following command and response
   * action =  deny
   * target = network connection
   * actuator = network firewall
   * modifiers = (fill out correctly for reply requested, command_id)
- N - test following command
   * action =  fill in
   * target = fill in
   * actuator = fill in
   * modifiers = (fill out correctly for reply requested, command_id)
- N - test all X targets simply processed
- N - test all X actuators simply processed
- N - test all X modifiers simply processed

## 3. Simple single command simulator
what goes here?

## 4. Playbook simulator
what goes here?

-license ApacheV2 
