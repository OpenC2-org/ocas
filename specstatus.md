#Development Status as a function of Specification Coverage.

Another way to consider status is how much of the specification is covered by what has so far been 
implemented.
This gets tricky due to the interactions and permuations.

## Simple Spec Coverage
### Actions
There are 35 actions in the specification. 
All 35 actions are coded far enough to accept those 35 commands
and reject if the action is not one of those 35. 
This is 100% coverage of this aspect.

### Response, Alert
These have not been addressed yet (0% coverage)

### Targets
Target is a required field. 
If any of the 35 actions do not have a target, 
an http 400 response is generated.

There are 28 targets in the specification.
Currently there are 4 targets "working":
- hostname
- ipv4 address
- network connection
- network firewall

This is 14% coverage of this aspect.

### Actuators

There are 43 actuators in the specification.
Currently there are 4 actuators "working":
- demense (ie if no actuator, then entire domain of consumer)
- network firewall
- network router
- network scanner

This is 9% coverage of this aspect.

### Modifiers

There are TBD modifiers in the specification.
Currently there are 0 targets "working",
so this is 0% coverage

### Linear Coverage

linear add of types yeilds ~25% coverage.

### Action/Target Combo's
However the semantics of the commands are more complicated.
For example, there are valid action/target combinations as well as invalid action/target combinations.


more work needed here
