ocas
=====

## Table of Contents
   * [1. Intro](#intro)
   * [2. Ground Rules](#ground)
      + [2.1 License](#2.1)
      + [2.2 Code of Conduct](#2.2)
      + [2.3 Collective Code Construction Contract (C4)](#2.3)
      + [2.4 Style Guide](#2.4)
   * [3. Getting it running](#3)
      + [3.1 Build](#3.1)
      + [3.2 Build & Test](#3.2)
      + [3.3 Pull Requests](#3.3)
      + [3.4 Release](#3.4)
   * [4. Vision](#4)
      + [4.1 Single-command Verification](#4.1)
      + [4.2 Single-command Orchestrator/Actuator Consumer Simulator](#4.2)
      + [4.3 Playbook Simulator](#4.3)
      + [4.4 Implementation Template](#4.4)
   * [5. Organization of this software](#5)
      + [5.1 status_handler](#5.1)
      + [5.2 status_ok_handler](#5.2)
      + [5.3 openc2_handler](#5.3)
      + [5.4 Actions](#5.4)
      + [5.5 More on software design](#5.5)
   * [6. Development Status](#6)
   * [7. Specification Coverage](#7)
   * [8. Examples](#8)
   * [9. Want to help?](#9)


##1. Intro <a id="intro"></a>

ocas - plural of oca, a edible tuber of Oxalis Tuberosa, a wood sorrel of the Andes; 
or maybe it's an acronym OCAS - OpenC2 API Simulator.

Ocas is an OTP application written in erlang to:
- prove out the OpenC2 spec (see http://openc2.org/ and https://github.com/OpenC2-org/)
- be a viable simulator for testing OpenC2 code and scenarios
- be a template for developing actual openc2 applications (ie replace the simulator code with code to actually perform the real function)

Ocas is being developed in erlang because:
- Concurrent, parallel, cloud computing
- Five 9’s or greater of reliability
- Erlang facilitates using the actor model and message passing for simulation (ie scales well for complex network simulations)


##2. Ground Rules <a id="ground"></a>
###2.1 License <a id="2.1"></a>
Copyright 2016 sFractal Consulting

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

###2.2 Code of Conduct <a id="2.2"></a>
TL;DR - Don't be a jerk!

To make sure there are no barriers to ocas development 
for any developers who want to get involved, 
a Code of Conduct was adopted and can be found at 
[code_of_conduct.md](./code_of_conduct.md). 
Please read and ahere to it.
This Code of Conduct is adapted from the Contributor Covenant, 
version 1.4.0, available at 
[http://contributor-covenant.org/](http://contributor-covenant.org/).

###2.3 Collective Code Construction Contract (C4) <a id="2.3"></a>
C4 provides a standard process for contributing, evaluating and discussing improvements on software projects. 
It defines specific technical requirements for projects 
like a style guide, unit tests, git and similar platforms. 
It also establishes different personas for projects, 
with clear and distinct duties. 
C4 specifies a process for documenting 
and discussing issues including seeking consensus and clear descriptions, 
use of "pull requests" and systematic reviews.

This C4 was adopted from [https://rfc.zeromq.org/spec:42/C4/](https://rfc.zeromq.org/spec:42/C4/) under GNU GPLv3.

Need to actually add the c4 to the repo. Need to edit zeromq - apache lisence instead of GPL, any other?

###2.4 Style Guide <a id="2.4"></a>
A standalong style guide needs to be established. 
Elvis (https://github.com/inaka/elvis) is used for verifying 
and for now the style guide is the rules in [elvis.config](./elvis.config).

##3. Getting it running <a id="3"></a>
###3.1 Build <a id="3.1"></a>

    $ rebar3 compile

###3.2 Build & Test <a id="3.2"></a>
To run complete set of test suites:

    $ rebar3 ct

To run an individual test suite:

    $ rebar3 ct --suite=test/query_SUITE

###3.3 Pull Requests <a id="3.3"></a>
As part of the quality checks on merging, github pull requests automagically run gadgetci which:
- validates the code compiles error free
- runs elvis
- runs xref
- runs dialyzer

###3.4 Release <a id="3.4"></a>
not done yet.
For now, you can 

    $ rebar shell

to build and run from command line

##4. Vision <a id="4"></a>
The vision is to have 
this code running on a cloud server, 
with a protected interface, 
that could be used for interworking testing. 
See TBD for the glue to do this.

###4.1 Single-command Verification <a id="4.1"></a>
A primary purpose (and first use) of the simulator is to uncover bugaboos in the openC2 specification. 
One such has already been uncovered (the mixed case issue). 
The openC2 specification is an attempt to simplify and standardize the command and control of all aspects of security. 
Alternative implementations, particularly if they can interoperate, help uncover discrepancies and different interpretations allowing the spec to be improved. 
Having ocas and the python reference implementation interact will go a long way towards validating the openC2 specification.

The first phase of software development will focus on getting 
an ocas generic consumer
up and running and accepting a single command. 
The software is being architected to both allow for easily scaling to a full network simulation and for using as the base for either an orchestrator or an actuator.
But the generic consumer only accepts a single command (ie does not have state information about the system being simulated) 
and only verifies the command is 'valid from a language viewpoint'.

###4.2 Single-command Orchestrator/Actuator Consumer Simulator <a id="4.2"></a>
The next phase of development (still future at this writing) will include state information for 
instantiating either an orchestrator-consumer (eg as seen from another orchestrator)
or an actuator-consumer (eg as seen from an orchestor).
The state information 'informs' the simulator as to the 'context' of the environment so it can respond appropriately. 
One use of this is still for specification validation for error conditions such as requesting resources that are not present.


###4.3 Playbook Simulator <a id="4.3"></a>
Once the single-command simulator is fully functional, it will be extended to multi-command to allow playbook simulation of a full network. 
This will involve retaining state information which will be then extended 
to allow multiple orchestrators and the study of race conditions and temporal vulnerabilities.

###4.4 Implementation Template <a id="4.4"></a>
Another vision is for this code be forked and 'filled in' as actual working implementations.
The software has been architected to serve as a base for production security software as either an orchestrator or an actuator.
See TBD for one example.


##5. Organization of this software <a id="5"></a>

The directory structure is follows the erlang OTP convention. Of interest:
- ./rebar.config contains the dependencies for building
  * cowboy is used for the webserver
  * jsx is used for json creation/parsing
  * lager is used for logging
  * shotgun is used just in testing as an http test client
- ./test contains tests for ct to execute. A test-driven-development is being attempted (eg first write a test, have it fail, and then develop the code to get it to pass, iterate).
- ./apps/ocas/src contains the meat 
  * ocas.app.src - OTP application resource file
  * ocas_sup.erl - OTP supervisor
  * ocas_app.erl - API for ocas

The cowboy webserver is used.

The start/2 module in ocas_app.erl is the callback run 
when the webserver is started. 
This module calls start_webserver() 
which contains the ocas specific software 
including a set of compiled routes 
which map url’s to the handler callbacks to run:
- /status will run status_hander callbacks 
   * for admin to find status
- /ok will run status_ok_handler callbacks
   * keepalive that just returns ok when all is ok
- /openc2 will run openc2_handler callbacks
   * receives the openc2 json, validates it, and executes what is in the openc2 command in the simulator

###5.1 status_handler  <a id="5.1"></a>
This is a future feature to allow those with administrative access
to get status information about the simulator itself 
(as opposed to about the network being simulated which would use openC2 commands). 
At the current point this api takes no parameters 
and returns the html for “Status Works - needs more later”.

###5.2 status_ok_handler <a id="5.2"></a>
This api returns a simple “ok” in either text, html, or json. This is to serve as a keepalive if one is needed.

###5.3 openc2_handler <a id="5.3"></a>
This module is the heart of the simulator. 
When the url path = /openc2 then the openc2_handler is used. 
Right now, only the verbose version of json is accepted. 
It may be that different url paths will be used for the 3 different versions (eg /openc2/verbose). 
It contains the following for it’s API:
- rest_init/2 - to tell cowboy this is a REST API
- allowed_methods/2 – to tell cowboy to only allow the POST method
- content_types_accepted/2 – to tell cowboy that only JSON is allowed, and to pass control to handle_json/2 when json is posted on this url 
- handle_json/2 (and it’s helper routines) :
  * verifies there is a body in the http request
  * decodes and verifies the json
  * spawns the necessary processes for action/target/actuator/modifier (more on this further down)
  * does the simulation
  * sends the appropriate http response 

The openC2 language is in JSON and consists of 
two mandatory top-level fields (action, target) 
and two optional top-level fields (actuator, modifiers). 
The nature of the openc2 command set poses some interesting challenges 
in how to organize the software 
(see add ref to MulitMode talk on 29-Sep-2016 at OpenC2 face2face). 
Ocas is organized taking advantage of erlang’s concurrency, small lightweight processes, and messaging. 
This allows the action/target/actuator/modifier code 
to remain independent of each other. 
For example, if a new actuator is added, it should not require changes to the actions software.

This is accomplished by spinning up an erlang process to handle orchestrating that particular command 
based on the action in that commmand.
For example
{action=deny, 
target=network connection, 
actuator=network firewall,
and modifier=acknowledge} will spin up a deny_server 
which will validate those other parameters 
(eg target=network connection is a valid target for deny command)
spinning up other processes as needed for validation (eg target process) and for simulation 
(the particular network connection instantiation).
These processes interact via messages to accomplish the desired simulation result.
This architecture allows separation of the functionality 
such that new targets can added without changing the action code.
Similarly for new actuators.
It also allows for the context and scope to be mimimized in the simulator to just what is needed.

Although this may be overkill in these first phases of the project 
(specification validation and one-time, single-command simulator); 
it allows the same code to scale up for the full network simulation 
including initializing the network to be simulated, playbook simulation, 
multiple concurrent orchestrators 
(eg looking for race conditions and time-dependent security vulnerabilities), 
and red-teaming via simulation.

The establishment of a separate erlang process for each entity 
and each action allows the simulator to scale both the scope (processes) 
and the context (messaging) of the network being simulated 
in addition to the modularizing the software that was mentioned earlier.

###5.4 Actions <a id="5.4"></a>
The module actions.erl contains spawn_action/3
which both verifies the request’s json action is valid, 
and it spawns the process for that action. The spawned process 
runs the function for that action in the action_servers directory 
(eg deny in the json action field spawns the process running the module act_deny.erl.
As of this report, skeleton code exists for some, but not all of the 35 actions defined in the openC2 specification.
It verifies a valid action,
but not target/actuator/modifiers that are semantically correct for that action. 
For each of the actions
the processes are actually spun up (albeit they only do a simple keepalive).

###5.5 More on software design <a id="5.5"></a>
See [README.md in apps/ocas/src](./apps/ocas/src/README.md) for more on the software design
including a sunny day walk thru the modules/functions.

##6. Development Status <a id="6"></a>
Development status will attempt to be explained using test status.
Tests that pass show functioning code.
Work to be done is shown thru tests that are incomplete, non-existent, or failing.
See [devstatus.md](./devstatus.md) for current status

##7. Specification Coverage <a id="7"></a>
Another way to consider status is how much of the specification is covered by what has so far been 
implemented. See [specstatus.md](specstatus.md) for current status of this.

##8. Examples <a id="8"></a>

put stuff here

##9. Want to help? <a id="9"></a>
Find a bug? or something missing? or something dumb? Want to complete more features? If yes to any of these then create an issue. Or ideally send a pull request with the changes you'd like.
