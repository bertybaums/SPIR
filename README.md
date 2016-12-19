# SPIR (Susceptible-Prophylactic-Infected-Recovered) Agent-Based Simulation Model

## Software Pre-requisites
The SPIR agent-based model version is written in **Python 3**, thus it can be run on any Python-supported operating system. In addition to **Python 3.0+**, this implementation uses a set of existing standard libraries that needs to be installed prior to its execution:
+ argparse
+ math
+ matplotlib
+ numpy
+ os.path
+ random
+ sys
+ time
+ xml.etree.ElementTree

## Download Project from GitHub
* Open a terminal
* Navigate to the directory where you want to download the SPIR code
* Type: **git clone https://github.com/bertybaums/SPIR.git**

## Configuration
You can configure the SPIR simulations by changing the values of the parameters in the _config.xml_ file. The Table below describes the configuration parameters for the SPIR model.

| Parameter Tag               | Parameter Symbol | Description |
|-----------------------------|------------------|-------------|
| <config>                    |                  | Start the configuration specification |
|   <agents>                  |                  | Specify the initial number of agents in each state |
|     <num.agents.S>          | -NS              | Initial number of agents in state Susceptible |
|     <num.agents.P>          | -NP              | Initial number of agents in state Prophylactic |
|     <num.agents.I>          | -NI              | Initial number of agents in state Infectious |
|     <num.agents.R>          | -NR              | Initial number of agents in state Recovered |
|   </agents>                 |                  | |
|   <profiles>                |                  | Set of agent profiles |
|     <profile>               |                  | Specify the profile of a subset of agents (Multiple possible) |
|       <num.agents>          |                  | Number of agents with this profile |
|       <payoff.S>            | -PS              | Payoff received per time step in state Susceptible |
|       <payoff.P>            | -PP              | Payoff received per time step in state Prophylactic |
|       <payoff.I>            | -PI              | Payoff received per time step in state Infectious |
|       <payoff.R>            | -PR              | Payoff received per time step in state Recovered |
|       <rho>                 | -RH              | Reduction in the transmission probability when adopting prophylactic behavior |
|       <kappa>               | -K               | Distortion of the perceived proportion of Infectious agents in the population (i.e. _distortion factor_) |
|       <decision.frequency>  | -D               | Probability an agent in the Susceptible or Prophylactic state decides which behavior to engage in |
|       <planning.horizon>    | -H               | The time in the future over which agents calculate their utilities to make a behavioral decision |
|     </profile>              |                  | |
|   </profiles>               |                  | |
|   <disease>                 |                  | Specify the disease information |
|     <beta>                  | -BS              | Probability that an agent in state Susceptible becomes infected upon interacting with an Infectious agent |
|     <gamma>                 | -G               | Probability an Infectious agent recover |
|   </disease>                |                  | |
|   <simulation>              |                  | Specify the simulation information |
|     <method>                | -M               | Method of executing the simulation (0 - Micro, 1 - Gillespie, 2 - Efficient Tau Leap) |
|     <replications>          | -R               | Number of replications to run the scenario using different random seeds |
|     <time.steps>            | -T               | Length of the simulation in steps |
|   </simulation>             |                  | |
|   <output>                  |                  | Specify the output information |
|     <output.format>         | -F               | Format of the output file (0 - Standard, 1 - Galapagos) |
|     <output.window>         | -W               | Size of the window to consolidate the output of several replications |
|     <output.path>           | -P               | Path of the output file |
|     <output.filename>       | -N               | Name of the output file without extension |
|     <output.header>         | -O               | Flag that indicates whether or not to write the output's columns header in the output file |
|     <output.separator>      | -S               | Character that separates the fields in the output file |
|   </output>                 |                  | |

## Usage
Syntax: **Main.py [-h] [-v] [-o] [-g] \{configFile, params\} ...**

where,
+ **-h**			    shows a SPIR model execution syntax
+ **-v**			    verbose
+ **-o**			    writes the output as Comma Separated Values (CSV) files
+ **-g**			    plots the output as a graphic
+ **configFile**	    identify the configuration file that specifies the parameters value to run the simulation (see **Configuration**)
+ **params**		  explicit specification of all parameters in the command-line through the symbols (see **Configuration**)

To execute the SPIR model from Linux terminal:
* Navigate to the **SPIR** directory
* Type
		
    **python SPIR/Main.py -v -g -o configFile config.xml**
		
    or
		
    **python SPIR/Main.py -v -g -o params -NS 9900 -NP 0 -NI 100 -NR 0 -PS 1 -PP 0.95 -PI 0.1 -PR 0.95 -BS 0.0303 -RH 0.1 -G 0.0152 -K 1 -D 0.0099 -H 90 -M 2 -R 10 -T 2100 -F 1 -W 100 -P ".." -N "output" -O True -S ";"**

A graphic with the dynamics of agents in the Susceptible, Prophylactic, Infectious, Recovered state in a panel and the proportion of infected agents over time in another are shown. In addition to generate the plot, output files will be generated with the raw data of the simulation.

## Scripts
There are several R Statistics scripts available in the directory _scripts_. These scripts have different aims:

| Script File Name              | Description |
|-------------------------------|-------------|
| calcExpectedTime.R            | Implements the _calc\_expectedTime(h, i, bs, rho, g, l, k, payoffs)_ function that calculates the expected times given a set of conditions. |
| calcSwitch.R                  | Implements the _calc\_iswitch(h, bs, rho, g, l, k, payoffs)_ function that calculates the switch points given a set of conditions. |
| calcUtilitties.R              | Implements the _calc\_utilities(h, bs, rho, g, l, k, payoffs)_ function that calculates the utilities given a set of conditions. |
| SPIRmodel.R                   | Implements the SPIR ODE model. |
| ode-sir-model.R               | Runs a ODE SIR model. |
| ode-spir-model.R              | Runs a ODE SPIR model. |
| disease1-parallel.R           | Generates the data of a hypothetical Disease 1. |
| disease2-parallel.R           | Generates the data of a hypothetical Disease 2. |
| general-plot.R                | Generates some general plots about Disease 1 and Disease 2. |
| disease1-plot.R               | Generates specific plots for Disease 1. |
| disease2-plot.R               | Generates specific plots for Disease 2. |

These scripts require **R v.3.3.1** with the following libraries: **colorRamps v.2.3**, **data.table v.1.9.6**, **deSolve 
v.1.13**, **doParallel v.1.0.10**, **ggplot2 v.2.1**, **grid v.3.3.1**, and **gridExtra v.2.2.1**, and **parallel v.3.3.1**.

To execute the scripts from Linux terminal:
*  Navigate to the _scripts_ directory
* Execute: **Rscript \<Script Filename\> --no-save**, where **\<Script Filename\>** is the script filename.
