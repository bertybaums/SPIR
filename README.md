# SPIR (Susceptible-Prophylactic-Infected-Recovered) Agent-Based Simulation Model

## Software Pre-requisites
The SPIR agent-based model version is written in **Python 3**, thus it can be run on any Python-supported operating system. In addition to **Python 3.0+**, this implementation uses a set of existing standard libraries that needs to be installed prior to its execution:
+ argparse
+ matplotlib
+ os.path
+ sys
+ time
+ itemize

## Download Project from GitHub
* Open a terminal
* Navigate to the directory where you want to download the SPIR code
* Type: **git clone https://github.com/bertybaums/SPIR.git**

## Configuration
You can configure the SPIR simulations by changing the values of the parameters in the _config.txt_ file. The Table below describes the configuration parameters for the SPIR model.

| Parameter Name | Parameter Symbol | Description |
|----------------|------------------|-------------|
| num.agents.S   | -NS              | Initial number of agents in state Susceptible |
| num.agents.P   | -NP              | Initial number of agents in state Prophylactic |
| num.agents.I   | -NI              | Initial number of agents in state Infectious |
| num.agents.R   | -NR              | Initial number of agents in state Recovered |
| payoff.S       | -PS              | Payoff received per time step in state Susceptible |
| payoff.P       | -PP              | Payoff received per time step in state Prophylactic |
| payoff.I       | -PI              | Payoff received per time step in state Infectious |
| payoff.R       | -PR              | Payoff received per time step in state Recovered |
| beta           | -BS              | Probability that an agent in state Susceptible becomes infected upon interacting with an Infectious agent |
| rho            | 

## Usage
Syntax: **Main.py [-h] [-v] [-o] [-g] \{configFile, params\} ...**

where,
+ **-h**			    shows a SPIR model execution syntax
+ **-v**			    verbose
+ **-o**			    writes the output as Comma Separated Values (CSV) files
+ **-g**			    plots the output as a graphic
+ **configFile**	identify the configuration file that specifies the parameters value to run the simulation (see **Configuration**)
+ **params**		  explicit specification of all parameters in the command-line through the symbols (see **Configuration**)

To execute the SPIR model from Linux terminal:
* Navigate to the **SPIR** directory
* Type
		
    **python SPIR/Main.py -v -g -o configFile config.txt**
		
    or
		
    **python SPIR/Main.py -v -g -o params -NS 9900 -NP 0 -NI 100 -NR 0 -PS 1 -PP 0.95 -PI 0.1 -PR 0.95 -BS 0.0303 -RH 0.1 -G 0.0152 -K 1 -D 0.0099 -H 90 -M 2 -R 10 -T 2100 -F 1 -W 100 -P ".." -N "output" -O True -S ";"**

A graphic with the dynamics of agents in the Susceptible, Prophylactic, Infectious, Recovered state in a panel and the proportion of infected agents over time in another are shown. In addition to generate the plot, output files will be generated with the raw data of the simulation.

## Scripts
There are several R Statistics scripts available in the directory _scripts_. These scripts have different aims:

* **ode-sir-model.R** - Traditional SIR ODE model
* **ode-spir-model.R** - SPIR model as ODE
* **decision-landscape-analysis.R** - 3D visualization of the Susceptible and Prophylactic Expected Utility values in relation to the proportion of infected and time horizon for the decision-making
* **switching-point-analysis.R** - Calculus of the precise switching point location and a comparison with a possible linear approximation
* **disease-analysis.R** - Analysis of specific diseases
