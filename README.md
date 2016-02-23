# SPIR (Susceptible-Prophylactic-Infected-Recovered) Agent-Based Simulation Model

## Software Pre-requisites
* Python 3.0+
* Python Libraries: argparse, matplotlib, os.path, sys, time

## Download Project from GitHub
* Open a terminal
* Navigate to the directory where you want to download the SPIR code
* Type: git clone https://github.com/bertybaums/SPIR.git

## Configure
You can configure the simulation for running by changing the values of the parameters in the _config.txt_ file.

## Execute
To execute the simulation,
* Open a terminal
* Navigate to the SPIR root directory
* Type: python SPIR/Main.py -v -g configFile config.txt

You should see as a result a graphic with the dynamics of agents in the Susceptible, Prophylactic, Infectious, Recovered state.

## Scripts
There are several R Statistics scripts available in the directory _scripts_. These scripts have different aims:

* **ode-sir-model.R** - Traditional SIR ODE model
* **ode-spir-model.R** - SPIR model as ODE
* **decision-landscape-analysis.R** - 3D visualization of the Susceptible and Prophylactic Expected Utility values in relation to the proportion of infected and time horizon for the decision-making
* **switching-point-analysis.R** - Calculus of the precise switching point location and a comparison with a possible linear approximation
* **disease-analysis.R** - Analysis of specific diseases
