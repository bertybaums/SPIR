##
## Python libraries
##
from matplotlib import pyplot as plt
import numpy as np
from os.path import isfile
from random import randint, shuffle, uniform
from sys import argv, exit
from time import clock

##
## Our classes
##
from Agent import Agent
from State import State

if __name__ == '__main__':
    pass
    

if (len(argv) < 2):
    print("usage: Main.py [config file]")
    exit()

##
## Initial time
##
start = clock()

##
## Constants
##
NUM_AGENTS_S = "num.agents.S"
NUM_AGENTS_P = "num.agents.P"
NUM_AGENTS_I = "num.agents.I"
NUM_AGENTS_R = "num.agents.R"
PAYOFF_S = "payoff.S"
PAYOFF_P = "payoff.P"
PAYOFF_I = "payoff.I"
PAYOFF_R = "payoff.R"
BETA_S = "beta.S"
BETA_P = "beta.P"
GAMMA = "gamma"
DECISION_PROB = "decision.prob"
TIME_HORIZON = "time.horizon"
TIME_STEPS = "time.steps"

##
## Input Variables
##
nAgents = {State.S: 990, State.P: 0, State.I: 10, State.R: 0}
payoffs = {State.S: 7, State.P: 5, State.I: 0, State.R: 4}
disease = {'bs': 0.2, 'bp': 0.01, 'g': 0.05}

decisionProb = 0.1
timeHorizon = 100
timeSteps = 500
numAgents = 0
        
##
## Read configuration file
##
if (not isfile(argv[1])):
    print("Configuration file does not exist")
    
f = open(str(argv[1]), 'r')
for line in f.readlines():
    param = line.replace(' ', '').split('=')
   
    if (param[0] == NUM_AGENTS_S):
        nAgents[State.S] = int(param[1])
    elif (param[0] == NUM_AGENTS_P):
        nAgents[State.P] = int(param[1])
    elif (param[0] == NUM_AGENTS_I):
        nAgents[State.I] = int(param[1])
    elif (param[0] == NUM_AGENTS_R):
        nAgents[State.R] = int(param[1])
    elif (param[0] == PAYOFF_S):
        payoffs[State.S] = float(param[1])
    elif (param[0] == PAYOFF_P):
        payoffs[State.P] = float(param[1])
    elif (param[0] == PAYOFF_I):
        payoffs[State.I] = float(param[1])
    elif (param[0] == PAYOFF_R):
        payoffs[State.R] = float(param[1])
    elif (param[0] == BETA_S):
        disease['bs'] = float(param[1])
    elif (param[0] == BETA_P):
        disease['bp'] = float(param[1])
    elif (param[0] == GAMMA):
        disease['g'] = float(param[1])
    elif (param[0] == DECISION_PROB):
        decisionProb = float(param[1])
    elif (param[0] == TIME_HORIZON):
        timeHorizon = int(param[1])
    elif (param[0] == TIME_STEPS):
        timeSteps = int(param[1])

##
## Output variables
##
outputs = np.zeros((timeSteps,4), float)
accS = 0
accP = 0
accI = 0
accR = 0

##
## Initialize agents
##    
agents = {State.S: [], State.P: [], State.I: [], State.R: []}
for state in nAgents:
    for index in range(nAgents[state]):
        agents[state].append(Agent(numAgents, state, disease))
        numAgents += 1

##
## Run the simulation
##
t = 0
i = len(agents[State.I]) / float(numAgents)
while ((t < timeSteps) and (i > 0)):
    ##
    ## Recording output metric values
    ##
    outputs[t,0] = len(agents[State.S])
    outputs[t,1] = len(agents[State.P])
    outputs[t,2] = len(agents[State.I])
    outputs[t,3] = len(agents[State.R])
    accS = accS + len(agents[State.S])
    accP = accP + len(agents[State.P])
    accI = accI + len(agents[State.I])
    accR = accR + len(agents[State.R])
    
    allAgents = agents[State.S] + agents[State.P] + agents[State.I] + agents[State.R]
    shuffle(allAgents)
    
    infected = agents[State.I]
    
    ##
    ## Interaction
    ##
    agents = {State.S: [], State.P: [], State.I: [], State.R: []}
    while(len(allAgents) > 1):
        a1 = allAgents.pop(randint(0,len(allAgents)-1))
        a2 = allAgents.pop(randint(0,len(allAgents)-1))
        
        a1.interact(a2)
        a2.interact(a1)
        
        agents[a1.getState()].append(a1)
        agents[a2.getState()].append(a2)
    
    ##
    ## Decision
    ##
    for agent in (agents[State.S] + agents[State.P]):
        if (uniform(0.0,1.0) < decisionProb):
            before = agent.getState()
            agent.decide(payoffs, i, timeHorizon)
            after = agent.decide(payoffs, i, timeHorizon)
            if (before != after):
                index = agents[before].index(agent)
                agents[before].pop(index)
                agents[after].append(agent)
    
    ##
    ## Recover
    ##
    recovered = []
    for agent in infected:
        state = agent.recover()
        if (state == State.R):
            recovered.append(agent)
            
    for agent in list(set(agents[State.I]) & set(recovered)):
        index = agents[State.I].index(agent)
        agents[State.I].pop(index)
        agents[State.R].append(agent)
    
    t += 1
    i = len(agents[State.I]) / float(numAgents)

##
## Execution time
##
end = clock()
print(end - start)

##
## Graphical visualization
##
x = range(timeSteps)

s = tuple(y[0] for y in outputs)
p = tuple(y[1] for y in outputs)
i = tuple(y[2] for y in outputs)
r = tuple(y[3] for y in outputs)

fig = plt.figure()
fig.add_subplot(11)

lineS, = fig.plot(x, s, "r")
lineP, = fig.plot(x, p, "b")
lineI, = fig.plot(x, i, "y")
lineR, = fig.plot(x, r, "g")
plt.legend((lineS, lineP, lineI, lineR), ('S', 'P', 'I', 'R'),
           title='Legend')
plt.annotate('Area.S: ' + str(accS), xy=(int(timeSteps / float(2)),
                                         int(numAgents / float(2))))
plt.annotate('Area.P: ' + str(accP), xy=(int(timeSteps / float(2)),
                                         int((numAgents / float(2)) * 0.9)))
plt.annotate('Area.I: ' + str(accI), xy=(int(timeSteps / float(2)),
                                         int((numAgents / float(2)) * 0.8)))
plt.show()

i = tuple(y[2] / float(numAgents) for y in outputs)
lineI, = plt.plot(x, i, "y")
plt.show()
