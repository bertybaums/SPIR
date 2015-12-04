##
## Python libraries
##
import matplotlib.pyplot as plt
import numpy as np
from random import randint, shuffle, uniform
from time import process_time

##
## User libraries
##
from SPIR.Agent import Agent
from SPIR.State import State

if __name__ == '__main__':
    pass

##
## Input Variables
##
nAgents = {State.S: 990, State.P: 0, State.I: 10, State.R: 0}
payoffs = {State.S: 7, State.P: 4, State.I: 0, State.R: 4}
disease = {'b1': 0.2, 'b2': 0.01, 'g': 0.05}

T = 500
H = 100
D = 0.5
N = 0

##
## Output variables
##
outputs = np.zeros((T,4), float)
accS = 0
accP = 0
accI = 0
accR = 0

##
## Initialize agents
##
agents = {State.S: [], State.P: [], State.I: [], State.R: []}
for state in nAgents:
    for index in range(0, nAgents[state]):
        agents[state].append(Agent(N, state, disease))
        N += 1

##
## Run the simulation
##
t = 0
i = len(agents[State.I]) / N
while ((t < T) and (i > 0)):
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
        if (uniform(0.0,1.0) < D):
            before = agent.getState()
            after = agent.decide(payoffs, i, H)
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
    i = len(agents[State.I]) / N

##
## Graphical visualization
##
x = range(T)

s = tuple(y[0] for y in outputs)
p = tuple(y[1] for y in outputs)
i = tuple(y[2] for y in outputs)
r = tuple(y[3] for y in outputs)

lineS, = plt.plot(x, s, "r")
lineP, = plt.plot(x, p, "b")
lineI, = plt.plot(x, i, "y")
lineR, = plt.plot(x, r, "g")
plt.legend((lineS, lineP, lineI, lineR), ('S', 'P', 'I', 'R'),
           title='Legend')
plt.annotate('Time..: ' + str(process_time()), xy=(300,600))
plt.annotate('Area.S: ' + str(accS), xy=(300,550))
plt.annotate('Area.P: ' + str(accP), xy=(300,500))
plt.annotate('Area.I: ' + str(accI), xy=(300,450))
plt.show()
