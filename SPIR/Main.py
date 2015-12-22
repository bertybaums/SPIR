##
## Python libraries
##
from matplotlib import pyplot
from matplotlib import gridspec
from numpy import zeros
from os.path import isfile
from random import shuffle, uniform
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
payoffs = {State.S: 1, State.P: 0.99, State.I: 0, State.R: 0.95}
disease = {'bs': 0.2, 'bp': 0.01, 'g': 0.05}
decisionProb = 0.1
timeHorizon = 100
timeSteps = 1000
        
##
## Read configuration file
##
if (not isfile(argv[1])):
    print("Configuration file does not exist")
    exit()
    
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
num = zeros((timeSteps, 4), int)
total = zeros(4, int)

##
## Initialize agents
##
N = 0
agents = []
infected = []
for state in nAgents:
    
    num[0,state] = nAgents[state]
    total[state] = nAgents[state]
    
    for i in range(nAgents[state]):
        agent = Agent(N, state, disease, timeHorizon, payoffs)
        agents.append(agent)
        N += 1
        
        if (state == State.I):
            infected.append(agent)

##
## Run the simulation
##
t = 0
i = num[t,State.I] / float(N)
while ((t < timeSteps) and (i > 0)):
    print 'Timestep:', str(t)
    
    ##
    ## Interaction
    ##
    shuffle(agents)
    
    n = N
    cInfected = []
    while(n > 1):
        a1 = agents[n-1]
        a2 = agents[n-2]
        
        a1State = a1.getState()
        a2State = a2.getState()
        
        a1S = a1.interact(a2State)
        a2S = a2.interact(a1State)
        
        if (a1S == State.I):
            cInfected.append(a1)
            
        if (a2S == State.I):
            cInfected.append(a2)
        
        num[t,a1S] = num[t,a1S] + 1
        num[t,a2S] = num[t,a2S] + 1
        
        n = n - 2
    
    ##
    ## Decision
    ##
    for agent in agents:
        if (((agent.getState() == State.S) or
             (agent.getState() == State.P)) and
            (uniform(0.0, 1.0) < decisionProb)):
            
            state = agent.getState()
            num[t,state] = num[t,state] - 1
            
            state = agent.decide(i)
            num[t,state] = num[t,state] + 1
    
    ##
    ## Recover
    ##
    for agent in infected:
        if (agent.recover()):
            index = cInfected.index(agent)
            cInfected.pop(index)
            
            num[t,State.I] = num[t,State.I] - 1
            num[t,State.R] = num[t,State.R] + 1
            
    infected = cInfected
    
    ##
    ## Updating output metric values
    ##
    total[State.S] = total[State.S] + num[t,State.S]
    total[State.P] = total[State.P] + num[t,State.P]
    total[State.I] = total[State.I] + num[t,State.I]
    total[State.R] = total[State.R] + num[t,State.R]
    
    i = num[t,State.I] / float(N)    
    t += 1
    

##
## Execution time
##
end = clock()
print('Processing Time:', str(end - start), 's')

##
## Graphical visualization
##
x = range(timeSteps)

##
## Absolute numbers
##
s = tuple(y[State.S] for y in num)
p = tuple(y[State.P] for y in num)
i = tuple(y[State.I] for y in num)
r = tuple(y[State.R] for y in num)

fig = pyplot.figure()

gs = gridspec.GridSpec(2, 1)

p1 = fig.add_subplot(gs[0, 0])
lineS, = p1.plot(x, s, "k")
lineP, = p1.plot(x, p, "b")
lineI, = p1.plot(x, i, "r")
lineR, = p1.plot(x, r, "g")
pyplot.xlabel('Time')
pyplot.ylabel('Number')
p1.legend((lineS, lineP, lineI, lineR), ('S', 'P', 'I', 'R'),
          title='Legend')
p1.annotate('Area.S: ' + str(total[State.S]),
            xy=(int(timeSteps / float(2)), int(N / float(2))))
p1.annotate('Area.P: ' + str(total[State.P]),
            xy=(int(timeSteps / float(2)), int((N / float(2)) * 0.9)))
p1.annotate('Area.I: ' + str(total[State.I]),
            xy=(int(timeSteps / float(2)), int((N / float(2)) * 0.8)))

##
## Frequency
##
p2 = fig.add_subplot(gs[1, 0])
i = tuple(y[State.I] / float(N) for y in num)
lineI, = p2.plot(x, i, "r")
pyplot.xlabel('Time')
pyplot.ylabel('Proportion infected (i)')

pyplot.show()
