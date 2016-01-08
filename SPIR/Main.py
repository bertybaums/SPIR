##
## Python libraries
##
import argparse
from matplotlib import pyplot
from matplotlib import gridspec
from numpy import zeros
from os.path import isfile
from random import shuffle, uniform
from sys import exit
from time import clock

##
## Our classes
##
from Agent import Agent
from Constants import Constant
from State import State

if __name__ == '__main__':
    pass

parser = argparse.ArgumentParser(prog="Main.py")

parser.add_argument("-v", "--verbose", action="store_true")
parser.add_argument("-g", "--graphic", action="store_true")
parser.add_argument("-d", "--debug", action="store_true")

sb = parser.add_subparsers()

configFile = sb.add_parser("configFile")
configFile.add_argument(Constant.F,
                        help="Configuration file name")

configCmd = sb.add_parser("params")
configCmd.add_argument(Constant.NS, nargs=1, required=True,
                       help="Initial number of Susceptible")
configCmd.add_argument(Constant.NP, nargs=1, required=True,
                       help="Initial number of Prophylactic")
configCmd.add_argument(Constant.NI, nargs=1, required=True,
                       help="Initial number of Infected")
configCmd.add_argument(Constant.NR, nargs=1, required=True,
                       help="Initial number of Recovered")
configCmd.add_argument(Constant.PS, nargs=1, required=True,
                       help="Payoff Susceptible")
configCmd.add_argument(Constant.PP, nargs=1, required=True,
                       help="Payoff Prophylactic")
configCmd.add_argument(Constant.PI, nargs=1, required=True,
                       help="Payoff Infected")
configCmd.add_argument(Constant.PR, nargs=1, required=True,
                       help="Payoff Recovered")
configCmd.add_argument(Constant.BS, nargs=1, required=True,
                       help="Probability of infection if Susceptible")
configCmd.add_argument(Constant.BP, nargs=1, required=True,
                       help="Probability of infection if Prophylactic")
configCmd.add_argument(Constant.G, nargs=1, required=True,
                       help="Probability of recovery")
configCmd.add_argument(Constant.D, nargs=1, required=True,
                       help="Decision probability")
configCmd.add_argument(Constant.H, nargs=1, required=True,
                       help="Time horizon")
configCmd.add_argument(Constant.T, nargs=1, required=True,
                       help="Time steps")
configCmd.add_argument(Constant.O, nargs=1, required=True,
                       help="Output file name")
configCmd.add_argument(Constant.P, nargs=1, required=True,
                       help="Include header row in the output file")
configCmd.add_argument(Constant.S, nargs=1, required=True,
                       help="Separator character for the output file")

args = parser.parse_args()

##
## Input Variables
##
nAgents = {State.S: 9999, State.P: 0, State.I: 10, State.R: 0}
payoffs = {State.S: 1, State.P: 0.9, State.I: 0, State.R: 1}
disease = {Constant.BETA_S: 0.2, Constant.BETA_P: 0.01, Constant.GAMMA: 0.05}
decisionProb = 0.1
timeHorizon = 20
timeSteps = 1000
outputFile = "output.csv"
outputHeader = True
outputSep = ";"
outputScreen = False
outputGraphics = False

if (args.__contains__(Constant.F)):
    
    if (not isfile(str(args.filename))):
        print("Configuration file does not exist")
        exit()
        
    ##
    ## Read configuration file
    ##
    f = open(args.filename, 'r')
    for line in f.readlines():
        param = line.rstrip('\n').replace(' ', '').split('=')
       
        if (param[0] == Constant.NUM_AGENTS_S):
            nAgents[State.S] = int(param[1])
        elif (param[0] == Constant.NUM_AGENTS_P):
            nAgents[State.P] = int(param[1])
        elif (param[0] == Constant.NUM_AGENTS_I):
            nAgents[State.I] = int(param[1])
        elif (param[0] == Constant.NUM_AGENTS_R):
            nAgents[State.R] = int(param[1])
        elif (param[0] == Constant.PAYOFF_S):
            payoffs[State.S] = float(param[1])
        elif (param[0] == Constant.PAYOFF_P):
            payoffs[State.P] = float(param[1])
        elif (param[0] == Constant.PAYOFF_I):
            payoffs[State.I] = float(param[1])
        elif (param[0] == Constant.PAYOFF_R):
            payoffs[State.R] = float(param[1])
        elif (param[0] == Constant.BETA_S):
            disease[Constant.BETA_S] = float(param[1])
        elif (param[0] == Constant.BETA_P):
            disease[Constant.BETA_P] = float(param[1])
        elif (param[0] == Constant.GAMMA):
            disease[Constant.GAMMA] = float(param[1])
        elif (param[0] == Constant.DECISION_PROB):
            decisionProb = float(param[1])
        elif (param[0] == Constant.TIME_HORIZON):
            timeHorizon = int(param[1])
        elif (param[0] == Constant.TIME_STEPS):
            timeSteps = int(param[1])
        elif (param[0] == Constant.OUTPUT_FILE):
            outputFile = param[1]
        elif (param[0] == Constant.OUTPUT_HEADER):
            outputHeader = bool(param[1])
        elif (param[0] == Constant.OUTPUT_SEP):
            outputSep = param[1]
    f.close()
else:
    nAgents[State.S] = int(args.NS[0])
    nAgents[State.P] = int(args.NP[0])
    nAgents[State.I] = int(args.NI[0])
    nAgents[State.R] = int(args.NR[0])
    payoffs[State.S] = float(args.PS[0])
    payoffs[State.P] = float(args.PP[0])
    payoffs[State.I] = float(args.PI[0])
    payoffs[State.R] = float(args.PR[0])
    disease[Constant.BETA_S] = float(args.BS[0])
    disease[Constant.BETA_P] = float(args.BP[0])
    disease[Constant.GAMMA] = float(args.G[0])
    decisionProb = float(args.D[0])
    timeHorizon = int(args.H[0])
    timeSteps = int(args.T[0])
    outputFile = args.O[0]
    outputHeader = bool(args.P[0])
    outputSep = args.S[0]

##
## Initial time
##
start = clock()

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
    
    num[0, state] = nAgents[state]
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
i = num[t, State.I] / float(N)
while ((t < timeSteps) and (i > 0)):
    if (args.verbose):
        print 'Timestep:', str(t)
    
    ##
    ## Interaction
    ##
    shuffle(agents)
    
    n = N
    infected = []
    while(n > 1):
        a1 = agents[n - 1]
        a2 = agents[n - 2]
        
        a1State = a1.getState()
        a2State = a2.getState()
        
        if (a1State == State.I):
            infected.append(a1)
            
        if (a2State == State.I):
            infected.append(a2)
        
        a1S = a1.interact(a2State)
        a2S = a2.interact(a1State)
        
        num[t, a1S] = num[t, a1S] + 1
        num[t, a2S] = num[t, a2S] + 1
        
        n = n - 2
    
    ##
    ## Decision
    ##
    for agent in agents:
        if (((agent.getState() == State.S) or
             (agent.getState() == State.P)) and
            (uniform(0.0, 1.0) < decisionProb)):
            
            state = agent.getState()
            num[t, state] = num[t, state] - 1
            
            state = agent.decide(i)
            num[t, state] = num[t, state] + 1
    
    ##
    ## Recover
    ##
    for agent in infected:
        if (agent.recover()):          
            num[t, State.I] = num[t, State.I] - 1
            num[t, State.R] = num[t, State.R] + 1
    
    ##
    ## Updating output metric values
    ##
    total[State.S] = total[State.S] + num[t, State.S]
    total[State.P] = total[State.P] + num[t, State.P]
    total[State.I] = total[State.I] + num[t, State.I]
    total[State.R] = total[State.R] + num[t, State.R]
    
    i = num[t, State.I] / float(N)    
    t += 1
    
##
## Output
##
f = open(outputFile, "w")

if (outputHeader):
    header = Constant.O_T + outputSep + Constant.O_S + outputSep + Constant.O_P + outputSep + Constant.O_I + outputSep + Constant.O_R + "\n"
    f.write(header)
    
t = 0
while (t < timeSteps):
    line = str(t) + outputSep + str(num[t, State.S]) + outputSep + str(num[t, State.P]) + outputSep + str(num[t, State.I]) + outputSep + str(num[t, State.R]) + "\n"
    f.write(line)
    t += 1
f.close()

##
## Execution time
##
end = clock()

if (args.verbose):
    print('Processing Time:', str(end - start), 's')

if (args.graphic):
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
