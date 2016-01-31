##
## Python libraries
##
import argparse
from matplotlib import pyplot, gridspec
from os.path import isfile
from sys import exit
from time import clock

##
## Our classes
##
from Constants import Constant
from GillespieMethod import GillespieMethod
from MicroMethod import MicroMethod
from NaiveMethod import NaiveMethod
from State import State

if __name__ == '__main__':
    pass

parser = argparse.ArgumentParser(prog="Main.py")

parser.add_argument("-v", "--verbose", action="store_true")
parser.add_argument("-o", "--output", action="store_true")
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
configCmd.add_argument(Constant.M, nargs=1, required=True,
                       help="Simulation method")
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
method = 0
timeSteps = 1000
outputFile = "output.csv"
outputHeader = True
outputSep = ";"

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
        elif (param[0] == Constant.DECISION):
            decisionProb = float(param[1])
        elif (param[0] == Constant.TIME_HORIZON):
            timeHorizon = int(param[1])
        elif (param[0] == Constant.METHOD):
            method = int(param[1])
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
    method = int(args.M[0])
    timeSteps = int(args.T[0])
    outputFile = args.O[0]
    outputHeader = bool(args.P[0])
    outputSep = args.S[0]

##
## Initial time
##
start = clock()

N = 0
for t in nAgents:
    N += nAgents[t]

##
## Simulation method
##
if (method == Constant.METHOD_NAIVE):
    m = NaiveMethod(args, nAgents, payoffs, disease, decisionProb, timeHorizon, timeSteps)
elif (method == Constant.METHOD_GILLESPIE):
    m = GillespieMethod(args, nAgents, payoffs, disease, decisionProb, timeHorizon, timeSteps)
elif (method == Constant.METHOD_MICRO):
    m = MicroMethod(args, nAgents, payoffs, disease, decisionProb, timeHorizon, timeSteps)
    
num = m.execute()
        
##
## Output
##
if (args.output):
    f = open(outputFile, "w")

    if (outputHeader):
        header = Constant.O_T + outputSep + Constant.O_S + outputSep + Constant.O_P + outputSep + Constant.O_I + outputSep + Constant.O_R + "\n"
        f.write(header)
    
    for row in num:
        line = str(row[0]) + outputSep + str(row[1]) + outputSep + str(row[2]) + outputSep + str(row[3]) + outputSep + str(row[4]) + "\n"
        f.write(line)
    f.close()

##
## Execution time
##
end = clock()

if (args.verbose):
    print 'Processing Time:', str(end - start), 's'

if (args.graphic):
    ##
    ## Graphical visualization
    ##
    x = []
    s = []
    p = []
    i = []
    r = []
    f = []
    for row in num:
        x.append(row[0])
        s.append(row[1])
        p.append(row[2])
        i.append(row[3])
        r.append(row[4])
        f.append(row[3] / float(N))
    
    fig = pyplot.figure()
    
    gs = gridspec.GridSpec(2, 1)
    
    p1 = fig.add_subplot(gs[0, 0])
    lineS, = p1.plot(x, s, "k")
    lineP, = p1.plot(x, p, "b")
    lineI, = p1.plot(x, i, "r")
    lineR, = p1.plot(x, r, "g")
    pyplot.xlabel('Time')
    pyplot.ylabel('Number')
    p1.legend((lineS, lineP, lineI, lineR), ('S', 'P', 'I', 'R'), title='Legend')
    #p1.annotate('Area.S: ' + str(total[State.S]),
    #            xy=(int(timeSteps / float(2)), int(N / float(2))))
    #p1.annotate('Area.P: ' + str(total[State.P]),
    #            xy=(int(timeSteps / float(2)), int((N / float(2)) * 0.9)))
    #p1.annotate('Area.I: ' + str(total[State.I]),
    #            xy=(int(timeSteps / float(2)), int((N / float(2)) * 0.8)))
    
    ##
    ## Frequency
    ##
    p2 = fig.add_subplot(gs[1, 0])
    lineI, = p2.plot(x, f, "r")
    pyplot.xlabel('Time')
    pyplot.ylabel('Proportion infected (i)')
    
    pyplot.show()
