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
from SPIR.Constants import Constant
from SPIR.GillespieMethod import GillespieMethod
from SPIR.MicroMethod import MicroMethod
from SPIR.NaiveMethod import NaiveMethod
from SPIR.State import State

if __name__ == '__main__':
    pass

##
## Command Line Argument Parsing
##
parser = argparse.ArgumentParser(prog="Main.py")

parser.add_argument("-v", "--verbose", action="store_true")
parser.add_argument("-o", "--output", action="store_true")
parser.add_argument("-g", "--graphic", action="store_true")

sb = parser.add_subparsers()

configFile = sb.add_parser("configFile")
configFile.add_argument(Constant.FILE,
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
                       help="Infection rate if Susceptible")
configCmd.add_argument(Constant.RH, nargs=1, required=True,
                       help="Prophylactic reduction on Susceptible infection rate")
configCmd.add_argument(Constant.G, nargs=1, required=True,
                       help="Probability of recovery")
configCmd.add_argument(Constant.K, nargs=1, required=True,
                       help="Fear effect")
configCmd.add_argument(Constant.D, nargs=1, required=True,
                       help="Decision rate")
configCmd.add_argument(Constant.H, nargs=1, required=True,
                       help="Time horizon")
configCmd.add_argument(Constant.M, nargs=1, required=True,
                       help="Simulation method")
configCmd.add_argument(Constant.R, nargs=1, required=True,
                       help="Replication")
configCmd.add_argument(Constant.T, nargs=1, required=True,
                       help="Time steps")
configCmd.add_argument(Constant.W, nargs=1, required=True,
                       help="Smooth window")
configCmd.add_argument(Constant.F, nargs=1, required=True,
                       help="Output format")
configCmd.add_argument(Constant.P, nargs=1, required=True,
                       help="Output file path")
configCmd.add_argument(Constant.N, nargs=1, required=True,
                       help="Output file name")
configCmd.add_argument(Constant.O, nargs=1, required=True,
                       help="Include header row in the output file")
configCmd.add_argument(Constant.S, nargs=1, required=True,
                       help="Separator character for the output file")

args = parser.parse_args()

##
## Input Variables - Default Values
##
nAgents = {State.S: 9999, State.P: 0, State.I: 1, State.R: 0}
payoffs = {State.S: 1, State.P: 0.95, State.I: 0.6, State.R: 1}
disease = {Constant.BETA: 0.031, Constant.RHO: 0.25, Constant.GAMMA: 0.015}
fear = 1.0
decision = 0.01
timeHorizon = 20
method = Constant.METHOD_GILLESPIE
replication = 1
timeSteps = 1000
window = 1
outputFormat = Constant.O_STANDARD
outputPath = ""
outputFilename = "output"
outputHeader = True
outputSep = ";"
    
if (args.__contains__(Constant.FILE)):
    
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
        elif (param[0] == Constant.BETA):
            disease[Constant.BETA] = float(param[1])
        elif (param[0] == Constant.RHO):
            disease[Constant.RHO] = float(param[1])
        elif (param[0] == Constant.GAMMA):
            disease[Constant.GAMMA] = float(param[1])
        elif (param[0] == Constant.FEAR):
            fear = float(param[1])
        elif (param[0] == Constant.DECISION):
            decision = float(param[1])
        elif (param[0] == Constant.TIME_HORIZON):
            timeHorizon = int(param[1])
        elif (param[0] == Constant.METHOD):
            method = int(param[1])
        elif (param[0] == Constant.REPLICATION):
            replication = int(param[1])
        elif (param[0] == Constant.TIME_STEPS):
            timeSteps = int(param[1])
        elif (param[0] == Constant.OUTPUT_FORMAT):
            outputFormat = int(param[1])
        elif (param[0] == Constant.OUTPUT_WINDOW):
            window = float(param[1])
        elif (param[0] == Constant.OUTPUT_PATH):
            outputPath = param[1]
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
    disease[Constant.BETA] = float(args.BS[0])
    disease[Constant.RHO] = float(args.RH[0])
    disease[Constant.GAMMA] = float(args.G[0])
    fear = float(args.K[0])
    decision = float(args.D[0])
    timeHorizon = int(args.H[0])
    method = int(args.M[0])
    replication = int(args.R[0])
    timeSteps = int(args.T[0])
    window = float(args.W[0])
    outputFormat = int(args.F[0])
    outputPath = args.P[0]
    outputFile = args.N[0]
    outputHeader = bool(args.O[0])
    outputSep = args.S[0]
    
##
## Initial time
##
if (args.verbose):
    start = clock()

##
## Total number of agents
##
N = 0
for i in nAgents:
    N += nAgents[i]

num = []
for rep in range(replication):
    if (args.verbose):
        print('Replication:', rep)
    
    agents = {State.S: nAgents[State.S], State.P: nAgents[State.P],
              State.I: nAgents[State.I], State.R: nAgents[State.R]}
    ##
    ## Simulation method
    ##
    if (method == Constant.METHOD_NAIVE):
        m = NaiveMethod(args, agents, payoffs, disease, fear, decision, timeHorizon, timeSteps * N)
    elif (method == Constant.METHOD_MICRO):
        m = MicroMethod(args, agents, payoffs, disease, fear, decision, timeHorizon, timeSteps * N)
    elif (method == Constant.METHOD_GILLESPIE):
        m = GillespieMethod(args, agents, payoffs, disease, fear, decision, timeHorizon, timeSteps * N)
    
    ##
    ## Execute simulation
    ##
    num.append(m.execute())

##
## Execution time
##
if (args.verbose):
    end = clock()
    print('Processing Time:', str(end - start), 's')
        
##
## Output
##
if (args.output):
    
    if (outputFormat == Constant.O_STANDARD):
        fname = outputPath + "/" + outputFile + ".csv"
        f = open(fname, "w")
        
        if (outputHeader):
            header = Constant.O_X + outputSep + Constant.O_T + outputSep + Constant.O_S + outputSep + Constant.O_P + outputSep + Constant.O_I + outputSep + Constant.O_R + "\n"
            f.write(header)
        
        for rep in range(replication):
            for row in num[rep]:
                line = str(rep) + outputSep + str(row[0]) + outputSep + str(row[1]) + outputSep + str(row[2]) + outputSep + str(row[4]) + outputSep + str(row[7]) + "\n"
                f.write(line)
        f.close()
        
    elif (outputFormat == Constant.O_GALAPAGOS):
        header = "simulator_time" + outputSep + "infection_state" + outputSep + "control_measure_status" + outputSep + "count" + "\n"
                
        for rep in range(replication):
            fname = outputPath + "/" + outputFile + str(rep) + ".csv"
            f = open(fname, "w")
            
            f.write(header)
            
            for row in num[rep]:
                timestep = str(row[0])
                line = timestep + outputSep + Constant.O_S + outputSep + "noControlMeasureAdopted" + outputSep + str(row[1]) + "\n"
                f.write(line)
                line = timestep + outputSep + Constant.O_S + outputSep + "controlMeasureAdoptedSuccessfully" + outputSep + str(row[2]) + "\n"
                f.write(line)
                line = timestep + outputSep + Constant.O_S + outputSep + "controlMeasureAdoptedUnsuccessfully" + outputSep + "0" + "\n"
                f.write(line)
                line = timestep + outputSep + Constant.O_I + outputSep + "noControlMeasureAdopted" + outputSep + str(row[4]) + "\n"
                f.write(line)
                line = timestep + outputSep + Constant.O_I + outputSep + "controlMeasureAdoptedSuccessfully" + outputSep + "0" + "\n"
                f.write(line)
                line = timestep + outputSep + Constant.O_I + outputSep + "controlMeasureAdoptedUnsuccessfully" + outputSep + "0" + "\n"
                f.write(line)
                line = timestep + outputSep + Constant.O_R + outputSep + "noControlMeasureAdopted" + outputSep + str(row[7]) + "\n"
                f.write(line)
                line = timestep + outputSep + Constant.O_R + outputSep + "controlMeasureAdoptedSuccessfully" + outputSep + "0" + "\n"
                f.write(line)
                line = timestep + outputSep + Constant.O_R + outputSep + "controlMeasureAdoptedUnsuccessfully" + outputSep + "0" + "\n"
                f.write(line)
            f.close()

##
## Graphical visualization
##
if (args.graphic):
    size = 0
    for vector in num:
        aux = vector[len(vector) - 1][0]
        if (aux > size):
            size = aux
    
    size = int((size / float(window)) + 1)
    
    x = [0 for y in range(size)]
    s = [0 for y in range(size)]
    p = [0 for y in range(size)]
    i = [0 for y in range(size)]
    r = [0 for y in range(size)]
    f = [0 for y in range(size)]
    for rep in range(replication):
        t = window
        pos = 0
        index = 0
        nSize = len(num[rep])
        pv = [nAgents[State.S] / float(N), nAgents[State.P] / float(N),
              nAgents[State.I] / float(N), nAgents[State.R] / float(N),
              nAgents[State.I] / float(N)]
        while (pos < size):
            v = [0, 0, 0, 0, 0]
            n = 0
            while ((index < nSize) and (num[rep][index][0] <= t)):
                v[0] += num[rep][index][1]
                v[1] += num[rep][index][2]
                v[2] += num[rep][index][4]
                v[3] += num[rep][index][7]
                v[4] += num[rep][index][4] / float(N)
                index += 1
                n += 1
            
            if (n > 0):
                pv[0] = v[0] / float(n)
                pv[1] = v[1] / float(n)
                pv[2] = v[2] / float(n)
                pv[3] = v[3] / float(n)
                pv[4] = v[4] / float(n)
                
            s[pos] += pv[0]
            p[pos] += pv[1]
            i[pos] += pv[2]
            r[pos] += pv[3]
            f[pos] += pv[4]
            
            pos += 1
            t += window
    
    for pos in range(size):
        x[pos] = (pos * window) / float(N)
        s[pos] /= replication
        p[pos] /= replication
        i[pos] /= replication
        r[pos] /= replication
        f[pos] /= replication
    
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
    
    ##
    ## Frequency
    ##
    p2 = fig.add_subplot(gs[1, 0])
    lineI, = p2.plot(x, f, "r")
    pyplot.xlabel('Time')
    pyplot.ylabel('Proportion infected (i)')
    
    pyplot.show()
