# # Python libraries
import argparse
from matplotlib import pyplot, gridspec
from os.path import isfile
from sys import exit
from time import clock

# # Load our classes
from Constants import Constant
from Methods.EfficientTauLeapMethod import EfficientTauLeapMethod
from Methods.GillespieMethod import GillespieMethod
from Methods.MicroMethod import MicroMethod
from Methods.NewMicroMethod import NewMicroMethod
from Objects.Config import Config
from State import State

if __name__ == '__main__':
    pass

# # Command Line Argument Parsing
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
                       help="Planning horizon")
configCmd.add_argument(Constant.M, nargs=1, required=True,
                       help="Simulation method")
configCmd.add_argument(Constant.R, nargs=1, required=True,
                       help="Replications")
configCmd.add_argument(Constant.T, nargs=1, required=True,
                       help="Time steps")
configCmd.add_argument(Constant.W, nargs=1, required=True,
                       help="Smooth output window")
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

# # Default values of the input parameters
types = 1
nAgents = {State.S: 0, State.P: 0, State.I: 0, State.R: 0}
payoffs = {State.S: 1, State.P: 0.95, State.I: 0.6, State.R: 1}
beta = 0.031
gamma = 0.015
rho = 0.25
fear = 1.0
decision = 0.01
planningHorizon = 20
method = Constant.METHOD_GILLESPIE
replications = 1
timesteps = 1000
outputWindow = 1
outputFormat = Constant.O_STANDARD
outputPath = ""
outputFilename = "output"
outputHeader = True
outputSeparator = ";"

if (args.__contains__(Constant.FILE)):
  
  # # Check if the configuration file exists
  if (not isfile(str(args.filename))):
    print("Configuration file does not exist")
    exit()
    
  # # Load input parameter values from configuration file
  config = Config(args.filename)
  
  beta = config.getBeta()
  gamma = config.getGamma()
  
  method = config.getMethod()
  replications = config.getReplications()
  timesteps = config.getTimesteps()
  
  outputWindow = config.getOutputWindow()
  outputFormat = config.getOutputFormat()
  outputPath = config.getOutputPath()
  outputFilename = config.getOutputFilename()
  outputHeader = config.getOutputHeader()
  outputSeparator = config.getOutputSeparator()
  
  profiles = config.getProfiles()
  if (method == Constant.METHOD_GILLESPIE) or (method == Constant.METHOD_EFFICIENT_TAU_LEAP) or (method == Constant.METHOD_MICRO):
    profile = profiles[0]
    
    nAgents[State.S] = profile.getNumAgentsS()
    nAgents[State.P] = profile.getNumAgentsP()
    nAgents[State.I] = profile.getNumAgentsI()
    nAgents[State.R] = profile.getNumAgentsR()
    
    payoffs[State.S] = profile.getPayoffs()[0]
    payoffs[State.P] = profile.getPayoffs()[1]
    payoffs[State.I] = profile.getPayoffs()[2]
    payoffs[State.R] = profile.getPayoffs()[3]
    rho = profile.getRho()
    fear = profile.getFear()
    decision = profile.getDecision()
    planningHorizon = profile.getPlanningHorizon()
    
    method = config.getMethod()
    replications = config.getReplications()
    timesteps = config.getTimesteps()
    
    outputWindow = config.getOutputWindow()
    outputFormat = config.getOutputFormat()
    outputPath = config.getOutputPath()
    outputFile = config.getOutputFilename()
    outputHeader = config.getOutputHeader()
    outputSeparator = config.getOutputSeparator()
else:
  # # Set input parameter from command line arguments
  types = 1
  nAgents[State.S] = int(args.NS[0])
  nAgents[State.P] = int(args.NP[0])
  nAgents[State.I] = int(args.NI[0])
  nAgents[State.R] = int(args.NR[0])
  payoffs[State.S] = float(args.PS[0])
  payoffs[State.P] = float(args.PP[0])
  payoffs[State.I] = float(args.PI[0])
  payoffs[State.R] = float(args.PR[0])
  beta = float(args.BS[0])
  gamma = float(args.G[0])
  rho = float(args.RH[0])
  fear = float(args.K[0])
  decision = float(args.D[0])
  planningHorizon = int(args.H[0])
  method = int(args.M[0])
  replications = int(args.R[0])
  timesteps = int(args.T[0])
  outputWindow = float(args.W[0])
  outputFormat = int(args.F[0])
  outputPath = args.P[0]
  outputFilename = args.N[0]
  outputHeader = bool(args.O[0])
  outputSeparator = args.S[0]
  
# # Set the simulation start time
if (args.verbose):
  start = clock()

# # Execute the simulation
num = []
for rep in range(replications):
  # # Print the replication on the screen
  if (args.verbose):
    print('Replication:', rep)
  
  agents = {State.S: nAgents[State.S], State.P: nAgents[State.P],
            State.I: nAgents[State.I], State.R: nAgents[State.R]}
  
  # # Initialize the simulation
  if method == Constant.METHOD_MICRO:
    m = MicroMethod(nAgents, payoffs, beta, gamma, rho, fear, decision, planningHorizon, timesteps)
  elif method == Constant.METHOD_GILLESPIE:
    m = GillespieMethod(nAgents, payoffs, beta, gamma, rho, fear, decision, planningHorizon, timesteps)
  elif method == Constant.METHOD_EFFICIENT_TAU_LEAP:
    m = EfficientTauLeapMethod(nAgents, payoffs, beta, gamma, rho, fear, decision, planningHorizon, timesteps)
  elif method == Constant.METHOD_HETEROGENEOUS_MICRO:
    m = NewMicroMethod(rep, profiles, beta, gamma, timesteps, outputPath + "/" + outputFilename, outputSeparator)
  
  # # Execute the simulation
  num.append(m.execute())
  
  N = m.getNumAgents()
  
# # Print the simulation execution time
if (args.verbose):
  end = clock()
  print('Processing Time:', str(end - start), 's')
  
  
# # Output
if (args.output):
  
  # # Write raw data to the output file (STANDARD format)
  if (outputFormat == Constant.O_STANDARD):
    fname = outputPath + "/" + outputFilename + ".csv"
    f = open(fname, "w")
    
    if (outputHeader):
      header = Constant.O_X + outputSeparator + Constant.O_T + outputSeparator + Constant.O_S + outputSeparator + Constant.O_P + outputSeparator + Constant.O_I + outputSeparator + Constant.O_R + outputSeparator + Constant.O_PS + outputSeparator + Constant.O_PP + outputSeparator + Constant.O_PI + outputSeparator + Constant.O_PR + "\n"
      f.write(str(header))
    
    for rep in range(replications):
      for row in num[rep]:
        line = str(rep) + outputSeparator + str(row[0]) + outputSeparator + str(row[1]) + outputSeparator + str(row[2]) + outputSeparator + str(row[4]) + outputSeparator + str(row[7]) + outputSeparator + str(row[10]) + outputSeparator + str(row[11]) + outputSeparator + str(row[12]) + outputSeparator + str(row[13]) + "\n"
        f.write(line)
    f.close()
  
  # # Write summarized data to the output file (GALAPAGOS format)
  elif (outputFormat == Constant.O_GALAPAGOS):
    header = "simulator_time" + outputSeparator + "infection_state" + outputSeparator + "control_measure_status" + outputSeparator + "count" + "\n"
    
    size = 0
    for vector in num:
      aux = vector[len(vector) - 1][0]
      if (aux > size):
        size = aux
        
    size = int((size / float(outputWindow)) + 1)
    
    x = [0 for y in range(size)]
    s = [0 for y in range(size)]
    p = [0 for y in range(size)]
    i = [0 for y in range(size)]
    r = [0 for y in range(size)]
    for rep in range(replications):
      t = outputWindow
      pos = 0
      index = 0
      nSize = len(num[rep])
      pv = [nAgents[State.S] / float(N), nAgents[State.P] / float(N),
            nAgents[State.I] / float(N), nAgents[State.R] / float(N)]
      while (pos < size):
        v = [0, 0, 0, 0]
        n = 0
        while ((index < nSize) and (num[rep][index][0] <= t)):
          v[0] += num[rep][index][1]
          v[1] += num[rep][index][2]
          v[2] += num[rep][index][4]
          v[3] += num[rep][index][7]
          index += 1
          n += 1
        
        if (n > 0):
          pv[0] = v[0] / float(n)
          pv[1] = v[1] / float(n)
          pv[2] = v[2] / float(n)
          pv[3] = v[3] / float(n)
            
        s[pos] = pv[0]
        p[pos] = pv[1]
        i[pos] = pv[2]
        r[pos] = pv[3]
        
        pos += 1
        t += outputWindow
          
      fname = outputPath + "/" + outputFilename + str(rep) + ".csv"
      f = open(fname, "w")
      
      f.write(str(header))
      
      for index in range(0, pos):
        timestep = str(index + 1)
        line = timestep + outputSeparator + Constant.O_S + outputSeparator + "noControlMeasureAdopted" + outputSeparator + str(s[index]) + "\n"
        f.write(line)
        line = timestep + outputSeparator + Constant.O_S + outputSeparator + "controlMeasureAdoptedSuccessfully" + outputSeparator + str(p[index]) + "\n"
        f.write(line)
        line = timestep + outputSeparator + Constant.O_S + outputSeparator + "controlMeasureAdoptedUnsuccessfully" + outputSeparator + "0" + "\n"
        f.write(line)
        line = timestep + outputSeparator + Constant.O_I + outputSeparator + "noControlMeasureAdopted" + outputSeparator + str(i[index]) + "\n"
        f.write(line)
        line = timestep + outputSeparator + Constant.O_I + outputSeparator + "controlMeasureAdoptedSuccessfully" + outputSeparator + "0" + "\n"
        f.write(line)
        line = timestep + outputSeparator + Constant.O_I + outputSeparator + "controlMeasureAdoptedUnsuccessfully" + outputSeparator + "0" + "\n"
        f.write(line)
        line = timestep + outputSeparator + Constant.O_R + outputSeparator + "noControlMeasureAdopted" + outputSeparator + str(r[index]) + "\n"
        f.write(line)
        line = timestep + outputSeparator + Constant.O_R + outputSeparator + "controlMeasureAdoptedSuccessfully" + outputSeparator + "0" + "\n"
        f.write(line)
        line = timestep + outputSeparator + Constant.O_R + outputSeparator + "controlMeasureAdoptedUnsuccessfully" + outputSeparator + "0" + "\n"
        f.write(line)
      f.close()
      
      
# #
# # Generate graphical visualization
# #
if (args.graphic):
  size = 0
  for vector in num:
    aux = vector[len(vector) - 1][0]
    if (aux > size):
      size = aux
  
  size = int((size / float(outputWindow)) + 1)
  
  x = [0 for y in range(size)]
  s = [0 for y in range(size)]
  p = [0 for y in range(size)]
  i = [0 for y in range(size)]
  r = [0 for y in range(size)]
  f = [0 for y in range(size)]
  for rep in range(replications):
    t = outputWindow
    pos = 0
    index = 0
    nSize = len(num[rep])
    pv = [nAgents[State.S] / float(N),
          nAgents[State.P] / float(N),
          nAgents[State.I] / float(N),
          nAgents[State.R] / float(N),
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
      
      if(method == Constant.METHOD_MICRO):
        x[pos] = (pos * outputWindow)
      elif(method == Constant.METHOD_GILLESPIE):
        x[pos] = (pos * outputWindow) / float(N)
      elif(method == Constant.METHOD_EFFICIENT_TAU_LEAP):
        x[pos] = (pos * outputWindow) / float(N)
      elif(method == Constant.METHOD_HETEROGENEOUS_MICRO):
        x[pos] = (pos * outputWindow)
        
      s[pos] += (pv[0] / replications)
      p[pos] += (pv[1] / replications)
      i[pos] += (pv[2] / replications)
      r[pos] += (pv[3] / replications)
      f[pos] += (pv[4] / replications)
      
      pos += 1
      t += outputWindow
  
  fig = pyplot.figure()
  gs = gridspec.GridSpec(2, 1)
  
  # # Plot the number of agents in each state over time
  p1 = fig.add_subplot(gs[0, 0])
  lineS, = p1.plot(x, s, "k")
  lineP, = p1.plot(x, p, "b")
  lineI, = p1.plot(x, i, "r")
  lineR, = p1.plot(x, r, "g")
  pyplot.xlabel('Time')
  pyplot.ylabel('Number')
  p1.legend((lineS, lineP, lineI, lineR), ('S', 'P', 'I', 'R'), title='Legend')
  
  # # Plot the disease prevalence over time
  p2 = fig.add_subplot(gs[1, 0])
  lineI, = p2.plot(x, f, "r")
  pyplot.xlabel('Time')
  pyplot.ylabel('Proportion infected (i)')
  
  pyplot.show()
