##
## Python libraries
##
from math import exp
from numpy import random
from random import shuffle

##
## Load our classes
##
from SPIR.Objects.Agent import Agent
from State import State
from Constants import Constant

class NewMicroMethod(object):
  ##
  ## Description: Constructor method
  ##
  ## @param profiles           Agent profile
  ## @param beta               Disease beta
  ## @param gamma              Disease gamma
  ## @param timesteps          Number of time steps to simulate
  ## @param prefixOutputFile   Prefix of file name of the agent characteristics and status
  ## @param outputSeparator    Separator character for the output file
  ##
  ## @return None
  ##
  def __init__(self, replica, profiles, beta, gamma, timesteps, prefixOutputFile, outputSeparator):
    self.numAgents = 0
    
    self.replica = replica
    self.profiles = profiles
    self.beta = 1 - exp(-beta)
    self.gamma = 1 - exp(-gamma)
    self.timesteps = timesteps
    self.prefixOutputFile = prefixOutputFile
    self.outputSeparator = outputSeparator
    
  ##
  ## Description: Get total number of agents simulated
  ##
  ## @param None
  ##
  ## @return None
  ##
  def getNumAgents(self):
    return self.numAgents
    
  ##
  ## Description: Execute the simulation
  ##
  ## @param None
  ##
  ## @return None
  ##
  def execute(self):
    ## Initialize agents
    nAgents = [0, 0, 0, 0]
    agents = []
    
    agentFile = self.prefixOutputFile + "-init.csv"
    sep = self.outputSeparator
    if(self.replica == 0):
      f = open(agentFile, "w")
      header = Constant.O_X + sep + Constant.O_ID + sep + Constant.O_STATE + sep + Constant.O_RHO + sep + Constant.O_KAPPA + sep + Constant.O_DELTA + sep + Constant.O_H + sep + Constant.O_UPS + sep + Constant.O_UPP + sep + Constant.O_UPI + sep + Constant.O_UPR + "\n"
      f.write(header)
    else:
      f = open(agentFile, "a")
    
    for profile in self.profiles:
      nagents = profile.getNumAgents()
      while (sum(nagents) > 0):
        d = random.dirichlet(nagents)
        t = random.uniform(0, 1)
        
        found = False
        i = 0
        s = 0
        while not found:
          s += d[i]
          if t <= s:
            found = True
          else:
            i += 1
        nagents[i] -= 1
        
        state = State.STATES[i]
        
        agent = Agent(self.numAgents, state, self.beta, self.gamma,
                      profile.getRho(), profile.getFear(), profile.getDecision(),
                      profile.getPlanningHorizon(), profile.getPayoffs())
        
        agents.append(agent)
        
        ## Record the agents characteristics
        line = str(self.replica) + sep + str(self.numAgents) + sep + str(state) + sep + str(profile.getRho()) + sep + str(profile.getFear()) + sep + str(profile.getDecision()) + sep + str(profile.getPlanningHorizon()) + sep + str(profile.getPayoffs()[0]) + sep + str(profile.getPayoffs()[1]) + sep + str(profile.getPayoffs()[2]) + sep + str(profile.getPayoffs()[3]) + "\n"
        
        print(self.numAgents)
        
        f.write(line)
        
        nAgents[state] += 1
        self.numAgents += 1
        
    f.close()
    
    ## Initialize output variables
    total = [0, 0, 0, 0]
    for agent in agents:
      state = agent.getState()
      total[state] += agent.getPayoffs()[state]
      
    num = []
    num.append([0,
                nAgents[State.S],
                nAgents[State.P],
                0,
                nAgents[State.I],
                0,
                0,
                nAgents[State.R],
                0,
                0,
                total[State.S],
                total[State.P],
                total[State.I],
                total[State.R]])
    
    ## Run the simulation
    t = 1
    i = nAgents[State.I] / float(self.numAgents)
    
    stateFile = self.prefixOutputFile + "-raw.csv"
    if(self.replica == 0):
      f = open(stateFile, "w")
      header = Constant.O_X + sep + Constant.O_T + sep + Constant.O_ID + sep + Constant.O_STATE + "\n"
      f.write(header)
    else:
      f = open(stateFile, "a")
    
    while ((t < self.timesteps) and (i > 0)):
      ## Shuffle the vector of agents
      shuffle(agents)
      
      ## Neighbor agents in the vector interact
      n = self.numAgents
      infected = []
      while(n > 1):
        a1 = agents[n - 1]
        a2 = agents[n - 2]
        
        a1State = a1.getState()
        a2State = a2.getState()
        
        if (a1State == State.I):
          infected.append(a1)
          a2.interact(a1State)
          
        if (a2State == State.I):
          infected.append(a2)
          a1.interact(a2State)
        
        n = n - 2
        
      ## Behavioral decision
      for agent in agents:
        if (random.uniform(0.0, 1.0) < agent.getDecision()):
          state = agent.getState()
          if (state == State.S) or (state == State.P):
            agent.decide(i)
          
      ## Recovery
      for agent in infected:
        agent.recover()
        
      ## Record output information
      numagents = [0, 0, 0, 0]
      total = [0, 0, 0, 0]
      for agent in agents:
        state = agent.getState()
        total[state] += agent.getPayoffs()[state]
        numagents[state] += 1
        
        line = str(self.replica) + sep + str(t) + sep + str(agent.getID()) + sep + str(state) + "\n"
        f.write(line)
      
      num.append([t,
                  numagents[State.S],
                  numagents[State.P],
                  0,
                  numagents[State.I],
                  0,
                  0,
                  numagents[State.R],
                  0,
                  0,
                  total[State.S],
                  total[State.P],
                  total[State.I],
                  total[State.R]])
      
      ## Recalculate the disease prevalence
      i = numagents[State.I] / float(self.numAgents)
      
      ## Advance time
      t += 1
    
    f.close()
    
    return num
