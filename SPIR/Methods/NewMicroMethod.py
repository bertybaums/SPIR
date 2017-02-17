##
## Python libraries
##
from copy import deepcopy
from math import exp
from numpy import random
from random import shuffle

##
## Load our classes
##
from Objects.Agent import Agent
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
    
    if(replica == 0):
      self.nAgents = [0, 0, 0, 0]
      self.agents = []
      
      agentFile = self.prefixOutputFile + "-init.csv"
      sep = self.outputSeparator
      f = open(agentFile, "w")
      header = Constant.O_ID + sep + Constant.O_STATE + sep + Constant.O_RHO + sep + Constant.O_KAPPA + sep + Constant.O_DELTA + sep + Constant.O_H + sep + Constant.O_UPS + sep + Constant.O_UPP + sep + Constant.O_UPI + sep + Constant.O_UPR + sep + Constant.O_SW + "\n"
      f.write(header)
      
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
          
          rho = profile.getRho()
          fear = profile.getFear()
          decision = profile.getDecision()
          planningHorizon = profile.getPlanningHorizon()
          payoffs = profile.getPayoffs()
          
          agent = Agent(self.numAgents, state, self.beta, self.gamma,
                        rho, fear, decision, planningHorizon, payoffs)
          
          switchingPoint = agent.getSwitchingPoint()
          
          self.agents.append(agent)
          
          ## Record the agents characteristics
          line = str(self.numAgents) + sep + str(state) + sep + str(rho) + sep + str(fear) + sep + str(decision) + sep + str(planningHorizon) + sep + str(payoffs[0]) + sep + str(payoffs[1]) + sep + str(payoffs[2]) + sep + str(payoffs[3]) + sep + str(switchingPoint[0][1]) + "\n"
          
          f.write(line)
          
          self.nAgents[state] += 1
          self.numAgents += 1
          
      f.close()
    
  ##
  ## Description: Gets the initial agents
  ##
  ## @param None
  ##
  ## @return Vector of initial agents configuration
  ##
  def getAgents(self):
    return(self.agents)
    
  ##
  ## Description: Set the agents
  ##
  ## @param agents Initial agents population
  ##
  ## @return None
  ##
  def setAgents(self, agents):
    self.agents = agents
    
    self.numAgents = 0
    self.nAgents = [0, 0, 0, 0]
    for agent in self.agents:
      self.nAgents[agent.getState()] += 1
      self.numAgents += 1
      
  ##
  ## Description: Execute the simulation
  ##
  ## @param None
  ##
  ## @return None
  ##
  def execute(self):
    ## Initialize output variables
    total = [0, 0, 0, 0]
    for agent in self.agents:
      state = agent.getState()
      total[state] += agent.getPayoffs()[state]
      
    num = []
    num.append([0,
                self.nAgents[State.S],
                self.nAgents[State.P],
                0,
                self.nAgents[State.I],
                0,
                0,
                self.nAgents[State.R],
                0,
                0,
                total[State.S],
                total[State.P],
                total[State.I],
                total[State.R]])
    
    ## Run the simulation
    t = 1
    i = self.nAgents[State.I] / float(self.numAgents)
    
    stateFile = self.prefixOutputFile + "-raw.csv"
    sep = self.outputSeparator
    if(self.replica == 0):
      f = open(stateFile, "w")
      header = Constant.O_X + sep + Constant.O_T + sep + Constant.O_ID + sep + Constant.O_STATE + "\n"
      f.write(header)
    else:
      f = open(stateFile, "a")
    
    while ((t < self.timesteps) and (i > 0)):
      ## Shuffle the vector of agents
      shuffle(self.agents)
      
      ## Neighbor agents in the vector interact
      n = self.numAgents
      infected = []
      while(n > 1):
        a1 = self.agents[n - 1]
        a2 = self.agents[n - 2]
        
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
      for agent in self.agents:
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
      for agent in self.agents:
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
