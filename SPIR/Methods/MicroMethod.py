##
## Python libraries
##
from math import exp
from numpy import random
from random import shuffle

##
## Load our classes
##
from Objects.Agent import Agent
from State import State
from Utils.Util import Util

class MicroMethod(object):
  ##
  ## Description: Constructor method
  ##
  ## @param nAgents          Number of agents
  ## @param payoffs          Payoff of each disease state
  ## @param beta             Disease beta
  ## @param gamma            Disease gamma
  ## @param rho              1 - Prophylactic protection
  ## @param fear             Distortion of disease prevalence
  ## @param decision         Decision frequency
  ## @param planningHorizon  Planning horizon
  ## @param timesteps        Number of time steps to simulate
  ##
  ## @return None
  ##
  def __init__(self, nAgents, payoffs, beta, gamma, rho, fear, decision, planningHorizon, timesteps):
    self.nAgents = nAgents
    self.numAgents = 0
    for i in self.nAgents:
      self.numAgents += self.nAgents[i]
      
    self.payoffs = payoffs
    
    self.beta = 1 - exp(-beta)
    self.gamma = 1 - exp(-gamma)
    
    self.rho = 1 - exp(-rho)
    self.fear = fear
    if (self.fear == 0):
        self.fear = 1.0
    
    self.decision = 1 - exp(-decision)
    self.planningHorizon = planningHorizon
    self.timesteps = timesteps
    
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
    ## Calculate switching points
    switchPoint = Util.calcISwitch(self.planningHorizon, self.beta, self.gamma, self.rho, self.payoffs)
    
    ## Initialize agents
    N = 0
    agents = []
    for state in self.nAgents:
      for x in range(self.nAgents[state]):
        agent = Agent(N, state, self.beta, self.gamma, self.rho, self.fear, self.decision, self.planningHorizon, self.payoffs, switchPoint)
        agents.append(agent)
          
        N += 1
    
    ## Initialize output variables
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
                self.nAgents[State.S] * self.payoffs[State.S],
                self.nAgents[State.P] * self.payoffs[State.P],
                self.nAgents[State.I] * self.payoffs[State.I],
                self.nAgents[State.R] * self.payoffs[State.R]])
    
    ## Run the simulation
    t = 1
    i = self.nAgents[State.I] / float(N)
    
    while ((t < self.timesteps) and (i > 0)):
      ## Number of agents in each State
      numagents = [0, 0, 0, 0]
      
      ## Shuffle the vector of agents
      shuffle(agents)
      
      ## Neighbor agents in the vector interact
      n = N
      infected = []
      while(n > 1):
        a1 = agents[n - 1]
        a2 = agents[n - 2]
        
        a1State = a1.getState()
        a2State = a2.getState()
        
        a1S = a1State
        a2S = a2State
        
        if (a1State == State.I):
          infected.append(a1)
          a2S = a2.interact(a1State)
          
        if (a2State == State.I):
          infected.append(a2)
          a1S = a1.interact(a2State)
          
        numagents[a1S] += 1
        numagents[a2S] += 1
        
        n = n - 2
        
      ## Behavioral decision
      for agent in agents:
        if (random.uniform(0.0, 1.0) < agent.getDecision()):
          state = agent.getState()
          numagents[state] -= 1
          
          state = agent.decide(i)
          numagents[state] += 1
          
      ## Recovery
      for agent in infected:
        if (agent.recover() == State.R):
          numagents[State.I] -= 1
          numagents[State.R] += 1
          
      ## Record output information
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
                  numagents[State.S] * self.payoffs[State.S],
                  numagents[State.P] * self.payoffs[State.P],
                  numagents[State.I] * self.payoffs[State.I],
                  numagents[State.R] * self.payoffs[State.R]])
      
      ## Recalculate the disease prevalence
      i = numagents[State.I] / float(N)
      
      ## Advance time
      t += 1
    
    return num
  
