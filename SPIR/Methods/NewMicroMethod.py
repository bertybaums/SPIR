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

class NewMicroMethod(object):
  ##
  ## Description: Constructor method
  ##
  ## @param nAgents      Number of agents
  ## @param profiles     Agent profile
  ## @param beta         Disease beta
  ## @param gamma        Disease gamma
  ## @param timesteps    Number of time steps to simulate
  ##
  ## @return None
  ##
  def __init__(self, nAgents, profiles, beta, gamma, timesteps):
    self.nAgents = nAgents
    self.profiles = profiles
    self.beta = 1 - exp(-beta)
    self.gamma = 1 - exp(-gamma)
    self.timesteps = timesteps
    
  ##
  ## Description: Execute the simulation
  ##
  ## @param None
  ##
  ## @return None
  ##
  def execute(self):
    ## Initialize agents
    N = 0
    agents = []
    nagents = list(self.nAgents.values())
    for profile in self.profiles:
      na = profile.getNumAgents()
      while (sum(nagents) > 0) and (na > 0):
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
        na -= 1
        
        state = State.STATES[i]
        
        agent = Agent(N, state, self.beta, self.gamma, profile.getRho(),
                      profile.getFear(), profile.getDecision(),
                      profile.getPlanningHorizon(), profile.getPayoffs())
        
        agents.append(agent)
        
        N += 1
        
    ## Initialize output variables
    total = [0, 0, 0, 0]
    for agent in agents:
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
    i = self.nAgents[State.I] / float(N)
    
    while ((t < self.timesteps) and (i > 0)):
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
      i = numagents[State.I] / float(N)
      
      ## Advance time
      t += 1
    
    return num
