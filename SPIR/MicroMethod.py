##
## Python libraries
##
import math
from random import shuffle, uniform

##
## Load our classes
##
from Agent import Agent
from Constants import Constant
from State import State
from Util import Util

class MicroMethod(object):
  ##
  ## Description: Constructor method
  ##
  ## @param nAgents      Number of agents
  ## @param payoffs      Payoff of each disease state
  ## @param disease      Disease parameters
  ## @param fear         Distortion of disease prevalence
  ## @param decision     Decision frequency
  ## @param timeHorizon  Planning horizon
  ## @param timeSteps    Number of time steps to simulate
  ##
  ## @return None
  ##
  def __init__(self, nAgents, payoffs, disease, fear, decision, timeHorizon, timeSteps):
    self.nAgents = nAgents
    self.payoffs = payoffs
    self.disease = disease
    
    self.fear = fear
    if (self.fear == 0):
        self.fear = 1.0
    
    self.decision = decision
    self.timeHorizon = timeHorizon
    self.timeSteps = timeSteps
    
  ##
  ## Description: Execute the simulation
  ##
  ## @param None
  ##
  ## @return None
  ##
  def execute(self):
    ## Disease parameters
    pDisease = {Constant.BETA: 1 - math.exp(-self.disease[Constant.BETA]),
                Constant.RHO: self.disease[Constant.RHO],
                Constant.GAMMA: 1 - math.exp(-self.disease[Constant.GAMMA])}
    
    ## Frequency of behavioral decision
    self.decision = 1 - math.exp(-self.decision)
    
    ## Calculate switching points
    switchPoint = Util.calcISwitch(self.timeHorizon, pDisease, self.payoffs)
    
    ## Initialize agents
    N = 0
    agents = []
    infected = []
    for state in self.nAgents:
      for x in range(self.nAgents[state]):
        agent = Agent(N, state, pDisease, self.fear, self.timeHorizon, self.payoffs, switchPoint)
        agents.append(agent)
        
        if (state == State.I):
          infected.append(agent)
          
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
    
    while ((t < self.timeSteps) and (i > 0)):
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
        if (uniform(0.0, 1.0) < self.decision):
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
