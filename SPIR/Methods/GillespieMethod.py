## Code based on the paper
##
## Gillespie, D. T. (1976). A general method for numerically simulating the
## stochastic time evolution of coupled chemical reactions. Journal of
## Computational Physics. 22:403-434.
##

## Python libraries
from math import exp, log
from numpy import random

## Load our classes
from State import State
from SPIR.Utils.Util import Util

class GillespieMethod(object):
  ## Types of interactions
  INT_SP = 0  # Decision Susceptible to Prophylactic
  INT_PS = 1  # Decision Prophylactic to Susceptible
  INT_SI = 2  # Infection of a Susceptible
  INT_PI = 3  # Infection of a Prophylactic
  INT_IR = 4  # Recovery of an Infected
  
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
    self.payoffs = payoffs
    
    self.beta = 1 - exp(-beta)
    self.gamma = 1 - exp(-gamma)
    
    self.rho = 1 - exp(-rho)
    self.fear = fear
    if (self.fear == 0):
      self.fear = 1.0
        
    self.decision = decision
    self.planningHorizon = planningHorizon
    self.timesteps = timesteps
    
  ##
  ## Execute the simulation
  ##
  def execute(self):
    ##
    ## Output variables
    ##
    ## 01 - Time step
    ## 02 - Susceptible no control measure adopted
    ## 03 - Susceptible control measure adopted successfully
    ## 04 - Susceptible control measure adopted unsuccessfully
    ## 05 - Infectious no control measure adopted
    ## 06 - Infectious control measure adopted successfully
    ## 07 - Infectious control measure adopted unsuccessfully
    ## 08 - Recovered no control measure adopted
    ## 09 - Recovered control measure adopted successfully
    ## 10 - Recovered control measure adopted unsuccessfully
    ## 11 - Accumulated Susceptible payoff
    ## 12 - Accumulated Prophylactic payoff
    ## 13 - Accumulated Infectious payoff
    ## 14 - Accumulated Recovered payoff
    ##
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
    
    ## Total number of agents
    N = self.nAgents[State.S] + self.nAgents[State.P] + self.nAgents[State.I] + self.nAgents[State.R]
    
    ## Disease prevalence
    i = (self.nAgents[State.I] / float(N)) ** float(1 / float(self.fear))
    
    ## Switching points
    switchPoint = Util.calcISwitch(self.planningHorizon, self.beta, self.gamma, self.rho, self.payoffs)
    
    ## Calculate the probability of each type of event (interaction) to occur
    c = {self.INT_SP: self.decision / float(N),
         self.INT_PS: self.decision / float(N),
         self.INT_SI: self.beta / (float(N) * float(N)),
         self.INT_PI: (self.beta * self.rho) / (float(N) * float(N)),
         self.INT_IR: self.gamma / float(N)}
    
    ## Probability density of each event
    a = {self.INT_SP: c[self.INT_SP] * self.nAgents[State.S],
         self.INT_PS: c[self.INT_PS] * self.nAgents[State.P],
         self.INT_SI: c[self.INT_SI] * self.nAgents[State.S] * self.nAgents[State.I],
         self.INT_PI: c[self.INT_PI] * self.nAgents[State.P] * self.nAgents[State.I],
         self.INT_IR: c[self.INT_IR] * self.nAgents[State.I]}
    
    ## Sum of probability density of all events
    A = a[self.INT_SP] + a[self.INT_PS] + a[self.INT_SI] + a[self.INT_PI] + a[self.INT_IR]
    
    prevT = 0

    ## Calculate the time step of the next event
    t = log(1 / random.uniform(0.0, 1.0)) / float(A)
    
    ## Run simulation
    while ((t < self.timesteps) and (i > 0)):
      ## Determine the next event
      cumA = random.uniform(0.0, 1.0) * A
      index = 0
      x = a[index]
      while(x <= cumA):
        index += 1
        x += a[index]
        
      ## Execute the event and
      ## update the number of agents in each State
      if ((index == self.INT_SP) and (self.nAgents[State.S] > 0)):
        ## Behavioral decision
        self.nAgents[State.S] -= 1
        self.nAgents[State.P] += 1
        
      elif ((index == self.INT_PS) and (self.nAgents[State.P] > 0)):
        ## Behavioral decision
        self.nAgents[State.P] -= 1
        self.nAgents[State.S] += 1
        
      elif ((index == self.INT_SI) and (self.nAgents[State.S] > 0)):
        ## Infection
        self.nAgents[State.S] -= 1
        self.nAgents[State.I] += 1
        
      elif ((index == self.INT_PI) and (self.nAgents[State.P] > 0)):
        ## Infection
        self.nAgents[State.P] -= 1
        self.nAgents[State.I] += 1
        
      elif ((index == self.INT_IR) and (self.nAgents[State.I] > 0)):
        ## Recovery
        self.nAgents[State.I] -= 1
        self.nAgents[State.R] += 1
        
      ## Update output variables
      num.append([t,
              self.nAgents[State.S],
              self.nAgents[State.P],
              0,
              self.nAgents[State.I],
              0,
              0,
              self.nAgents[State.R],
              0,
              0,
              (t - prevT) * self.nAgents[State.S] * self.payoffs[State.S],
              (t - prevT) * self.nAgents[State.P] * self.payoffs[State.P],
              (t - prevT) * self.nAgents[State.I] * self.payoffs[State.I],
              (t - prevT) * self.nAgents[State.R] * self.payoffs[State.R]])
      
      ## Update simulation parameters
      prevT = t
      
      ## Recalculate the new disease prevalence
      i = (self.nAgents[State.I] / float(N)) ** float(1 / float(self.fear))
      
      if (i > 0):
        if (self.fear != 0):
          adjustedI = i ** float(1 / float(self.fear))
        else:
          adjustedI = i
        
        ## Determine the preferred State for the agent
        switch = 0
        for iswitch in switchPoint:
          if ((adjustedI > iswitch[0]) and (adjustedI < iswitch[1])):
            switch = State.STATES[iswitch[2]]
            
        ## Recalculate the probability density of each event
        a = {self.INT_SP: c[self.INT_SP] * self.nAgents[State.S] * switch,
             self.INT_PS: c[self.INT_PS] * self.nAgents[State.P] * (1 - switch),
             self.INT_SI: c[self.INT_SI] * self.nAgents[State.S] * self.nAgents[State.I],
             self.INT_PI: c[self.INT_PI] * self.nAgents[State.P] * self.nAgents[State.I],
             self.INT_IR: c[self.INT_IR] * self.nAgents[State.I]}
        
        ## Sum the probability density of all events
        A = a[self.INT_SP] + a[self.INT_PS] + a[self.INT_SI] + a[self.INT_PI] + a[self.INT_IR]
        
        ## Advance time to next event
        t = t + (log(1 / random.uniform(0.0, 1.0)) / float(A))
        
    return num
