## Code based on the paper
##
## Cao, Y., Gillespie, D. T., Petzold, L. R. (2006). Efficient step size
## selection for the tau-leaping simulation method. The Journal of Chemical
## Physics. 124:044109.
##

## Python libraries
from math import ceil, exp, inf, log
from numpy import arange, random

## Load our classes
from State import State
from SPIR.Utils.Util import Util

class EfficientTauLeapMethod(object):
  ## Number and types of interactions
  NUM_INTERACTIONS = 5
  INT_SP = 0  # Decision Susceptible to Prophylactic
  INT_PS = 1  # Decision Prophylactic to Susceptible
  INT_SI = 2  # Infection of a Susceptible
  INT_PI = 3  # Infection of a Prophylactic
  INT_IR = 4  # Recovery of an Infected
  
  INTERACTIONS = [INT_SP, INT_PS, INT_SI, INT_PI, INT_IR]
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
    
    ## Agents involved in each type of event (interaction)
    r = [[State.S, State.P],
         [State.P, State.S],
         [State.S, State.I],
         [State.P, State.I],
         [State.I, State.R]]
    
    ## Sum of probability density of all events
    A = a[self.INT_SP] + a[self.INT_PS] + a[self.INT_SI] + a[self.INT_PI] + a[self.INT_IR]
    
    ## Change on the number of agents in each State depending on the event
    v = [[-1, 1, -1, 0, 0],
         [1, -1, 0, -1, 0],
         [0, 0, 1, 1, -1],
         [0, 0, 0, 0, 1]]
    
    nc = 10
    g = [2, 2, 2, 1]
    e = 0.03
    
    prevT = 0
    t = 0
    
    ## Run simulation
    while((t < self.timesteps) and (i > 0)):
      ## Identify critical and non-critical interactions
      Incr = [State.S, State.P, State.I, State.R]
      jc = []
      jnc = []
      for j in range(self.NUM_INTERACTIONS):
        l = 0
        first = True
        for s in State.STATES:
          if(v[s][j] < 0):
            aux = ceil(self.nAgents[s] / abs(v[s][j]))
            if((aux < l) or (first)):
              l = aux
              first = False
                  
        if((l < nc) and (a[j] > 0)):
          jc.append(j)
          for reaction in r[j]:
            if(reaction in Incr):
              Incr.remove(reaction)
        else:
          jnc.append(j)
          
      ## Calculate tau'
      tau1 = inf
      if(len(jnc) > 0):
        u = [0 for s in State.STATES]
        std = [0 for s in State.STATES]
        for s in State.STATES:
          for j in jnc:
            aux = v[s][j] * a[j]
            u[s] += aux
            std[s] += aux * v[s][j]
            
        for s in State.STATES:
          if(s in Incr):
            aux = max((e * self.nAgents[s]) / g[s], 1)
            if(abs(u[s]) != 0):
              term1 = aux / abs(u[s])
            else:
              term1 = inf
              
            aux = aux * aux
            if(std[s] != 0):
              term2 = aux / std[s]
            else:
              term2 = inf
              
            mintau = min(term1, term2)
            if(mintau < tau1):
              tau1 = mintau
              
      ## Calculate tau''
      tau2 = inf
      Ac = 0
      for j in jc:
        Ac += a[j]
        
        if(Ac > 0):
          tau2 = random.exponential(1 / Ac)
          
      if(tau1 < (10 / A)):
        count = 0
        while ((t < self.timesteps) and (i > 0) and (count < 10)):
          cumA = random.uniform(0.0, 1.0) * A
          index = 0
          x = a[index]
          while(x <= cumA):
            index += 1
            x += a[index]
            
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
          count += 1
          prevT = t
          
          ## Recalculate the disease prevalence
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
      else:
        positive = False
        while(not positive):
          k = [0 for k in range(self.NUM_INTERACTIONS)]
          
          if(tau1 < tau2):
            tau = tau1
            for j in range(self.NUM_INTERACTIONS):
              if((j in jc) or (a[j] == 0)):
                k[j] = 0
              else:
                k[j] = random.poisson(a[j] * tau)
          else:
            tau = tau2
            
            selJC = -1
            if((len(jc) > 0) and (Ac > 0)):
              selJC = random.choice(arange(0, len(jc)), p=[a[j] / Ac for j in jc])
            
            for j in range(self.NUM_INTERACTIONS):
              if(j in jc):
                if(j == jc[selJC]):
                  k[j] = 1
                else:
                  k[j] = 0
              else:
                if(a[j] > 0):
                  k[j] = random.poisson(a[j] * tau)
                else:
                  k[j] = 0
                  
          negative = False
          for s in State.STATES:
            value = self.nAgents[s]
            for j in range(self.NUM_INTERACTIONS):
              value += k[j] * v[s][j]
            
            if(value < 0):
              negative = True
                
          if(negative):
            tau1 = tau1 / 2
            tau2 = tau2 / 2
          else:
            for s in State.STATES:
              for j in range(self.NUM_INTERACTIONS):
                self.nAgents[s] += k[j] * v[s][j]
              
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
            positive = True
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
              t += tau
              
    return num
