## Python libraries
from numpy import random

## Load our classes
from State import State
from SPIR.Utils.Util import Util


class Agent(object):
  ##
  ## Description: Constructor method
  ##
  ## @param num              Identification of the agent
  ## @param state            Current state of the agent
  ## @param beta             Disease beta
  ## @param gamma            Disease gamma
  ## @param rho              1 - Prophylactic protection
  ## @param fear             Distortion of disease prevalence
  ## @param decision         Frequency of behavioral decision
  ## @param planningHorizon  Planning horizon
  ## @param payoffs          Payoff of each disease state
  ## @param switchPoint      Switching points
  ##
  ## @return None
  ##
  def __init__(self, num, state, beta, gamma, rho, fear, decision, planningHorizon, payoffs, switchPoint = None):
    self.num = num
    self.state = state
    self.beta = beta
    self.gamma = gamma
    self.rho = rho
    self.fear = fear
    self.decision = decision
    self.planningHorizon = planningHorizon
    self.payoffs = payoffs
    
    if (switchPoint == None):
      self.switchPoint = Util.calcISwitch(self.planningHorizon, self.beta, self.gamma, self.rho, self.payoffs)
    else:
      self.switchPoint = switchPoint
    
    
  ##
  ## Description: Get agent identification
  ##
  ## @param None
  ##
  ## @return Agent identification
  ##
  def getID(self):
    return self.num
  
  
  ##
  ## Description: Get agent state
  ##
  ## @param None
  ##
  ## @return Agent state
  ##
  def getState(self):
    return self.state
  
  
  ##
  ## Description: Get disease beta
  ##
  ## @param None
  ##
  ## @return Disease beta
  ##
  def getBeta(self):
    return self.beta
  
  
  ##
  ## Description: Get disease gamma
  ##
  ## @param None
  ##
  ## @return Disease gamma
  ##
  def getGamma(self):
    return self.gamma
  
  
  ##
  ## Description: Get 1 - prophylactic protection
  ##
  ## @param None
  ##
  ## @return 1 - Prophylactic protection
  ##
  def getRho(self):
    return self.rho
  
  
  ##
  ## Description: Get distortion of disease prevalence
  ##
  ## @param None
  ##
  ## @return Distortion of disease prevalence
  ##
  def getFear(self):
    return self.fear
  
  
  ##
  ## Description: Get planning horizon
  ##
  ## @param None
  ##
  ## @return Planning horizon
  ##
  def getDecision(self):
    return self.decision
  
  
  ##
  ## Description: Get planning horizon
  ##
  ## @param None
  ##
  ## @return Planning horizon
  ##
  def getPlanningHorizon(self):
    return self.planningHorizon
  
  
  ##
  ## Description: Get payoffs
  ##
  ## @param None
  ##
  ## @return Payoffs
  ##
  def getPayoffs(self):
    return self.payoffs
  
  
  ##
  ## Description: Set agent state
  ##
  ## @param state State
  ##
  ## @return None
  ##
  def setState(self, state):
    self.state = state
    
    
  ##
  ## Description: Set disease beta
  ##
  ## @param beta  Disease beta
  ##
  ## @return None
  ##
  def setBeta(self, beta):
    self.beta = beta
    
    
  ##
  ## Description: Set disease gamma
  ##
  ## @param gamma  Disease gamma
  ##
  ## @return None
  ##
  def setGamma(self, gamma):
    self.gamma = gamma
    
    
  ##
  ## Description: Set 1 - Prophylactic protection
  ##
  ## @param rho  1 - Prophylactic protection
  ##
  ## @return None
  ##
  def setRho(self, rho):
    self.rho = rho
    
    
  ##
  ## Description: Set distortion of disease prevalence
  ##
  ## @param fear Distortion of disease prevalence
  ##
  ## @return None
  ##
  def setFear(self, fear):
    self.fear = fear
    
    
      ##
  ## Description: Set frequency of behavioral decision
  ##
  ## @param decision  Frequency of behavioral decision
  ##
  ## @return None
  ##
  def setDecision(self, decision):
    self.decision = decision
    
    
  ##
  ## Description: Set planning horizon
  ##
  ## @param planningHorizon Planning horizon
  ##
  ## @return None
  ##
  def setPlanningHorizon(self, planninghorizon):
    self.planningHorizon = planninghorizon
    
    
  ##
  ## Description: Set payoffs
  ##
  ## @param payoffs Payoffs
  ##
  ## @return None
  ##
  def setPayoffs(self, payoffs):
    self.payoffs = payoffs
    
    
  ##
  ## Description: Calculate Susceptible and Prophylactic utilities
  ##
  ## @param i Current disease prevalence
  ##
  ## @return Vector with Susceptible and Prophylactic utilities
  ##
  def calcUtilities(self, i):
    US = 0
    UP = 0
    if (i > 0):
      h = self.planningHorizon
      q = self.gamma
      
      ## Calculate expected times of Susceptible
      ps = i * self.beta
      Tss = (1 - ((1 - ps) ** h)) / ps
      if (ps != q):
        Tis = (1 / q) - (((ps * ((1 - q) ** h)) / (q * (ps - q))) * (1 - (((1 - ps) / (1 - q)) ** h))) - (((1 - ps) ** h) / q)
      else:
        Tis = (1 / q) - ((ps * h * ((1 - q) ** (h - 1))) / q) - (((1 - ps) ** h) / q)
      Trs = h - Tss - Tis
                
      ## Calculate expected times of Prophylactic
      pp = i * self.beta * self.rho
      Tpp = (1 - ((1 - pp) ** h)) / pp
      if (pp != q):
        Tip = (1 / q) - (((pp * ((1 - q) ** h)) / (q * (pp - q))) * (1 - (((1 - pp) / (1 - q)) ** h))) - (((1 - pp) ** h) / q)
      else:
        Tip = (1 / q) - ((pp * h * ((1 - q) ** (h - 1))) / q) - (((1 - pp) ** h) / q)
      Trp = h - Tpp - Tip
      
      ## Calculate Expected Utilities
      US = (self.payoffs[State.S] * Tss) + (self.payoffs[State.I] * Tis) + (self.payoffs[State.R] * Trs)
      UP = (self.payoffs[State.P] * Tpp) + (self.payoffs[State.I] * Tip) + (self.payoffs[State.R] * Trp)
      
    return [US, UP]
  
  
  ##
  ## Description: Interaction with another in state State
  ##
  ## @param state State of agent interacting to
  ##
  ## @return state New state
  ##
  def interact(self, state):
    if (state == State.I):
      if (self.state == State.S):
        if (random.uniform(0.0, 1.0) < self.beta):
          self.state = State.I
        elif (self.state == State.P):
          if (random.uniform(0.0, 1.0) < (self.beta * self.rho)):
            self.state = State.I
    return self.state
    
    
  ##
  ## Description: Perform a behavioral decision to adopt the Susceptible or
  ##              the Prophylactic state
  ##
  ## @param i Current disease prevalence
  ##
  ## @return state New state
  ##
  def decide(self, i):
    if (self.fear != 0):
      adjustedI = i ** float(1 / float(self.fear))
    else:
      adjustedI = i
    
    if (((self.state == State.S) or (self.state == State.P)) and (adjustedI > 0)):
      for iswitch in self.switchPoint:
        if ((adjustedI > iswitch[0]) and (adjustedI < iswitch[1])):
          self.state = State.STATES[iswitch[2]]
          
    return self.state
    
    
  ##
  ## Description: Recover from the disease
  ##
  ## @param None
  ##
  ## @return state New state
  ##
  def recover(self):
    if ((self.state == State.I) and (random.uniform(0.0, 1.0) < self.gamma)):
      self.state = State.R
    return self.state
  