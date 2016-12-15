##
## Python libraries
##
from random import uniform

##
## Our classes
##
from Constants import Constant
from State import State
from Util import Util


class Agent(object):
  ##
  ## Description: Constructor method
  ##
  ## @param num          Identification of the agent
  ## @param state        Current state of the agent
  ## @param disease      Disease parameters
  ## @param fear         Distortion of disease prevalence
  ## @param timeHorizon  Planning horizon
  ## @param payoffs      Payoff of each disease state
  ## @param switchPoint  Switching points
  ##
  ## @return None
  ##
  def __init__(self, num, state, disease, fear, timeHorizon, payoffs, switchPoint = None):
    self.num = num
    self.state = state
    self.disease = disease
    self.fear = fear
    self.timeHorizon = timeHorizon
    self.payoffs = payoffs
    
    if (switchPoint == None):
      self.switchPoint = Util.calcISwitch(self.timeHorizon, self.disease, self.payoffs)
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
  ## Description: Get disease information
  ##
  ## @param None
  ##
  ## @return Disease information
  ##
  def getDisease(self):
    return self.disease
  
  
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
  def getTimeHorizon(self):
    return self.timeHorizon
  
  
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
  ## Description: Set disease information
  ##
  ## @param disease Disease information
  ##
  ## @return None
  ##
  def setDisease(self, disease):
    self.disease = disease
    
    
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
  ## Description: Set planning horizon
  ##
  ## @param timeHorizon Planning horizon
  ##
  ## @return None
  ##
  def setTimeHorizon(self, timehorizon):
    self.timeHorizon = timehorizon
    
    
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
      h = self.timeHorizon
      q = self.disease[Constant.GAMMA]
      
      ## Calculate expected times of Susceptible
      ps = i * self.disease[Constant.BETA]
      Tss = (1 - ((1 - ps) ** h)) / ps
      if (ps != q):
        Tis = (1 / q) - (((ps * ((1 - q) ** h)) / (q * (ps - q))) * (1 - (((1 - ps) / (1 - q)) ** h))) - (((1 - ps) ** h) / q)
      else:
        Tis = (1 / q) - ((ps * h * ((1 - q) ** (h - 1))) / q) - (((1 - ps) ** h) / q)
      Trs = h - Tss - Tis
                
      ## Calculate expected times of Prophylactic
      pp = i * self.disease[Constant.BETA] * self.disease[Constant.RHO]
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
        if (uniform(0.0, 1.0) < self.disease[Constant.BETA]):
          self.state = State.I
        elif (self.state == State.P):
          if (uniform(0.0, 1.0) < (self.disease[Constant.BETA] * self.disease[Constant.RHO])):
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
    if ((self.state == State.I) and (uniform(0.0, 1.0) < self.disease[Constant.GAMMA])):
      self.state = State.R
    return self.state
  