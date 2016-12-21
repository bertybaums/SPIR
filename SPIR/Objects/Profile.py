## Load our classes
from Utils.Util import Util

class Profile(object):
  ##
  ## Description: Constructor method
  ##
  ## @param None
  ##
  ## @return None
  ##
  def __init__(self):
    ## Number of agents
    self.num_agents = 0
    
    ## Agent states payoff
    self.payoffs = [["", ""], ["", ""], ["", ""], ["", ""]]
    
    ## 1 - Prophylactic protection
    self.rho = ["", ""]
    
    ## Fear factor
    self.kappa = ["", ""]
    
    ## Behavioral decision frequency
    self.delta = ["", ""]
    
    ## Planning horizon
    self.h = ["", ""]
    
  ##
  ## Description: Get number of agents
  ##
  ## @param None
  ##
  ## @return Number of agents
  ##
  def getNumAgents(self):
    return self.num_agents
  
  ##
  ## Description: Set number of agents
  ##
  ## @param num_agents  Number of agents
  ##
  ## @return None
  ##
  def setNumAgents(self, num_agents):
    self.num_agents = num_agents
    
  ##
  ## Description: Get agent states payoff
  ##
  ## @param None
  ##
  ## @return Agent states payoff
  ##
  def getPayoffs(self):
    values = []
    for payoff in self.payoffs:
      value = Util.getValue(payoff[0], payoff[1])
      if value < 0:
        value = 0.00
      elif value > 1:
        value = 1.00
      values.append(value)
      
    return values
  
  ##
  ## Description: Set payoff attributes and contents
  ##
  ## @param attribute  Type of content
  ## @param content    Content
  ##
  ## @return None
  ##
  def setPayoffs(self, state, attribute, content):
    self.payoffs[state] = [attribute, content]
    
  ##
  ## Description: Get rho
  ##
  ## @param None
  ##
  ## @return Rho
  ##
  def getRho(self):
    value = Util.getValue(self.rho[0], self.rho[1])
    if value < 0:
      value = 0.00
    elif value > 1:
      value = 1.00
    return value
  
  ##
  ## Description: Set rho attribute and content
  ##
  ## @param attribute  Type of content
  ## @param content    Content
  ##
  ## @return None
  ##
  def setRho(self, attribute, content):
    self.rho = [attribute, content]
  
  ##
  ## Description: Get fear factor
  ##
  ## @param None
  ##
  ## @return Fear factor
  ##
  def getFear(self):
    value = Util.getValue(self.kappa[0], self.kappa[1])
    if value < 0:
      value = 0.00
      
    return value
  
  ##
  ## Description: Set fear factor attribute and content
  ##
  ## @param attribute  Type of content
  ## @param content    Content
  ##
  ## @return None
  ##
  def setFear(self, attribute, content):
    self.kappa = [attribute, content]
  
  ##
  ## Description: Get behavioral decision frequency
  ##
  ## @param None
  ##
  ## @return Decision frequency
  ##
  def getDecision(self):
    value = Util.getValue(self.delta[0], self.delta[1])
    if value < 0:
      value = 0.00
    elif value > 1:
      value = 1.00
      
    return value
  
  ##
  ## Description: Set behavioral decision frequency attribute and content
  ##
  ## @param attribute  Type of content
  ## @param content    Content
  ##
  ## @return None
  ##
  def setDecision(self, attribute, content):
    self.delta = [attribute, content]
  
  ##
  ## Description: Get planning horizon
  ##
  ## @param None
  ##
  ## @return Planning horizon
  ##
  def getPlanningHorizon(self):
    value = Util.getValue(self.h[0], self.h[1])
    if value < 0:
      value = 0.00
      
    return int(value)
  
  ##
  ## Description: Set planning horizon attribute and content
  ##
  ## @param attribute  Type of content
  ## @param content    Content
  ##
  ## @return None
  ##
  def setPlanningHorizon(self, attribute, content):
    self.h = [attribute, content]
    