# # Load our classes
from Utils.Util import Util

class Profile(object):
  # #
  # # Description: Constructor method
  # #
  # # @param None
  # #
  # # @return None
  # #
  def __init__(self):
    # # Number of Susceptible agents
    self.num_agents_s = 0
    
    # # Number of Prophylactic agents
    self.num_agents_p = 0
    
    # # Number of Infectious agents
    self.num_agents_i = 0
    
    # # Number of Recovered agents
    self.num_agents_r = 0
    
    # # Agent states payoff
    self.payoffs = [["", ""], ["", ""], ["", ""], ["", ""]]
    
    # # 1 - Prophylactic protection
    self.rho = ["", ""]
    
    # # Fear factor
    self.kappa = ["", ""]
    
    # # Behavioral decision frequency
    self.delta = ["", ""]
    
    # # Planning horizon
    self.h = ["", ""]
    
  # #
  # # Description: Get number of agents
  # #
  # # @param None
  # #
  # # @return Number of agents
  # #
  def getNumAgents(self):
    return [self.num_agents_s,
            self.num_agents_p,
            self.num_agents_i,
            self.num_agents_r]
  
  # #
  # # Description: Get number of Susceptible agents
  # #
  # # @param None
  # #
  # # @return Number of Susceptible agents
  # #
  def getNumAgentsS(self):
    return self.num_agents_s
  
  # #
  # # Description: Set number of Susceptible agents
  # #
  # # @param num_agents  Number of Susceptible agents
  # #
  # # @return None
  # #
  def setNumAgentsS(self, num_agents_s):
    self.num_agents_s = num_agents_s
    
  # #
  # # Description: Get number of Prophylactic agents
  # #
  # # @param None
  # #
  # # @return Number of Prophylactic agents
  # #
  def getNumAgentsP(self):
    return self.num_agents_p
  
  # #
  # # Description: Set number of Prophylactic agents
  # #
  # # @param num_agents  Number of Prophylactic agents
  # #
  # # @return None
  # #
  def setNumAgentsP(self, num_agents_p):
    self.num_agents_p = num_agents_p
    
  # #
  # # Description: Get number of Infectious agents
  # #
  # # @param None
  # #
  # # @return Number of Infectious agents
  # #
  def getNumAgentsI(self):
    return self.num_agents_i
  
  # #
  # # Description: Set number of Infectious agents
  # #
  # # @param num_agents  Number of Infectious agents
  # #
  # # @return None
  # #
  def setNumAgentsI(self, num_agents_i):
    self.num_agents_i = num_agents_i
    
  # #
  # # Description: Get number of Recovered agents
  # #
  # # @param None
  # #
  # # @return Number of Recovered agents
  # #
  def getNumAgentsR(self):
    return self.num_agents_r
  
  # #
  # # Description: Set number of Recovered agents
  # #
  # # @param num_agents  Number of Recovered agents
  # #
  # # @return None
  # #
  def setNumAgentsR(self, num_agents_r):
    self.num_agents_r = num_agents_r
    
  # #
  # # Description: Get agent states payoff
  # #
  # # @param None
  # #
  # # @return Agent states payoff
  # #
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
  
  # #
  # # Description: Set payoff attributes and contents
  # #
  # # @param attribute  Type of content
  # # @param content    Content
  # #
  # # @return None
  # #
  def setPayoffs(self, state, attribute, content):
    self.payoffs[state] = [attribute, content]
    
  # #
  # # Description: Get rho
  # #
  # # @param None
  # #
  # # @return Rho
  # #
  def getRho(self):
    value = Util.getValue(self.rho[0], self.rho[1])
    if value < 0:
      value = 0.00
    elif value > 1:
      value = 1.00
    return value
  
  # #
  # # Description: Set rho attribute and content
  # #
  # # @param attribute  Type of content
  # # @param content    Content
  # #
  # # @return None
  # #
  def setRho(self, attribute, content):
    self.rho = [attribute, content]
  
  # #
  # # Description: Get fear factor
  # #
  # # @param None
  # #
  # # @return Fear factor
  # #
  def getFear(self):
    value = Util.getValue(self.kappa[0], self.kappa[1])
    if value < 0:
      value = 0.00
      
    return value
  
  # #
  # # Description: Set fear factor attribute and content
  # #
  # # @param attribute  Type of content
  # # @param content    Content
  # #
  # # @return None
  # #
  def setFear(self, attribute, content):
    self.kappa = [attribute, content]
  
  # #
  # # Description: Get behavioral decision frequency
  # #
  # # @param None
  # #
  # # @return Decision frequency
  # #
  def getDecision(self):
    value = Util.getValue(self.delta[0], self.delta[1])
    if value < 0:
      value = 0.00
    elif value > 1:
      value = 1.00
      
    return value
  
  # #
  # # Description: Set behavioral decision frequency attribute and content
  # #
  # # @param attribute  Type of content
  # # @param content    Content
  # #
  # # @return None
  # #
  def setDecision(self, attribute, content):
    self.delta = [attribute, content]
  
  # #
  # # Description: Get planning horizon
  # #
  # # @param None
  # #
  # # @return Planning horizon
  # #
  def getPlanningHorizon(self):
    value = Util.getValue(self.h[0], self.h[1])
    if value < 0:
      value = 0.00
      
    return int(value)
  
  # #
  # # Description: Set planning horizon attribute and content
  # #
  # # @param attribute  Type of content
  # # @param content    Content
  # #
  # # @return None
  # #
  def setPlanningHorizon(self, attribute, content):
    self.h = [attribute, content]
    
