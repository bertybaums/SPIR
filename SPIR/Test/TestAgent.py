from Objects.Agent import Agent

if __name__ == '__main__':
  # (self.numAgents, state, self.beta, self.gamma,
  # profile.getRho(), profile.getFear(), profile.getDecision(),
  # profile.getPlanningHorizon(), profile.getPayoffs())
  agent = Agent(10, 0, 0.030524426923974057, 0.014888060396937353,
                0.9658874349249019, 1.2490949065270287, 0.06663886467564656,
                86, [1, 0.93306046546803, 0.14804095326469574, 0.9803154484289105])
  
  print(agent.getSwitchingPoint())
  print(str(agent.getSwitchingPoint()[0][1]))
  