from SPIR.Objects.Config import Config

if __name__ == '__main__':
  config = Config("/data/workspace/cmci/SPIR/config/config.xml")
  
  print(config.getBeta())
  print(config.getGamma())
  
  for profile in config.getProfiles():
    print(profile.getNumAgents())
    print(profile.getPayoffs())
    print(profile.getRho())
    print(profile.getFear())
    print(profile.getDecision())
    print(profile.getPlanningHorizon())
    
  print(config.getMethod())
  print(config.getReplications())
  print(config.getTimesteps())
  
  print(config.getOutputFormat())
  print(config.getOutputWindow())
  print(config.getOutputPath())
  print(config.getOutputFilename())
  print(config.getOutputHeader())
  print(config.getOutputSeparator())