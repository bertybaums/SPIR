## Python library
import xml.etree.ElementTree as ET

## Load our classes
from Constants import Constant
from Objects.Profile import Profile
from Utils.Util import Util

class Config(object):
  ##
  ## Description: Constructor method
  ##
  ## @param filename    Configuration filename
  ##
  ## @return None
  ##
  def __init__(self, filename):
    self.filename = filename
    
    self.profiles = []
    
    self.beta = 0
    self.gamma = 0
    
    self.method = 0
    self.replications = 1
    self.time_steps = 1000
    
    self.output_format = 0
    self.output_window = 0
    self.output_path = "."
    self.output_filename = "output.csv"
    self.output_header = True
    self.output_separator = ";"
    
    self.parsing()
    
  ##
  ## Description: Get configuration filename
  ##
  ## @param  None
  ##
  ## @return  Configuration filename
  ##
  def getFilename(self):
    return self.filename
  
  ##
  ## Description: Get agent profiles
  ##
  ## @param  None
  ##
  ## @return  Agent profiles
  ##
  def getProfiles(self):
    return self.profiles
  
  ##
  ## Description: Get disease Beta
  ##
  ## @param  None
  ##
  ## @return  Disease Beta
  ##
  def getBeta(self):
    return self.beta
  
  ##
  ## Description: Get disease Gamma
  ##
  ## @param  None
  ##
  ## @return  Disease gamma
  ##
  def getGamma(self):
    return self.gamma
  
  ##
  ## Description: Get simulation method
  ##
  ## @param  None
  ##
  ## @return  Simulation method
  ##
  def getMethod(self):
    return self.method
  
  ##
  ## Description: Get number of replications
  ##
  ## @param  None
  ##
  ## @return  Number of replications
  ##
  def getReplications(self):
    return self.replications
  
  ##
  ## Description: Get number of timesteps
  ##
  ## @param  None
  ##
  ## @return  Number of timesteps
  ##
  def getTimesteps(self):
    return self.time_steps
  
  ##
  ## Description: Get output format
  ##
  ## @param  None
  ##
  ## @return  Output format
  ##
  def getOutputFormat(self):
    return self.output_format
  
  ##
  ## Description: Get output window
  ##
  ## @param  None
  ##
  ## @return  Output window
  ##
  def getOutputWindow(self):
    return self.output_window
  
  ##
  ## Description: Get output path
  ##
  ## @param  None
  ##
  ## @return  Output path
  ##
  def getOutputPath(self):
    return self.output_path
  
  ##
  ## Description: Get output filename
  ##
  ## @param  None
  ##
  ## @return  Output filename
  ##
  def getOutputFilename(self):
    return self.output_filename
  
  ##
  ## Description: Get output header
  ##
  ## @param  None
  ##
  ## @return  Output header
  ##
  def getOutputHeader(self):
    return self.output_header
  
  ##
  ## Description: Get output field separator
  ##
  ## @param  None
  ##
  ## @return  Output field separator
  ##
  def getOutputSeparator(self):
    return self.output_separator
  
  ##
  ## Description: Parse the configuration file and creates a configuration object
  ##
  ## @param None
  ##
  ## @return None
  def parsing(self):
    config = ET.parse(self.filename)
    
    root = config.getroot()
    
    if root.tag == Constant.CONFIG:
      
      for e1 in root:
        ## PROFILES parameters
        if e1.tag == Constant.PROFILES:
          for e2 in e1:
            if e2.tag == Constant.PROFILE:
              p = Profile()
              for e3 in e2:
                if e3.tag == Constant.NUM_AGENTS_S:
                  p.setNumAgentsS(int(e3.text))
                elif e3.tag == Constant.NUM_AGENTS_P:
                  p.setNumAgentsP(int(e3.text))
                elif e3.tag == Constant.NUM_AGENTS_I:
                  p.setNumAgentsI(int(e3.text))
                elif e3.tag == Constant.NUM_AGENTS_R:
                  p.setNumAgentsR(int(e3.text))
                elif e3.tag == Constant.PAYOFF_S:
                  p.setPayoffs(0, e3.attrib.get("value"), e3.text)
                elif e3.tag == Constant.PAYOFF_P:
                  p.setPayoffs(1, e3.attrib.get("value"), e3.text)
                elif e3.tag == Constant.PAYOFF_I:
                  p.setPayoffs(2, e3.attrib.get("value"), e3.text)
                elif e3.tag == Constant.PAYOFF_R:
                  p.setPayoffs(3, e3.attrib.get("value"), e3.text)
                elif e3.tag == Constant.RHO:
                  p.setRho(e3.attrib.get("value"), e3.text)
                elif e3.tag == Constant.KAPPA:
                  p.setFear(e3.attrib.get("value"), e3.text)
                elif e3.tag == Constant.DECISION_FREQUENCY:
                  p.setDecision(e3.attrib.get("value"), e3.text)
                elif e3.tag == Constant.PLANNING_HORIZON:
                  p.setPlanningHorizon(e3.attrib.get("value"), e3.text)
              self.profiles.append(p)
              
        ## DISEASE parameters
        elif e1.tag == Constant.DISEASE:
          for e2 in e1:
            if e2.tag == Constant.BETA:
              self.beta = Util.getValue(e2.attrib.get("value"), e2.text)
            elif e2.tag == Constant.GAMMA:
              self.gamma = Util.getValue(e2.attrib.get("value"), e2.text)
              
        ## SIMULATION parameters
        elif e1.tag == Constant.SIMULATION:
          for e2 in e1:
            if e2.tag == Constant.METHOD:
              self.method = int(e2.text)
            elif e2.tag == Constant.REPLICATIONS:
              self.replications = int(e2.text)
            elif e2.tag == Constant.TIME_STEPS:
              self.time_steps = int(e2.text)
              
        ## OUTPUT parameters
        elif e1.tag == Constant.OUTPUT:
          for e2 in e1:
            if e2.tag == Constant.OUTPUT_FORMAT:
              self.output_format = int(e2.text)
            elif e2.tag == Constant.OUTPUT_WINDOW:
              self.output_window = int(e2.text)
            elif e2.tag == Constant.OUTPUT_PATH:
              self.output_path = e2.text
            elif e2.tag == Constant.OUTPUT_FILENAME:
              self.output_filename = e2.text
            elif e2.tag == Constant.OUTPUT_HEADER:
              self.output_header = bool(e2.text)
            elif e2.tag == Constant.OUTPUT_SEP:
              self.output_separator = e2.text
              
