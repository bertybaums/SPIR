##
## Our classes
##
from Constants import Constant
from State import State

class Util(object):
  ##
  ## Description: Calculate the switching points
  ##
  ## @param timeHorizon  Planning horizon
  ## @param disease      Disease parameters
  ## @param payoffs      Payoff of each disease state
  ##
  ## @return Switching point
  ##
  @staticmethod
  def calcISwitch(timeHorizon, disease, payoffs):
    h = timeHorizon
    q = disease[Constant.GAMMA]
    
    iswitch = []
    pI = 0
    pUs = -1
    pUp = -1
    iState = -1
    step = 0.00001
    i = step
    while (i <= 1.0):
      ## Calculate expected times of Susceptible
      ps = i * disease[Constant.BETA]
      Tss = ((1 / float(ps)) - 1) * (1 - ((1 - ps) ** h))
      if (ps != q):
        Tis = ((1 / float(q)) - 1) * (((((1 / float(q)) - 1) * (1 - ((1 - q) ** h))) - (((1 / float(ps)) - 1) * (1 - ((1 - ps) ** h)))) / ((((1 / float(q)) - 1)) - ((1 / float(ps)) - 1)))
      else:
        Tis = (((1 / float(q)) - 1) * (1 - ((1 - q) ** h))) - (h * ((1 - q) ** (h + 1)))
      Trs = h - Tss - Tis
      
      ## Calculate expected times of Prophylactic
      pp = i * disease[Constant.BETA] * disease[Constant.RHO]
      Tpp = ((1 / float(pp)) - 1) * (1 - ((1 - pp) ** h))
      if (pp != q):
        Tip = ((1 / float(q)) - 1) * (((((1 / float(q)) - 1) * (1 - ((1 - q) ** h))) - (((1 / float(pp)) - 1) * (1 - ((1 - pp) ** h)))) / ((((1 / float(q)) - 1)) - ((1 / float(pp)) - 1)))
      else:
        Tip = (((1 / float(q)) - 1) * (1 - ((1 - q) ** h))) - (h * ((1 - q) ** (h + 1)))
      Trp = h - Tpp - Tip
      
      ## Calculate Expected Utilities
      US = (payoffs[State.S] * Tss) + (payoffs[State.I] * Tis) + (payoffs[State.R] * Trs)
      UP = (payoffs[State.P] * Tpp) + (payoffs[State.I] * Tip) + (payoffs[State.R] * Trp)
      
      if (pUs == -1):
        pUs = US
        
      if (pUp == -1):
        pUp = UP
        
      if (iState == -1):
        if (US > UP):
          iState = 0
        else:
          iState = 1
        
      if (((US >= UP) and (pUs < pUp)) or ((UP >= US) and (pUp < pUs))):
        iswitch.append([pI, i, iState])
        
        if ((US >= UP) and (pUs < pUp)):
          iState = 0
        elif ((UP >= US) and (pUp < pUs)):
          iState = 1
        
        pI = i
        pUs = US
        pUp = UP
        
      i += step
    
    if (pI < 1):
      iswitch.append([pI, 1, iState])
    
    return iswitch
  