##
## Python libraries
##
from random import uniform

##
## Our classes
##
from Constants import Constant
from State import State

class Agent(object):
        
    ##
    ## Constructor
    ##
    def __init__(self, num, state, disease, fear, timeHorizon, payoffs):
        self.num = num
        self.state = state
        self.disease = disease
        self.fear = fear
        self.timeHorizon = timeHorizon
        self.payoffs = payoffs
        
    ##
    ## Get agent identification
    ##
    def getID(self):
        return self.num
    
    ##
    ## Get agent state
    ##
    def getState(self):
        return self.state
    
    ##
    ## Get agent disease information
    ##
    def getDisease(self):
        return self.disease
    
    ##
    ## Get fear effect
    ##
    def getFear(self):
        return self.fear
    
    ##
    ## Get time horizon
    ##
    def getTimeHorizon(self):
        return self.timeHorizon
    
    ##
    ## Get agent payoffs
    ##
    def getPayoffs(self):
        return self.payoffs
    
    ##
    ## Set agent state
    ##
    def setState(self, state):
        self.state = state
    
    ##
    ## Set agent disease information
    ##
    def setDisease(self, disease):
        self.disease = disease
        
    ##
    ## set fear effect
    ##
    def setFear(self, fear):
        self.fear = fear
    
    ##
    ## Set time horizon
    ##
    def setTimeHorizon(self, timehorizon):
        self.timeHorizon = timehorizon
    
    ##
    ## Set agent payoffs
    ##
    def setPayoffs(self, payoffs):
        self.payoffs = payoffs
        
    ##
    ## Calculate i switch
    ##
    def calcUtilities(self, i):
        US = 0
        UP = 0
        if (i > 0):
            h = self.timeHorizon
            q = self.disease[Constant.GAMMA]
                
            ## Calculate expected times of Susceptible
            ps = i * self.disease[Constant.BETA]
            Tss = (1 - ((1 - ps)**h)) / ps
            if (ps != q):
                Tis = (1 / q) - (((ps * ((1 - q)**h)) / (q * (ps - q))) * (1 - (((1 - ps) / (1 - q))**h))) - (((1 - ps)**h) / q)
            else:
                Tis = (1 / q) - ((ps * h * ((1 - q)**(h-1))) / q) - (((1 - ps)**h) / q)
            Trs = h - Tss - Tis
                
            ## Calculate expected times of Prophylactic
            pp = i * self.disease[Constant.BETA] * self.disease[Constant.RHO]
            Tpp = (1 - ((1 - pp)**h)) / pp
            if (pp != q):
                Tip = (1 / q) - (((pp * ((1 - q)**h)) / (q * (pp - q))) * (1 - (((1 - pp) / (1 - q))**h))) - (((1 - pp)**h) / q)
            else:
                Tip = (1 / q) - ((pp * h * ((1 - q)**(h-1))) / q) - (((1 - pp)**h) / q)
            Trp = h - Tpp - Tip
                
            ## Calculate Expected Utilities
            US = (self.payoffs[State.S] * Tss) + (self.payoffs[State.I] * Tis) + (self.payoffs[State.R] * Trs)
            UP = (self.payoffs[State.P] * Tpp) + (self.payoffs[State.I] * Tip) + (self.payoffs[State.R] * Trp)
        
        return [US, UP]
        
    ##
    ## Interact
    ##
    def interact(self, state):
        if (state == State.I):
            if (self.state == State.S):
                if (uniform(0.0,1.0) < self.disease[Constant.BETA]):
                    self.state = State.I
            elif (self.state == State.P):
                if (uniform(0.0,1.0) < (self.disease[Constant.BETA] * self.disease[Constant.RHO])):
                    self.state = State.I
        return self.state
    
    ##
    ## Decide between Susceptible and Prophylactic
    ##
    def decide(self, i):
        if (self.fear != 0):
            adjustedI = i ** float(1 / float(self.fear))
        
        if (((self.state == State.S) or (self.state == State.P)) and (i > 0)):
            
            U = self.calcUtilities(adjustedI)
            
            if (U[0] >= U[1]):
                self.state = State.S
            else:
                self.state = State.P
            
        return self.state
    
    ##
    ## Recover
    ##
    def recover(self):
        if ((self.state == State.I) and (uniform(0.0,1.0) < self.disease[Constant.GAMMA])):
            self.state = State.R
        return self.state
    