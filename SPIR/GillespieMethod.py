##
## Python libraries
##
import math
from random import uniform

##
## Our classes
##
from Util import Util
from Constants import Constant
from State import State

class GillespieMethod(object):
    ##
    ## Interactions
    ##
    INT_SP = 0  # Decision Susceptible to Prophylactic
    INT_PS = 1  # Decision Prophylactic to Susceptible
    INT_SI = 2  # Infection of a Susceptible
    INT_PI = 3  # Infection of a Prophylactic
    INT_IR = 4  # Recovery of an Infected
    
    ##
    ## Constructor
    ##
    def __init__(self, args, nAgents, payoffs, disease, fear, decision, timeHorizon, timeSteps):
        self.args = args
        self.nAgents = nAgents
        self.payoffs = payoffs
        self.disease = disease
        
        self.fear = fear
        if (self.fear == 0):
            self.fear = 1.0
            
        self.decision = decision
        self.timeHorizon = timeHorizon
        self.timeSteps = timeSteps
    
    ##
    ## Execute the simulation
    ##
    def execute(self):
        ##
        ## Initialize agents
        ##
        pDisease = {Constant.BETA: 1 - math.exp(-self.disease[Constant.BETA]),
                    Constant.RHO: 1 - math.exp(-self.disease[Constant.RHO]),
                    Constant.GAMMA: 1 - math.exp(-self.disease[Constant.GAMMA])}
        
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
        
        ##
        ## Run the simulation
        ##
        N = self.nAgents[State.S] + self.nAgents[State.P] + self.nAgents[State.I] + self.nAgents[State.R]
        
        i = (self.nAgents[State.I] / float(N)) ** float(1 / float(self.fear))
        
        iSwitch = Util.calcISwitch(self.timeHorizon, pDisease, self.payoffs)
        
        c = {self.INT_SP: self.decision / float(N),
             self.INT_PS: self.decision / float(N),
             self.INT_SI: self.disease[Constant.BETA] / (float(N) * float(N)),
             self.INT_PI: (self.disease[Constant.BETA] * self.disease[Constant.RHO]) / (float(N) * float(N)),
             self.INT_IR: self.disease[Constant.GAMMA] / float(N)}
        
        a = {self.INT_SP: c[self.INT_SP] * self.nAgents[State.S],
             self.INT_PS: c[self.INT_PS] * self.nAgents[State.P],
             self.INT_SI: c[self.INT_SI] * self.nAgents[State.S] * self.nAgents[State.I],
             self.INT_PI: c[self.INT_PI] * self.nAgents[State.P] * self.nAgents[State.I],
             self.INT_IR: c[self.INT_IR] * self.nAgents[State.I]}
                
        A = a[self.INT_SP] + a[self.INT_PS] + a[self.INT_SI] + a[self.INT_PI] + a[self.INT_IR]
        
        prevT = 0
        t = math.log(1 / uniform(0.0, 1.0)) / float(A)
        while ((t < self.timeSteps) and (i > 0)):            
            cumA = uniform(0.0, 1.0) * A
            index = 0
            x = a[index]
            while(x <= cumA):
                index += 1
                x += a[index]
                
            if ((index == self.INT_SP) and (self.nAgents[State.S] > 0)):
                ## Decide
                
                self.nAgents[State.S] -= 1
                self.nAgents[State.P] += 1
                    
            elif ((index == self.INT_PS) and (self.nAgents[State.P] > 0)):
                ## Decide
                
                self.nAgents[State.P] -= 1
                self.nAgents[State.S] += 1
                
            elif ((index == self.INT_SI) and (self.nAgents[State.S] > 0)):
                ## Infect
                
                self.nAgents[State.S] -= 1
                self.nAgents[State.I] += 1
                
            elif ((index == self.INT_PI) and (self.nAgents[State.P] > 0)):
                ## Infect
                
                self.nAgents[State.P] -= 1
                self.nAgents[State.I] += 1
                
            elif ((index == self.INT_IR) and (self.nAgents[State.I] > 0)):
                ## Recover
                
                self.nAgents[State.I] -= 1
                self.nAgents[State.R] += 1
                
            ##
            ## Update output
            ##
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
            
            ##
            ## Update
            ##
            prevT = t
            i = (self.nAgents[State.I] / float(N)) ** float(1 / float(self.fear))
            
            if (i > 0):
                s = 0
                if (i > iSwitch):
                    s = 1
                                    
                a = {self.INT_SP: c[self.INT_SP] * self.nAgents[State.S] * s,
                     self.INT_PS: c[self.INT_PS] * self.nAgents[State.P] * (1 - s),
                     self.INT_SI: c[self.INT_SI] * self.nAgents[State.S] * self.nAgents[State.I],
                     self.INT_PI: c[self.INT_PI] * self.nAgents[State.P] * self.nAgents[State.I],
                     self.INT_IR: c[self.INT_IR] * self.nAgents[State.I]}
                
                A = a[self.INT_SP] + a[self.INT_PS] + a[self.INT_SI] + a[self.INT_PI] + a[self.INT_IR]
                
                t = t + (math.log(1 / uniform(0.0, 1.0)) / float(A))
                
        return num
