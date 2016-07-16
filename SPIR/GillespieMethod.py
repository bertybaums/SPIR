##
## Python libraries
##
import math
from random import uniform

##
## Our classes
##
from SPIR.Agent import Agent
from SPIR.Constants import Constant
from SPIR.State import State

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
    ## Calculate i Switch
    ##
    def calcISwitch(self, timeHorizon, disease, payoffs):
        h = timeHorizon
        q = disease[Constant.GAMMA]
        
        iswitch = 1
        pUs = -1
        pUp = -1
        step = 0.00001
        i = step
        while (i <= 1.0):
            ## Calculate expected times of Susceptible
            ps = i * disease[Constant.BETA]
            Tss = ((1 / float(ps)) - 1) * (1 - ((1 - ps)**h))
            if (ps != q):
                Tis = ((1 / float(q)) - 1) * (((((1 / float(q)) - 1) * (1 - ((1 - q)**h))) - (((1 / float(ps)) - 1) * (1 - ((1 - ps)**h)))) / ((((1 / float(q)) - 1)) - ((1 / float(ps)) - 1)))
            else:
                Tis = (((1 / float(q)) - 1) * (1 - ((1 - q)**h))) - (h * ((1 - q)**(h + 1)))
            Trs = h - Tss - Tis
            
            ## Calculate expected times of Prophylactic
            pp = i * disease[Constant.BETA] * disease[Constant.RHO]
            Tpp = ((1 / float(pp)) - 1) * (1 - ((1 - pp)**h))
            if (pp != q):
                Tip = ((1 / float(q)) - 1) * (((((1 / float(q)) - 1) * (1 - ((1 - q)**h))) - (((1 / float(pp)) - 1) * (1 - ((1 - pp)**h)))) / ((((1 / float(q)) - 1)) - ((1 / float(pp)) - 1)))
            else:
                Tip = (((1 / float(q)) - 1) * (1 - ((1 - q)**h))) - (h * ((1 - q)**(h + 1)))
            Trp = h - Tpp - Tip
            
            ## Calculate Expected Utilities
            US = (payoffs[State.S] * Tss) + (payoffs[State.I] * Tis) + (payoffs[State.R] * Trs)
            UP = (payoffs[State.P] * Tpp) + (payoffs[State.I] * Tip) + (payoffs[State.R] * Trp)
            
            if (pUs == -1):
                pUs = US
            
            if (pUp == -1):
                pUp = UP
                            
            if (((US >= UP) and (pUs < pUp)) or ((UP >= US) and (pUp < pUs))):
                iswitch = i
                break
            
            i += step
        
        return iswitch
    
    ##
    ## Execute the simulation
    ##
    def execute(self):
        ##
        ## Initialize agents
        ##
        pDisease = {Constant.BETA: 1 - math.exp(-self.disease[Constant.BETA]),
                    Constant.RHO: self.disease[Constant.RHO],
                    Constant.GAMMA: 1 - math.exp(-self.disease[Constant.GAMMA])}
        
        agents = []
        S = []
        P = []
        I = []
        R = []
        N = 0
        for state in self.nAgents:
            for x in range(self.nAgents[state]):
                agent = Agent(N, state, pDisease, self.fear, self.timeHorizon, self.payoffs)
                agents.append(agent)
                
                if (state == State.S):
                    S.append(agent)
                elif (state == State.P):
                    P.append(agent)
                elif (state == State.I):
                    I.append(agent)
                elif (state == State.R):
                    R.append(agent)
                    
                N += 1
        
        ##
        ## Output variables
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
                    0])
        
        ##
        ## Run the simulation
        ##
        i = (self.nAgents[State.I] / float(N)) ** float(1 / float(self.fear))
        
        iSwitch = self.calcISwitch(self.timeHorizon, pDisease, self.payoffs)
        
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
        
        t = math.log(1 / uniform(0.0, 1.0)) / A
        while ((t < self.timeSteps) and (i > 0)):            
            cumA = uniform(0.0, 1.0) * A
            index = 0
            x = a[index]
            while(x <= cumA):
                index += 1
                x += a[index]
                
            if ((index == self.INT_SP) and (self.nAgents[State.S] > 0)):
                ## Decide
                agent = S[int(uniform(0, self.nAgents[State.S] - 1))]
                
                S.remove(agent)
                self.nAgents[State.S] -= 1
                
                agent.setState(State.P)
                
                P.append(agent)
                self.nAgents[State.P] += 1
                    
            elif ((index == self.INT_PS) and (self.nAgents[State.P] > 0)):
                ## Decide
                agent = P[int(uniform(0, self.nAgents[State.P] - 1))]
                
                P.remove(agent)
                self.nAgents[State.P] -= 1
                
                agent.setState(State.S)
                
                S.append(agent)
                self.nAgents[State.S] += 1
        
            elif ((index == self.INT_SI) and (self.nAgents[State.S] > 0)):
                ## Infect
                agent = S[int(uniform(0, self.nAgents[State.S] - 1))]

                S.remove(agent)
                self.nAgents[State.S] -= 1
                
                agent.setState(State.I)
                
                I.append(agent)
                self.nAgents[State.I] += 1
                    
            elif ((index == self.INT_PI) and (self.nAgents[State.P] > 0)):
                ## Infect
                agent = P[int(uniform(0, self.nAgents[State.P] - 1))]
                
                P.remove(agent)
                self.nAgents[State.P] -= 1
                
                agent.setState(State.I)
                    
                I.append(agent)
                self.nAgents[State.I] += 1
                    
            elif ((index == self.INT_IR) and (self.nAgents[State.I] > 0)):
                ## Recover
                agent = I[int(uniform(0, self.nAgents[State.I] - 1))]

                I.remove(agent)
                self.nAgents[State.I] -= 1
                
                agent.setState(R)
                
                R.append(agent)
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
                    0])
            
            ##
            ## Update
            ##
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
                
                t = t + (math.log(1 / uniform(0.0, 1.0)) / A)
                
        return num
