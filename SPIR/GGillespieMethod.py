##
## Python libraries
##
import math
from random import uniform

##
## Our classes
##
from Agent import Agent
from Constants import Constant
from State import State

class GillespieMethod(object):
    
    ##
    ## Constructor
    ##
    def __init__(self, args, nAgents, payoffs, disease, decisionProb, timeHorizon, timeSteps):
        self.args = args
        self.nAgents = nAgents
        self.payoffs = payoffs
        self.disease = disease
        self.decisionProb = decisionProb
        self.timeHorizon = timeHorizon
        self.timeSteps = timeSteps
        
    ##
    ## Execute the simulation
    ##
    def execute(self):
        ##
        ## Initialize agents
        ##
        pDisease = {Constant.BETA_S: 1 - math.exp(-self.disease[Constant.BETA_S]),
                    Constant.BETA_P: 1 - math.exp(-self.disease[Constant.BETA_P]),
                    Constant.GAMMA: 1 - math.exp(-self.disease[Constant.GAMMA])}
        
        agents = []
        S = []
        P = []
        I = []
        R = []
        N = 0
        for state in self.nAgents:
            for i in range(self.nAgents[state]):
                agent = Agent(N, state, pDisease, self.timeHorizon, self.payoffs)
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
                    self.nAgents[State.I],
                    self.nAgents[State.R]])
        
        ##
        ## Run the simulation
        ##
        i = self.nAgents[State.I] / float(N)
        
        c = {Constant.INT_SP: self.decisionProb / float(N),
             Constant.INT_PS: self.decisionProb / float(N),
             Constant.INT_SI: self.disease[Constant.BETA_S] / (float(N) * float(N)),
             Constant.INT_PI: self.disease[Constant.BETA_P] / (float(N) * float(N)),
             Constant.INT_IR: self.disease[Constant.GAMMA] / float(N)}
        
        a = {Constant.INT_SP: c[Constant.INT_SP] * self.nAgents[State.S],
             Constant.INT_PS: c[Constant.INT_PS] * self.nAgents[State.P],
             Constant.INT_SI: c[Constant.INT_SI] * self.nAgents[State.S] * self.nAgents[State.I],
             Constant.INT_PI: c[Constant.INT_PI] * self.nAgents[State.P] * self.nAgents[State.I],
             Constant.INT_IR: c[Constant.INT_IR] * self.nAgents[State.I]}
                
        A = a[Constant.INT_SP] + a[Constant.INT_PS] + a[Constant.INT_SI] + a[Constant.INT_PI] + a[Constant.INT_IR]
        
        t = math.log(1 / uniform(0.0, 1.0)) / A
        count = 0
        while ((t < self.timeSteps) and (i > 0)):
            if (self.args.verbose):
                print 'Timestep:', str(t)
            
            cumA = uniform(0.0, 1.0) * A
            index = 0
            x = a[index]
            while(x <= cumA):
                index = index + 1
                x = x + a[index]
                
            if ((index == Constant.INT_SP) and (self.nAgents[State.S] > 0)):
                ## Decide
                agent = S[int(uniform(0, self.nAgents[State.S] - 1))]
                
                S.remove(agent)
                self.nAgents[State.S] -= 1
                
                agent.setState(State.P)
                
                P.append(agent)
                self.nAgents[State.P] += 1
                    
            elif ((index == Constant.INT_PS) and (self.nAgents[State.P] > 0)):
                ## Decide
                agent = P[int(uniform(0, self.nAgents[State.P] - 1))]
                
                P.remove(agent)
                self.nAgents[State.P] -= 1
                
                agent.setState(State.S)
                
                S.append(agent)
                self.nAgents[State.S] += 1
        
            elif ((index == Constant.INT_SI) and (self.nAgents[State.S] > 0)):
                ## Infect
                agent = S[int(uniform(0, self.nAgents[State.S] - 1))]

                S.remove(agent)
                self.nAgents[State.S] -= 1
                
                agent.setState(State.I)
                
                I.append(agent)
                self.nAgents[State.I] += 1
                    
            elif ((index == Constant.INT_PI) and (self.nAgents[State.P] > 0)):
                ## Infect
                agent = P[int(uniform(0, self.nAgents[State.P] - 1))]
                
                P.remove(agent)
                self.nAgents[State.P] -= 1
                
                agent.setState(State.I)
                    
                I.append(agent)
                self.nAgents[State.I] += 1
                    
            elif ((index == Constant.INT_IR) and (self.nAgents[State.I] > 0)):
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
                    self.nAgents[State.I],
                    self.nAgents[State.R]])
            
            ##
            ## Update
            ##
            i = self.nAgents[State.I] / float(N)
            
            if (i > 0):
                a = {Constant.INT_SP: c[Constant.INT_SP] * self.nAgents[State.S],
                     Constant.INT_PS: c[Constant.INT_PS] * self.nAgents[State.P],
                     Constant.INT_SI: c[Constant.INT_SI] * self.nAgents[State.S] * self.nAgents[State.I],
                     Constant.INT_PI: c[Constant.INT_PI] * self.nAgents[State.P] * self.nAgents[State.I],
                     Constant.INT_IR: c[Constant.INT_IR] * self.nAgents[State.I]}
                
                A = a[Constant.INT_SP] + a[Constant.INT_PS] + a[Constant.INT_SI] + a[Constant.INT_PI] + a[Constant.INT_IR]
                
                t = t + (math.log(1 / uniform(0.0, 1.0)) / A)
                
                count += 1
        print count
        return num
