##
## Python libraries
##
import math
from numpy import arange, random
from random import uniform

##
## Our classes
##
from Util import Util
from Constants import Constant
from State import State

class EfficientTauLeapMethod(object):
    ##
    ## Interactions
    ##
    NUM_INTERACTIONS = 5
    INT_SP = 0  # Decision Susceptible to Prophylactic
    INT_PS = 1  # Decision Prophylactic to Susceptible
    INT_SI = 2  # Infection of a Susceptible
    INT_PI = 3  # Infection of a Prophylactic
    INT_IR = 4  # Recovery of an Infected
    
    INTERACTIONS = [INT_SP, INT_PS, INT_SI, INT_PI, INT_IR]
    
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
        
        r = [[State.S, State.P],
             [State.P, State.S],
             [State.S, State.I],
             [State.P, State.I],
             [State.I, State.R]]
        
        A = a[self.INT_SP] + a[self.INT_PS] + a[self.INT_SI] + a[self.INT_PI] + a[self.INT_IR]
        
        v = [[-1, 1, -1, 0, 0],
             [1, -1, 0, -1, 0],
             [0, 0, 1, 1, -1],
             [0, 0, 0, 0, 1]]
        
        nc = 10
        g = [2, 2, 2, 1]
        e = 0.03
        
        prevT = 0
        t = 0
        while((t < self.timeSteps) and (i > 0)):
            ##
            ## Identify critical and non-critical interactions
            ##
            Incr = [State.S, State.P, State.I, State.R]
            jc = []
            jnc = []
            for j in range(self.NUM_INTERACTIONS):
                l = 0
                first = True
                for s in State.STATES:
                    if(v[s][j] < 0):
                        aux = math.ceil(self.nAgents[s] / abs(v[s][j]))
                        if((aux < l) or (first)):
                            l = aux
                            first = False
                            
                if((l < nc) and (a[j] > 0)):
                    jc.append(j)
                    for reaction in r[j]:
                        if(reaction in Incr):
                            Incr.remove(reaction)
                else:
                    jnc.append(j)
                    
            ##
            ## Calculate tau'
            ##
            tau1 = math.inf
            if(len(jnc) > 0):
                u = [0 for s in State.STATES]
                std = [0 for s in State.STATES]
                for s in State.STATES:
                    for j in jnc:
                        aux = v[s][j] * a[j]
                        u[s] += aux
                        std[s] += aux * v[s][j]
                        
                for s in State.STATES:
                    if(s in Incr):
                        aux = max((e * self.nAgents[s]) / g[s], 1)
                        if(abs(u[s]) != 0):
                            term1 = aux / abs(u[s])
                        else:
                            term1 = math.inf
                            
                        aux = aux * aux
                        if(std[s] != 0):
                            term2 = aux / std[s]
                        else:
                            term2 = math.inf
                            
                        mintau = min(term1, term2)
                        if(mintau < tau1):
                            tau1 = mintau
                            
            ##
            ## Calculate tau''
            ##
            tau2 = math.inf
            Ac = 0
            for j in jc:
                Ac += a[j]
                
                if(Ac > 0):
                    tau2 = random.exponential(1 / Ac)
            
            if(tau1 < (10 / A)):
                count = 0
                while ((t < self.timeSteps) and (i > 0) and (count < 10)):
                    cumA = uniform(0.0, 1.0) * A
                    index = 0
                    x = a[index]
                    while(x <= cumA):
                        index += 1
                        x += a[index]
                        
                    if ((index == self.INT_SP) and (self.nAgents[State.S] > 0)):
                        ## Decide
                        #agent = S[int(uniform(0, self.nAgents[State.S] - 1))]
                        
                        #S.remove(agent)
                        #agent.setState(State.P)
                        #P.append(agent)
                        
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
                    count += 1
                    prevT = t
                    i = (self.nAgents[State.I] / float(N)) ** float(1 / float(self.fear))
                    
                    if (i > 0):
                        switch = 0
                        if (i > iSwitch):
                            switch = 1
                                            
                        a = {self.INT_SP: c[self.INT_SP] * self.nAgents[State.S] * switch,
                             self.INT_PS: c[self.INT_PS] * self.nAgents[State.P] * (1 - switch),
                             self.INT_SI: c[self.INT_SI] * self.nAgents[State.S] * self.nAgents[State.I],
                             self.INT_PI: c[self.INT_PI] * self.nAgents[State.P] * self.nAgents[State.I],
                             self.INT_IR: c[self.INT_IR] * self.nAgents[State.I]}
                        
                        A = a[self.INT_SP] + a[self.INT_PS] + a[self.INT_SI] + a[self.INT_PI] + a[self.INT_IR]
                        
                        t = t + (math.log(1 / uniform(0.0, 1.0)) / float(A))
            else:
                positive = False
                while(not positive):
                    k = [0 for k in range(self.NUM_INTERACTIONS)]
                    
                    if(tau1 < tau2):
                        tau = tau1
                        for j in range(self.NUM_INTERACTIONS):
                            if((j in jc) or (a[j] == 0)):
                                k[j] = 0
                            else:
                                k[j] = random.poisson(a[j] * tau)
                    else:
                        tau = tau2
                        
                        selJC = -1
                        if((len(jc) > 0) and (Ac > 0)):
                            selJC = random.choice(arange(0, len(jc)), p=[a[j] / Ac for j in jc])
                        
                        for j in range(self.NUM_INTERACTIONS):
                            if(j in jc):
                                if(j == jc[selJC]):
                                    k[j] = 1
                                else:
                                    k[j] = 0
                            else:
                                if(a[j] > 0):
                                    k[j] = random.poisson(a[j] * tau)
                                else:
                                    k[j] = 0
                                    
                    negative = False
                    for s in State.STATES:
                        value = self.nAgents[s]
                        for j in range(self.NUM_INTERACTIONS):
                            value += k[j] * v[s][j]
                        
                        if(value < 0):
                            negative = True
                            
                    if(negative):
                        tau1 = tau1 / 2
                        tau2 = tau2 / 2
                    else:
                        for s in State.STATES:
                            for j in range(self.NUM_INTERACTIONS):
                                self.nAgents[s] += k[j] * v[s][j]
                            
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
                        positive = True
                        prevT = t
                        i = (self.nAgents[State.I] / float(N)) ** float(1 / float(self.fear))
                        
                        if (i > 0):
                            switch = 0
                            if (i > iSwitch):
                                switch = 1
                                                
                            a = {self.INT_SP: c[self.INT_SP] * self.nAgents[State.S] * switch,
                                 self.INT_PS: c[self.INT_PS] * self.nAgents[State.P] * (1 - switch),
                                 self.INT_SI: c[self.INT_SI] * self.nAgents[State.S] * self.nAgents[State.I],
                                 self.INT_PI: c[self.INT_PI] * self.nAgents[State.P] * self.nAgents[State.I],
                                 self.INT_IR: c[self.INT_IR] * self.nAgents[State.I]}
                            
                            A = a[self.INT_SP] + a[self.INT_PS] + a[self.INT_SI] + a[self.INT_PI] + a[self.INT_IR]
                            
                            t += tau
        
        return num
