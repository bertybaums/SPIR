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

class MicroMethod(object):
    
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
                    Constant.RHO: self.disease[Constant.RHO],
                    Constant.GAMMA: 1 - math.exp(-self.disease[Constant.GAMMA])}
        
        self.decision = 1 - math.exp(-self.decision)
        
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
                    self.nAgents[State.I],
                    self.nAgents[State.R]])
        
        numagents = [self.nAgents[State.S],
                    self.nAgents[State.P],
                    self.nAgents[State.I],
                    self.nAgents[State.R]]
        
        totagents = [self.nAgents[State.S],
                    self.nAgents[State.P],
                    self.nAgents[State.I],
                    self.nAgents[State.R]]
        
        ##
        ## Run the simulation
        ##
        cycle = 1
        elapsed = 1
        t = 1
        i = self.nAgents[State.I] / float(N)
        while ((t < self.timeSteps) and (i > 0)):
            n = 4
            target = [0, 0, 0, 0]
            while (n > 0):
                agent = int(uniform(0, len(agents) - 1))
                if (agent not in target):
                    target[n - 1] = agent
                    n -= 1
            
            ##
            ## Interaction
            ##
            agent = agents[target[0]]
            numagents[agent.getState()] -= 1
                        
            state = agent.interact(agents[target[1]].getState())
            numagents[state] += 1
            
            ##
            ## Decision
            ##
            agent = agents[target[2]]
            
            if (uniform(0.0, 1.0) < self.decision):
                numagents[agent.getState()] -= 1
                
                state = agent.decide(i)
                numagents[state] += 1
            
            ##
            ## Recover
            ##
            agent = agents[target[3]]
            numagents[agent.getState()] -= 1
            
            state = agent.recover()
            numagents[state] += 1
            
            if (elapsed < N):
                totagents = [totagents[State.S] + numagents[State.S],
                             totagents[State.P] + numagents[State.P],
                             totagents[State.I] + numagents[State.I],
                             totagents[State.R] + numagents[State.R]]
                elapsed += 1
            else:
                num.append([cycle,
                            totagents[State.S] / float(N),
                            totagents[State.P] / float(N),
                            0,
                            totagents[State.I] / float(N),
                            0,
                            0,
                            totagents[State.R] / float(N),
                            0,
                            0])
                totagents = [0, 0, 0, 0]
                cycle += 1
                elapsed = 0
            
            i = numagents[State.I] / float(N)
            t += 1
            
        if (elapsed < N):
            num.append([cycle,
                            totagents[State.S] / float(elapsed),
                            totagents[State.P] / float(elapsed),
                            0,
                            totagents[State.I] / float(elapsed),
                            0,
                            0,
                            totagents[State.R] / float(elapsed),
                            0,
                            0])
        
        return num
    