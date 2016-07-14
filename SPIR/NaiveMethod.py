##
## Python libraries
##
import math
from random import shuffle, uniform

##
## Our classes
##
from SPIR.Agent import Agent
from SPIR.Constants import Constant
from SPIR.State import State

class NaiveMethod(object):

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
        
        
    def execute(self):        
        ##
        ## Initialize agents
        ##
        pDisease = {Constant.BETA: 1 - math.exp(-self.disease[Constant.BETA]),
                    Constant.RHO: self.disease[Constant.RHO],
                    Constant.GAMMA: 1 - math.exp(-self.disease[Constant.GAMMA])}
        
        self.decision = 1 - math.exp(-self.decision)
                
        N = 0
        agents = []
        infected = []
        for state in self.nAgents:            
            for x in range(self.nAgents[state]):
                agent = Agent(N, state, pDisease, self.fear, self.timeHorizon, self.payoffs)
                agents.append(agent)
                
                if (state == State.I):
                    infected.append(agent)
                
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
        t = 1
        i = self.nAgents[State.I] / float(N)
        
        while ((t < self.timeSteps) and (i > 0)):
            numagents = [0, 0, 0, 0]
            
            ##
            ## Interaction
            ##
            shuffle(agents)
            
            n = N
            infected = []
            while(n > 1):
                a1 = agents[n - 1]
                a2 = agents[n - 2]
                
                a1State = a1.getState()
                a2State = a2.getState()
                
                a1S = a1State
                a2S = a2State
                
                if (a1State == State.I):
                    infected.append(a1)
                    a2S = a2.interact(a1State)
                    
                if (a2State == State.I):
                    infected.append(a2)
                    a1S = a1.interact(a2State)
                
                numagents[a1S] += 1
                numagents[a2S] += 1
                
                n = n - 2
            
            ##
            ## Decision
            ##
            for agent in agents:
                if (uniform(0.0, 1.0) < self.decision):
                    
                    state = agent.getState()
                    numagents[state] -= 1
                    
                    state = agent.decide(i)
                    numagents[state] += 1
            
            ##
            ## Recover
            ##
            for agent in infected:
                if (agent.recover() == State.R):
                    numagents[State.I] -= 1
                    numagents[State.R] += 1
            
            num.append([t,
                    numagents[State.S],
                    numagents[State.P],
                    0,
                    numagents[State.I],
                    0,
                    0,
                    numagents[State.R],
                    0,
                    0])
            
            i = numagents[State.I] / float(N)
            t += 1
        
        return num
