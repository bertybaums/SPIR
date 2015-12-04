from random import uniform
from SPIR.State import State

class Agent(object):
    
    ##
    ## Constructor
    ##
    def __init__(self, num, state, disease):
        self.num = num
        self.state = state
        self.disease = disease
        
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
    ## Interact
    ##
    def interact(self, partner):
        if (partner.getState() == State.I):
            if (self.state == State.S):
                if (uniform(0.0,1.0) < self.disease['b1']):
                    self.state = State.I
            elif (self.state == State.P):
                if (uniform(0.0,1.0) < self.disease['b2']):
                    self.state = State.I
        return self.state
    
    ##
    ## Decide between Susceptible and Prophylactic
    ##
    def decide(self, payoffs, i, h):
        if (((self.state == State.S) or (self.state == State.P)) and (i > 0)):
            ## Calculate utility Susceptible
            p = i * self.disease['b1']
            q = self.disease['g']
            Tss = (1 - ((1 - p)**h)) / p
            if (p != q):
                Tis = (1 / q) - (((p * ((1 - q)**h)) / (q * (p - q))) * (1 - (((1 - p) / (1 - q))**h))) - (((1 - p)**h) / q)
            else:
                Tis = (1 / q) - ((p * h * ((1 - q)**(h-1))) /q) - (((1 - p)**h) / q)
            Trs = h - Tss - Tis
            
            ## Calculate utility Prophylactic
            p = i * self.disease['b2']
            q = self.disease['g']
            Tpp = (1 - ((1 - p)**h)) / p
            if (p != q):
                Tip = (1 / q) - (((p * ((1 - q)**h)) / (q * (p - q))) * (1 - (((1 - p) / (1 - q))**h))) - (((1 - p)**h) / q)
            else:
                Tip = (1 / q) - ((p * h * ((1 - q)**(h-1))) /q) - (((1 - p)**h) / q)
            Trp = h - Tpp - Tip
            
            US = (payoffs[State.S] * Tss) + (payoffs[State.I] * Tis) + (payoffs[State.R] * Trs)
            UP = (payoffs[State.P] * Tpp) + (payoffs[State.I] * Tip) + (payoffs[State.R] * Trp)
            
            #print(payoffs[State.S], " ", Tss, " ", payoffs[State.I], " ",Tis, " ", payoffs[State.R], " ", Trs, " ", payoffs[State.P], " ", Tpp, " ", payoffs[State.I], " ",Tip, " ", payoffs[State.R], " ", Trp)
            
            if (US >= UP):
                self.state = State.S
            else:
                self.state = State.P
        return self.state
    
    ##
    ## Recover
    ##
    def recover(self):
        if ((self.state == State.I) and (uniform(0.0,1.0) < self.disease['g'])):
            self.state = State.R
        return self.state
    