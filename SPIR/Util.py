# #
# # Our classes
# #
from Constants import Constant
from State import State

class Util(object):
    # #
    # # Calculate i Switch
    # #
    @staticmethod
    def calcISwitch(timeHorizon, disease, payoffs):
        h = timeHorizon
        q = disease[Constant.GAMMA]
        
        found = False
        iswitch = 1
        pUs = -1
        pUp = -1
        step = 0.00001
        i = step
        while (i <= 1.0):
            # # Calculate expected times of Susceptible
            ps = i * disease[Constant.BETA]
            Tss = ((1 / float(ps)) - 1) * (1 - ((1 - ps) ** h))
            if (ps != q):
                Tis = ((1 / float(q)) - 1) * (((((1 / float(q)) - 1) * (1 - ((1 - q) ** h))) - (((1 / float(ps)) - 1) * (1 - ((1 - ps) ** h)))) / ((((1 / float(q)) - 1)) - ((1 / float(ps)) - 1)))
            else:
                Tis = (((1 / float(q)) - 1) * (1 - ((1 - q) ** h))) - (h * ((1 - q) ** (h + 1)))
            Trs = h - Tss - Tis
            
            # # Calculate expected times of Prophylactic
            pp = i * disease[Constant.BETA] * disease[Constant.RHO]
            Tpp = ((1 / float(pp)) - 1) * (1 - ((1 - pp) ** h))
            if (pp != q):
                Tip = ((1 / float(q)) - 1) * (((((1 / float(q)) - 1) * (1 - ((1 - q) ** h))) - (((1 / float(pp)) - 1) * (1 - ((1 - pp) ** h)))) / ((((1 / float(q)) - 1)) - ((1 / float(pp)) - 1)))
            else:
                Tip = (((1 / float(q)) - 1) * (1 - ((1 - q) ** h))) - (h * ((1 - q) ** (h + 1)))
            Trp = h - Tpp - Tip
            
            # # Calculate Expected Utilities
            US = (payoffs[State.S] * Tss) + (payoffs[State.I] * Tis) + (payoffs[State.R] * Trs)
            UP = (payoffs[State.P] * Tpp) + (payoffs[State.I] * Tip) + (payoffs[State.R] * Trp)
            
            if ((pUs == -1) or (pUp == -1)):
                iswitch = i
                pUs = US
                pUp = UP
                
            if (((US >= UP) and (pUs < pUp)) or ((UP >= US) and (pUp < pUs))):
                iswitch = i
                found = True
                break
            
            i += step
        
        if (not found):
            iswitch = 1
        
        return iswitch
