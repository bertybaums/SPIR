class Constant():
    ##
    ## Configuration file fields
    ##
    NUM_AGENTS_S = "num.agents.S"
    NUM_AGENTS_P = "num.agents.P"
    NUM_AGENTS_I = "num.agents.I"
    NUM_AGENTS_R = "num.agents.R"
    PAYOFF_S = "payoff.S"
    PAYOFF_P = "payoff.P"
    PAYOFF_I = "payoff.I"
    PAYOFF_R = "payoff.R"
    BETA_S = "beta.S"
    BETA_P = "beta.P"
    GAMMA = "gamma"
    DECISION = "decision"
    TIME_HORIZON = "time.horizon"
    TIME_STEPS = "time.steps"
    OUTPUT_FILE = "output.file"
    OUTPUT_HEADER = "output.header"
    OUTPUT_SEP = "output.separator"
    
    ##
    ## Interactions
    ##
    INT_SP = 0  # Decision Susceptible to Prophylactic
    INT_PS = 1  # Decision Prophylactic to Susceptible
    INT_SI = 2  # Infection of a Susceptible
    INT_PI = 3  # Infection of a Prophylactic
    INT_IR = 4  # Recovery of an Infected
    
    ##
    ## Command line parameters
    ##
    F = "filename"
    NS = "-NS"
    NP = "-NP"
    NI = "-NI"
    NR = "-NR"
    PS = "-PS"
    PP = "-PP"
    PI = "-PI"
    PR = "-PR"
    BS = "-BS"
    BP = "-BP"
    G = "-G"
    D = "-D"
    H = "-H"
    T = "-T"
    O = "-O"
    P = "-P"
    S = "-S"
    
    ##
    ## Output
    ##
    O_T = "time"
    O_S = "susceptible"
    O_P = "prophylactic"
    O_I = "infected"
    O_R = "recovered"
    