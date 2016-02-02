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
    METHOD = "method"
    REPLICATION = "replication"
    TIME_STEPS = "time.steps"
    OUTPUT_FILE = "output.file"
    OUTPUT_HEADER = "output.header"
    OUTPUT_SEP = "output.separator"
    
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
    M = "-M"
    R = "-R"
    T = "-T"
    O = "-O"
    P = "-P"
    S = "-S"
    
    ##
    ## Methods
    ##
    METHOD_NAIVE = 0
    METHOD_GILLESPIE = 1
    METHOD_MICRO = 2
    METHOD_GGILLESPIE = 3
    
    ##
    ## Output
    ##
    O_T = "time"
    O_S = "susceptible"
    O_P = "prophylactic"
    O_I = "infected"
    O_R = "recovered"
    