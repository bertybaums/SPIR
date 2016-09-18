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
    BETA = "beta"
    RHO = "rho"
    GAMMA = "gamma"
    FEAR = "fear"
    DECISION = "decision"
    TIME_HORIZON = "time.horizon"
    METHOD = "method"
    REPLICATION = "replication"
    TIME_STEPS = "time.steps"
    OUTPUT_FORMAT = "output.format"
    OUTPUT_WINDOW = "output.window"
    OUTPUT_PATH = "output.path"
    OUTPUT_FILE = "output.file"
    OUTPUT_HEADER = "output.header"
    OUTPUT_SEP = "output.separator"
    
    ##
    ## Command line parameters
    ##
    FILE = "filename"
    NS = "-NS"
    NP = "-NP"
    NI = "-NI"
    NR = "-NR"
    PS = "-PS"
    PP = "-PP"
    PI = "-PI"
    PR = "-PR"
    BS = "-BS"
    RH = "-RH"
    G = "-G"
    K = "-K"
    D = "-D"
    H = "-H"
    M = "-M"
    R = "-R"
    T = "-T"
    W = "-W"
    F = "-F"
    P = "-P"
    N = "-N"
    O = "-O"
    S = "-S"
    
    ##
    ## Methods
    ##
    METHOD_MICRO = 0
    METHOD_GILLESPIE = 1
    
    ##
    ## Output format
    ##
    O_STANDARD = 0
    O_GALAPAGOS = 1
    
    ##
    ## Output
    ##
    O_X = "replication"
    O_T = "time"
    O_S = "susceptible"
    O_P = "prophylactic"
    O_I = "infected"
    O_R = "recovered"
    O_PS = "payoff_susceptible"
    O_PP = "payoff_prophylactic"
    O_PI = "payoff_infectious"
    O_PR = "payoff_recovered"