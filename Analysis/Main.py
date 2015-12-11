from mpl_toolkits.mplot3d import Axes3D
from matplotlib import pyplot as plt
import numpy as np
from SPIR.State import State

if __name__ == '__main__':
    pass

##
## Variables
##
payoffs = {State.S: 1, State.P: 0.99, State.I: 0, State.R: 1}
disease = {'b1': 0.1, 'b2': 0.01, 'g': 0.05}

T = 100
H = 100
I = 100

dataUS = np.zeros((H,I))
dataUP = np.zeros((H,I))
for h in range(1,H+1):
    for j in range(1,I+1):
        
        i = j / 100.0
        p = i * disease['b1']
        q = disease['g']
        Tss = (1 - ((1 - p)**h)) / p
        if (p != q):
            Tis = (1 / q) - (((p * ((1 - q)**h)) / (q * (p - q))) * (1 - (((1 - p) / (1 - q))**h))) - (((1 - p)**h) / q)
        else:
            Tis = (1 / q) - ((p * h * ((1 - q)**(h-1))) /q) - (((1 - p)**h) / q)
        Trs = h - Tss - Tis
            
        ## Calculate utility Prophylactic
        p = i * disease['b2']
        q = disease['g']
        Tpp = (1 - ((1 - p)**h)) / p
        if (p != q):
            Tip = (1 / q) - (((p * ((1 - q)**h)) / (q * (p - q))) * (1 - (((1 - p) / (1 - q))**h))) - (((1 - p)**h) / q)
        else:
            Tip = (1 / q) - ((p * h * ((1 - q)**(h-1))) /q) - (((1 - p)**h) / q)
        Trp = h - Tpp - Tip
            
        US = (payoffs[State.S] * Tss) + (payoffs[State.I] * Tis) + (payoffs[State.R] * Trs)
        UP = (payoffs[State.P] * Tpp) + (payoffs[State.I] * Tip) + (payoffs[State.R] * Trp)
        
        dataUS[h-1, j-1] = US
        dataUP[h-1, j-1] = UP

##
## 3D Visualization
##        
fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
X = np.arange(0, H, 1)
Y = np.arange(0, I, 1)
X, Y = np.meshgrid(X, Y, sparse=False)

##
## Susceptible
##
Z = dataUS
ax.plot_surface(X, Y, Z, color='b', linewidth=0.1, antialiased=False)

##
## Prophylactic
##
Z = dataUP
ax.plot_surface(X, Y, Z, color='r', linewidth=0.1, antialiased=False)

ax.set_xlabel('H')
ax.set_ylabel('I')
ax.set_zlabel('E()')

plt.show()
