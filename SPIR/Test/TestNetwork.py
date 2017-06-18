import matplotlib.pyplot as plt
import networkx as nx

from Utils.Util import Util

if __name__ == '__main__':
  
  cNet = Util.createNetwork(10, "smallworld 2 0.5")
  
  print(cNet.nodes())
  
  print(cNet.neighbors(1))
  
  nx.draw_networkx(cNet)
  
  plt.show()