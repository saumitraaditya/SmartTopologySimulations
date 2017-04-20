from graph_tool.all import *
import graph_tool.topology as gt
import dill as pickle
import time
from collections import defaultdict
from lact_utilities import *
import sys


class pair(object):
    def __init__(self,src,dst):
        self.src = src
        self.dst = dst
        
    def __hash__(self):
        return hash(31*self.src+self.dst)
        
    def __eq__(self,other):
        if not isinstance(other,pair):
            return False
        elif (self.src == other.src and self.dst == other.dst):
            return True
        else:
            return False
            
G = load_graph(sys.argv[1])
infile = open(sys.argv[2],'rb')
dist = pickle.load(infile)
infile.close()
HCD = []

v_list = [v for v in G.vertices()]
v_indexList = [G.vertex_index[v] for v in v_list]

for v_index in v_indexList:
    n_list = [G.vertex_index[n] for n in G.vertex(v_index).out_neighbours()]
    hop_sum = 0
    for n in n_list:
        if (v_index <= n):
            v1 = v_index
            v2 = n
        else:
            v1 = n
            v2 = v_index
        try :
            hop_sum+=dist[pair(v1,v2)]
        except Exception as ex:
            template = "An exception of type {0} occured. Arguments:\n{1!r}"
            message = template.format(type(ex).__name__, ex.args)
            print message
            print(dist[pair(int(v1),int(v2))])
            print("src "+str(v1)+" dst "+str(v2))
            print("src "+str(type(v1))+" dst "+str(type(v2)))
    avg_hop_sum = float(hop_sum) / len(n_list)
    HCD.append(avg_hop_sum)

hop_dist = [0,0,0,0,0]
num_vertices = G.num_vertices()
for hc in HCD:
    if (hc <=1):
        hop_dist[0]+=1
    elif (hc >1 and hc <=2):
        hop_dist[1]+=1
    elif (hc > 2 and hc<=3):
        hop_dist[2]+=1
    elif (hc>3 and hc<=4):
        hop_dist[3]+=1
    else:
        hop_dist[4]+=1    

for i in range(0,len(hop_dist)):
    hop_dist[i] = (float(hop_dist[i])/num_vertices)*100

print ("hop count distribution")
print("one_hop "+str(hop_dist[0]))
print ("two_hop " + str(hop_dist[1]))
print ("three_hop " + str(hop_dist[2]))
print ("four_hop " + str(hop_dist[3]))
print ("gt_four " + str(hop_dist[4]))
