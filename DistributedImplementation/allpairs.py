from graph_tool.all import *
import graph_tool.topology as gt
import dill as pickle
import time
from collections import defaultdict
from lact_utilities import *
import sys
import multiprocessing


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

def gen_distance_matrix(orig_graph, new_graph):            
    dist = defaultdict(lambda:None)

    start_time = time.time()
    print start_time
    pickle_file = new_graph.split(".")[0]+".pk"
    myfile = open(pickle_file,'wb')
    u = utilities(orig_graph)
    g = u.G
    u.createGraph(new_graph)
    new_g = u.newG

    for e in g.edges():
        src = g.vertex_index[e.source()]
        dst = g.vertex_index[e.target()]
        v1=0
        v2=0
        if (src < pair):
            v1 = src
            v2 = dst
        else:
            v1 = dst
            v2 = src
        p = pair(v1,v2)
        if (dist[p]==None):
            dist[p] = gt.shortest_distance(new_g,source=new_g.vertex(src), target=new_g.vertex(dst))

    pickle.dump(dist,myfile)
    myfile.close()
    print ("it took "+ str(time.time()-start_time))
    
if __name__ == "__main__":
    orig_graph = sys.argv[1]
    new_graph = sys.argv[2]
    gen_distance_matrix(orig_graph,new_graph)
    input_graphs=["facebook_wosn_EPL_GREEDY_1_graph.txt","facebook_wosn_EPL_GREEDY_2_graph.txt","facebook_wosn_EPL_GREEDY_3_graph.txt"]
    for graph in input_graphs:
       p = multiprocessing.Process(target=gen_distance_matrix,args=(orig_graph,graph,))
       p.start()
