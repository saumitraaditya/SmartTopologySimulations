#! /usr/bin/python

import networkx as nx
from graph_tool.all import *
import numpy as np
from networkx.utils import powerlaw_sequence as pls

G = load_graph("nx_20_PL.graphml")
edge_cost = G.new_edge_property("double",0.0)


for v in G.vertices():
    counter = 0
    l = pls(v.out_degree(),2)
    for e in v.out_edges():
        if (edge_cost[e] < l[counter]):
            edge_cost[e] = l[counter]
        counter+=1
G.edge_properties["edge_cost"] = edge_cost
G.save("nx_20_PL.xml.gz")

# write into a file in the format src dst-cost dst-cost....
f = open("nx_20_PL.txt","w")
for v in G.vertices():
    line=""
    line+=str(G.vertex_index[v])
    for e in v.out_edges():
        dst = str(G.vertex_index[e.target()])
        cost = str(edge_cost[e])
        line+=(" "+dst+"-"+cost)
    line+="\n"
    f.write(line)
f.close()
