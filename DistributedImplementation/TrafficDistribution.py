from lact_utilities import *
import pickle


def getKey(vertex):
        return vertex.out_degree()
        
G = load_graph("facebook_wosn_PL.xml.gz")
GG = load_graph("processed_graph.xml.gz")
edge_cost = G.edge_properties["edge_cost"]

# keep track of traffic 


total_traffic = 0
hop1_traffic = 0
hop2_traffic = 0
hop3_traffic = 0
hop4_traffic = 0


for e in G.edges():
    src = G.vertex_index[e.source()]
    dst = G.vertex_index[e.target()]
    edge_traffic = edge_cost[e]
    hop_dist = graph_tool.topology.shortest_distance(GG, source=GG.vertex(src), target=GG.vertex(dst))
    if (hop_dist == 1):
            hop1_traffic+= edge_traffic
    elif (hop_dist == 2):
            hop2_traffic+= edge_traffic
    elif (hop_dist == 3):
            hop3_traffic+= edge_traffic
    else:
            hop4_traffic+=edge_traffic
    total_traffic += edge_traffic
    
hop1_traffic = (float(hop1_traffic)/total_traffic) *100
hop2_traffic = (float(hop2_traffic)/total_traffic) *100
hop3_traffic = (float(hop3_traffic)/total_traffic) *100
hop4_traffic = (float(hop4_traffic)/total_traffic) *100   

print ("hop_1 "+str(hop1_traffic))
print ("hop_2 "+str(hop2_traffic))
print ("hop_3 "+str(hop3_traffic))
print ("hop_4 "+str(hop4_traffic))
