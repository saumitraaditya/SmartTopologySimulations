from graph_tool.all import *
from collections import defaultdict
import numpy as np
import matplotlib.pyplot as plt
from operator import itemgetter
import random
import sys

# this method captures multi-hop degrees of all vertices of the graph
# and stores it as an internal property saving it with the graph.
def updateHopNeighbors(network,hops,save_as=None):
    vprop_hopdegree = network.new_vertex_property("object")
    for vertex in network.vertices():
        dist = graph_tool.topology.shortest_distance(network, source=vertex,max_dist=hops).get_array().tolist()
        hop_neighbors = defaultdict(int)
        for i in dist:
            hop_neighbors[i]+=1
        print(hop_neighbors)
        vprop_hopdegree[vertex]=hop_neighbors
    # save the vertex property as an internal property
    network.vertex_properties["HopDegree"]=vprop_hopdegree
    # save the graph
    if(save_as!=None):
        network.save(save_as)

# this method will snip edges in a random fashion to reduce the
# out_degree of a vertex to the supplied parameter and will return
# a graph_view object.
# def randomSnip(network,out_degree_limit,save_as=None):
#     edge_prop_random_snip=network.new_edge_property("bool")
#     # initially mark all edges as true,edges marked false will be filtered off.
#     for edge in network.edges():
#         edge_prop_random_snip[edge]=True
#     for vertex in network.vertices():
#         # create a list of all the neighbors
#         neighbors=[]
#         for neighbor in vertex.out_neighbours():
#             neighbors.append(network.vertex_index[neighbor])
#         if (vertex.out_degree()>out_degree_limit):
#             num_edges_to_trim=vertex.out_degree()-out_degree_limit
#             # randomly select a list of vertices to whom edges will be trimmed
#             snip_list=random.sample(neighbors,num_edges_to_trim)
#             source=network.vertex_index[vertex]
#             for target in snip_list:
#                 edge_prop_random_snip[network.edge(source,target)]=False
#     network.edge_properties["RandomSnip"]=edge_prop_random_snip
#     # save the graph
#     if(save_as!=None):
#         network.save(save_as)
#     # create a graphView object with filtered parameters
#     gView=GraphView(network,efilt=edge_prop_random_snip)
#     return gView

def randomSnip(network,out_degree_limit,save_as=None):
    edge_prop_random_snip=network.new_edge_property("bool",True)
    for vertex in network.vertices():
        # create a list of all the neighbors
        out_edges=[]
        for edge in vertex.out_edges():
            out_edges.append(edge)
        out_edges = filter(lambda e:edge_prop_random_snip[e]==False,out_edges)
        already_marked=vertex.out_degree-len(out_edges)
        if (vertex.out_degree()>out_degree_limit):
            num_edges_to_trim=(vertex.out_degree()-out_degree_limit)-already_marked
            # randomly select a list of vertices to whom edges will be trimmed
            snip_list=random.sample(out_edges,num_edges_to_trim)
            for e in snip_list:
                edge_prop_random_snip[e]=False
    network.edge_properties["RandomSnip"]=edge_prop_random_snip
    # save the graph
    if(save_as!=None):
        network.save(save_as)
    # create a graphView object with filtered parameters
    gView=GraphView(network,efilt=edge_prop_random_snip)
    return gView

# this method captures and reports graph statistics of importance, i.e. total vertices,edges,degree_distribution
def report_stats(network_file,degree_stats):
    network = load_graph(network_file)
    # print number of vertices and edges
    print("total_vertices:%d total_edges:%d"%(network.num_vertices(),network.num_edges()))
    # calculate and report degree distribution
    degree_points=defaultdict(int)
    degree_stats=sorted(degree_stats)
    for vertex in network.vertices():
        for degree in degree_points:
            if (vertex.out_degree()>=degree):
                degree_stats[degree]+=1
    for degree in degree_points:
        print("node with degree more or equal %d: %d"%(degree,degree_stats[degree]))

''' Objective- For a vertex that has more edges than the limit, we trim vertices with minimum priority.
If I can get to a vertex B using a 1 hop neighbor A and I am V, I will increment the priority of the edge V<->A & A<->B.
'''
def mutual_snip(network,out_degree_limit,save_as=None):
    # edge property to store priorities.
    edge_prop_priority=network.new_edge_property("long")
    # vertex_property to save collected information
    v_prop_reachability_from=network.new_vertex_property("object")
    v_prop_reachability_to=network.new_vertex_property("object")
    v_prop_rank_list=network.new_vertex_property("object")
    # list of my neighbours
    # mapping 1-hop neighbor and vertices reachable via them
    for vertex in network.vertices():
        reachability_from=defaultdict(list) #How can I get to this vertex
        reachability_to=defaultdict(list)   #Where can I get from this vertex
        my_neighbors=[]
        for neighbor in vertex.out_neighbours():
            my_neighbors.append(network.vertex_index[neighbor])
            for neighbors_neighbor in neighbor.out_neighbours():
                reachability_from[network.vertex_index[neighbor]].append(network.vertex_index[neighbors_neighbor])
        # construct a reachabilty_to dataStructure from reachability_from
        for key in reachability_from.keys():
            for node_index in reachability_from[key]:
                reachability_to[node_index].append(key)
        # Based upon how many of my neighbours I can reach via this neigbour
        # I will rank the neighbours.
        rank_list=[]
        for node in vertex.out_neighbours():
            rank_list.append([network.vertex_index[node],len(set(my_neighbors).\
                                                         intersection(reachability_from[network.vertex_index[node]]))])
        # sort the list based on second element of the tuple/i.e. length of the intersection
        rank_list=sorted(rank_list,key=itemgetter(1))
        # save the collected information into properties.
        v_prop_reachability_from[vertex]=reachability_from
        v_prop_reachability_to[vertex]=reachability_to
        v_prop_rank_list[vertex]=rank_list

        # *************************************TEST*******************************************************
        print("*******************************************")
        print(network.vertex_index[vertex])
        print(reachability_to)
        print(reachability_from)
        print(rank_list)
        print("*******************************************")
        # ********************************************************************************************
     # save properties as internal graph properties
    network.vertex_properties["reachability_from"] = v_prop_reachability_from
    network.vertex_properties["reachability_to"] = v_prop_reachability_to
    network.vertex_properties["rank_list"] = v_prop_rank_list
    # save the graph
    if (save_as!=None):
        network.save(save_as)

''' Given the graph already has
1. reachability_to
2. reachability_from
3. rank_list
properties, this method assigns priorities to the edges, and saves it as a edge priority.'''
def prioritize_edges(network,save_as=None):
    # load properties
    reachability_from=network.vertex_properties["reachability_from"]
    reachability_to=network.vertex_properties["reachability_to"]
    rank_list=network.vertex_properties["rank_list"]
    # create new edge_property
    edge_priority=network.new_edge_property("float",0.0)
    # iterate over all th vertices
    for vertex in network.vertices():
        source=network.vertex_index[vertex]
        for neighbor,priority in rank_list[vertex]:
            edge_priority[network.edge(source,neighbor)]+=priority
    for vertex in network.vertices():
        vertex_neighbors=[network.vertex_index[v] for v in vertex.out_neighbours()]
        for neighbor,priority in rank_list[vertex]:
            for dst in set(reachability_from[vertex][neighbor]).intersection(vertex_neighbors) :
                edge_priority[network.edge(neighbor,dst)]+=priority*(.25)+1.0
        # every vertex nominates two of it's edges to have maximum priority
        # to prevent partiotioning, especially in the case of nodes that have low degrees
        # as edges to them will be treated unfavorably by other nodes
        if (vertex.out_degree()>0):
            if (vertex.out_degree()>1):
                forver_neighbors=random.sample(rank_list[vertex],2)
                for neighbor,priority in forver_neighbors:
                    edge_priority[network.edge(network.vertex_index[vertex],neighbor)]=float("inf")
            else:
                edge_priority[network.edge(network.vertex_index[vertex],rank_list[vertex][0][0])]=float("inf")

    network.edge_properties["priority"]=edge_priority
    if (save_as!=None):
        network.save(save_as)

def snip_on_priority(network,edge_limit,save_as):
    '''At this stage the input graph already has edge priorities
    I will select lowest (out_going_edges-edge_limit) and will
    filter them out. Will return a GraphView of the resulting graph also
    saving it'''
    edge_prop_priority_snip = network.new_edge_property("bool",True)
    edge_priority=network.edge_properties["priority"]
    for vertex in network.vertices():
        if (vertex.out_degree()>edge_limit):
            out_edges=[]
            for edge in vertex.out_edges():
                out_edges.append(edge)
                out_edges = filter(lambda e:edge_prop_priority_snip[e]==False,out_edges)
                already_marked=vertex.out_degree-len(out_edges)
            # get priority of all out going edges and sort it
            out_edges=[]
            for edge in vertex.out_edges():
                out_edges.append([edge,edge_priority[edge]])
            out_edges=sorted(out_edges,key=itemgetter(1))
            snip_count=vertex.out_degree()-edge_limit-already_marked
            for snip_edge in out_edges[0:snip_count]:
                 edge_prop_priority_snip[snip_edge[0]]=False
    network.edge_properties["PrioritySnip"] = edge_prop_priority_snip
    if (save_as!=None):
        network.save(save_as)

def compare_connectivity(original_graph,modified_graph,save_as=None):
    '''We have to compare before and after conectivity in this method
    i.e. for example - for a given vertex how many of the original
    neighbors are still within 2 hop reach after trimming.
    We count the unreachability for every node and report it.'''
    v_prop_unreachability=modified_graph.new_vertex_property("long")
    for v in original_graph.vertices():
        vert_index=original_graph.vertex_index[v]
        unreachable_counter=0
        for neighbor in v.out_neighbours():
            neighbor_index=original_graph.vertex_index[neighbor]
            dist=graph_tool.topology.shortest_distance(modified_graph,modified_graph.vertex(vert_index),modified_graph.vertex(neighbor_index))
            if(dist>2):
                unreachable_counter+=1
        v_prop_unreachability[modified_graph.vertex(vert_index)]=unreachable_counter
    modified_graph.vertex_properties["unreachability"] = v_prop_unreachability
    # save the graph
    if (save_as!=None):
        modified_graph.save(save_as)






if __name__=='__main__':
    input_filename=sys.argv[1]
    # network=Graph(directed=False)
    # with open(input_filename) as f:
    #     content=f.readlines()
    # num_vertices=int(content[0].split("\n")[0])
    # network.add_vertex(num_vertices)
    # for line in content[1:]:
    #     src,dst=line.split("\n")[0].split(",")
    #     network.add_edge(network.vertex(src),network.vertex(dst))
    # mutual_snip(network,5,"sample_graph.xml.gz")
    # network=load_graph(input_filename)
    # mutual_snip(network,5,"gowalla_with_mutual_snip.xml.gz")
    # prioritize_edges(network,save_as="nx_1000_with_updated_edge_priorities.xml.gz")
    # snip_on_priority(network,50,"nx_1000_with_updated_priority_snip.xml.gz")
    original_graph=load_graph(sys.argv[1])
    modified_graph=load_graph(sys.argv[2])
    modified_graph=GraphView(modified_graph,efilt=modified_graph.edge_properties["PrioritySnip"])
    compare_connectivity(original_graph,modified_graph,save_as="nx1000_unreachabilityAfterPSnip.xml.gz")
