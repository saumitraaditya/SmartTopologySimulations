from graph_tool.all import *


class utilities:
        def __init__(self,GraphFile):
            self.G = load_graph(GraphFile)

        def createAdjList(self, save_as =None):
            adj_list = self.G.new_vertex_property("object")
            for v in self.G.vertices():
                adj_list[v]={}
                # find all edges in two-hop neighbourhood
                edge_list = []
                for e in v.out_edges():
                    edge_list.append(e)
                for n in v.out_neighbours():
                    for e in n.out_edges():
                        edge_list.append(e)
                # remove duplicate edges
                edge_list = list(set(edge_list))
                # translate in terms of indices
                indexed_edge_list = []
                for e in edge_list:
                    src = self.G.vertex_index[e.source()]
                    dst = self.G.vertex_index[e.target()]
                    indexed_edge_list.append((src,dst))
                # parse into adj_list graph_view for the vertex
                # every vertex in the view has an edge associated with a list containing edges from/to it
                for (src,dst) in indexed_edge_list:
                    adj_list[v][src]=[]
                    adj_list[v][dst]=[]
                for (src,dst) in indexed_edge_list:
                    adj_list[v][src].append((src,dst))
                    adj_list[v][dst].append((src,dst))

            self.G.vertex_properties["adj_list"] = adj_list
            if (save_as != None):
                    self.G.save(save_as)
            return save_as

        def assignEdgeCosts(self,base_value=100000.00,save_as=None):
            edge_cost = self.G.new_edge_property("double")
            for e in self.G.edges():
                out_degree_sum = e.source().out_degree() + e.target().out_degree()
                cost = base_value/out_degree_sum
                edge_cost[e]=cost
            self.G.edge_properties["edge_cost"] = edge_cost
            if (save_as != None):
                    self.G.save(save_as)
            return save_as
