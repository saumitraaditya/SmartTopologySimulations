from graph_tool.all import *
import matplotlib.pyplot as plt
import numpy as np

def getKey(vertex):
        return vertex.out_degree()

class utilities:
        def __init__(self,GraphFile):
            self.G = load_graph(GraphFile)
            self.newG = None
            self.sequence = None

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
                # we only need information about one hop neighbors, we
                # should only have them in the adj-list so remove any edge
                # that links to a node outside one-hop radius
                immed_neighbors = []
                for neighbor in v.out_neighbours():
                    immed_neighbors.append(self.G.vertex_index[neighbor])
                # Add self to the list
                immed_neighbors.append(self.G.vertex_index[v])
                # filter out edges between any vertex not in the immed_neighbors list
                filtered_edge_list = []
                for (src, dst) in indexed_edge_list:
                    if ((src in immed_neighbors) and (dst in immed_neighbors)):
                        filtered_edge_list.append((src,dst))
                # every vertex in the view has an edge associated with a list containing edges from/to it
                for (src,dst) in filtered_edge_list:
                    adj_list[v][src]=[]
                    adj_list[v][dst]=[]
                for (src,dst) in filtered_edge_list:
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

        def createGraph(self,file_name):
            # creates a Graph object by reading a adj_list file.
            newG = Graph(directed=False)
            # get number of vertices in the file
            linecount=0
            f = open(file_name)
            line = f.readline()
            while line:
                linecount+=1
                line = f.readline()
            f.close()
            newG.add_vertex(linecount)
            # Start adding edges
            f = open(file_name)
            line = f.readline()
            while line:
                v_list = line.split(" ")
                src = v_list[0]
                # last element is newline
                for dst in v_list[1:-1]:
                    if (newG.edge(src,dst)==None):
                        newG.add_edge(src,dst)
                line = f.readline()
            f.close()
            self.newG = newG
            self.newG.save("processed_graph.xml.gz")

        def plotDegreeDistribution(self,file_name=None):
            if self.newG==None:
                if (file_name==None):
                    print("Please enter the path of processed Graph file.")
                    return
                else:
                    self.createGraph(file_name)
            # compares degree distribution before and after
            x_axis = range(0,self.G.num_vertices())
            plt.gca().set_color_cycle(['red', 'green'])
            social_degree = []
            overlay_degree = []
            for i in x_axis:
                social_degree.append(self.G.vertex(i).out_degree())
                overlay_degree.append(self.newG.vertex(i).out_degree())
            plt.plot(x_axis, social_degree)
            plt.plot(x_axis, overlay_degree)
            plt.legend(["social_degree", "overlay_degree"], loc="upper_left")
            plt.title("social degree vs overlay degree")
            plt.show()
            
        

        def MHopCountDistribution(self,file_name=None):
            #
            if self.newG == None:
                if (file_name == None):
                    print("Please enter the path of processed Graph file.")
                    return
                else:
                    self.createGraph(file_name)
           
            MCD = []
            edge_property = self.G.edge_properties["edge_cost"]
            v_list = [v for v in self.G.vertices()]
            v_list = sorted(v_list,key=getKey)
            v_indexList = [self.G.vertex_index[v] for v in v_list]
            
            for v_index in v_indexList:
                src = self.newG.vertex(v_index)
                n_list = [self.G.vertex_index[n] for n in self.G.vertex(v_index).out_neighbours()]
         
                messaging_cost = 0
                for n in n_list:
                    messaging_cost += edge_property[self.G.edge(src,n)]*graph_tool.topology.shortest_distance(self.newG, source=src, target=self.newG.vertex(n))
                avg_messaging_cost = float(messaging_cost)/len(n_list)
                MCD.append(avg_messaging_cost)
            self.sequence = MCD
            plt.gca().set_color_cycle(['red'])
            plt.plot(range(0,self.G.num_vertices()),MCD)
            #plt.yscale('log')
            plt.legend(["avg social neighbour message costs"], loc="upper_left")
            plt.title("Avg Messaging cost")
            plt.show()
            
        def HopCountDistribution(self,file_name=None):
            #
            if self.newG == None:
                if (file_name == None):
                    print("Please enter the path of processed Graph file.")
                    return
                else:
                    self.createGraph(file_name)
            HCD = []
            v_list = [v for v in self.G.vertices()]
            v_list = sorted(v_list,key=getKey)
            v_indexList = [self.G.vertex_index[v] for v in v_list]
            for v_index in v_indexList:
                src = self.newG.vertex(v_index)
                n_list = [self.G.vertex_index[n] for n in self.G.vertex(v_index).out_neighbours()]
                hop_sum = 0
                for n in n_list:
                    hop_sum += graph_tool.topology.shortest_distance(self.newG, source=src, target=self.newG.vertex(n))
                avg_hop_sum = float(hop_sum) / len(n_list)
                HCD.append(avg_hop_sum)
            self.sequence = HCD
            plt.gca().set_color_cycle(['red'])
            plt.plot(range(0,self.G.num_vertices()),HCD)
            plt.legend(["avg social neighbour hop count"], loc="upper_left")
            plt.title("Avg Social Hop Count")
            plt.show()
            

        def edgePlot(self,file_name=None):
            if self.newG == None:
                if (file_name == None):
                    print("Please enter the path of processed Graph file.")
                    return
                else:
                    self.createGraph(file_name)
            objects = ("social_edges", "overlay_edges")
            y_pos = np.arange(len(objects))
            num_edges = [self.G.num_edges(), self.newG.num_edges()]
            plt.bar(y_pos, num_edges, align='center', alpha=0.5)
            plt.xticks(y_pos, objects)
            plt.ylabel('num edges')
            plt.title('total edges-social vs overlay')
            plt.show()

        def isConnected(self,file_name=None):
            if self.newG == None:
                if (file_name == None):
                    print("Please enter the path of processed Graph file.")
                    return
                else:
                    self.createGraph(file_name)
            comp, hist = graph_tool.topology.label_components(self.newG)
            if (len(hist) > 1):
                print (hist)
                print ("Graph is cot connected.")
            else:
                print ("Graph is connected.")


        def getResults(self,filename):
            self.createGraph(filename)
            self.isConnected()
            self.edgePlot()
            self.plotDegreeDistribution()
            self.HopCountDistribution()
            self.MHopCountDistribution()

