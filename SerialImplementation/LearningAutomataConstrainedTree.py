from graph_tool.all import *
import random
from operator import itemgetter
from collections import defaultdict

class automaton:
    def __init__(self,graph_object,vert_object,adj_list,reward):
        self.vert=vert_object
        self.vIndex=graph_object.vertex_index[self.vert]
        #every automaton maintains an action_set, which is set of incident edges
        #each action is denoted as a triplet
        #<E,probability,valid_for_current_iteration>
        self.edge_costs=graph_object.edge_properties["edge_cost"]
        self.edgeIsMarked=graph_object.edge_properties["edge_marked"]
        self.edgeProb=graph_object.edge_properties["edge_prob"]
        self.adj_list = adj_list
        self.action_set=[]
        self.active=True
        self.numActionsTaken=0
        self.reward=reward
        self.dynamicCost=999999.00 # initial value of minimum Cost incident edge seen till now.
        for src,dst in self.adj_list[self.vIndex]:#-------------------Have to use adjacency list representation here------------------------------------------------------
            self.action_set.append([graph_object.edge(src,dst),1.0/len(self.adj_list[self.vIndex]),True])

    def displayActionSet(self):
        for action in self.action_set:
            print("%s--%s, %f,%f,%s\t"%(action[0].source(),action[0].target(),action[1],self.edge_costs[action[0]],action[2]))

    # Edges that connect to a vertex already
    # on the tree should be set to False
    # for the current iteration
    def cycle_avoidance(self,verticesInTree):
        for triplet in self.action_set:
            if (verticesInTree[triplet[0].source()]==True and verticesInTree[triplet[0].target()]==True):
                triplet[2]=False

    # Scale action set if second argument is True else Rescale
    def scaleActionSet(self,scale):
        sum = 0
        for triplet in self.action_set:
            if (triplet[2]==False):
                continue
            sum = sum + triplet[1]
        # set scaled probabilities
        if (scale==True):
            for triplet in self.action_set:
                if (triplet[2] == False):
                    continue
                triplet[1] = triplet[1]/sum
        else:
            for triplet in self.action_set:
                if (triplet[2] == False):
                    continue
                triplet[1] = triplet[1] * sum

    def updateActionSet(self,edge):
        # update performed only for actions that are available
        for triplet in self.action_set:
            if (triplet[0]==edge):
                triplet[1]=triplet[1]+self.reward*(1-triplet[1])
            else:
                if (triplet[2]==True):
                    triplet[1]=(1-self.reward)*triplet[1]

    # called after cycle_avoidance has been called sets probability of actions associated with edges that
    # have been selected as part of DCST by other node high, so that they have a better chance of being selected
    def biasActionSet(self):
        for triplet in self.action_set:
            if (triplet[2]==False):
                continue
            else:
                if (self.edgeIsMarked[triplet[0]]):
                    triplet[1]=0.90

    def validateActionSet(self,degree_constraint,root=False):
        if (root==True):
            if (self.numActionsTaken>=degree_constraint):
                self.active=False
                return False
        else:
            if (self.numActionsTaken >= degree_constraint - 1):
                self.active=False
                return False
        for action in self.action_set:
            if (action[2]==True):
                return True
        return False

    # Select an action based on the probability distribution
    # if the selected edge is less than dynamic cost of automaton
    # reward the action,update probability and rescale the probabilities
    def selectAction(self):
        self.scaleActionSet(True)
        # sort action set in descending order of probabilities
        self.action_set=sorted(self.action_set,key=itemgetter(1))
        choice=random.random()
        selected_triplet=None
        sum=0
        for triplet in self.action_set:
            if (triplet[2]==False):
                continue
            sum=sum+triplet[1]
            if (sum>choice):
                selected_triplet=triplet
                # update probability of this triplet if action is associated with lowest cost edge
                if (self.edge_costs[triplet[0]]<=self.dynamicCost):
                    self.updateActionSet(triplet[0])
                    self.dynamicCost=self.edge_costs[triplet[0]]
                break
        selected_prob = selected_triplet[1]
        self.scaleActionSet(False)
        invited_vertex=None
        if (selected_triplet[0].source()==self.vert):
            invited_vertex=selected_triplet[0].target()
        else:
            invited_vertex=selected_triplet[0].source()
        self.numActionsTaken += 1
        return [invited_vertex,selected_triplet[0],self.edge_costs[selected_triplet[0]],selected_prob]
    # called our every iteration,
    # resets numActionsTaken, enables all disabled actions
    def refresh(self):
        self.numActionsTaken=0
        for triplet in self.action_set:
            triplet[2]=True

class LACT:
    def __init__(self,graphObject,adj_list,degree_constraint,reward):
        # initialize automatons, one at every vertex
        # adj_list is the LocalGraphView
        self.G = graphObject
        self.AutomatonTable={}
        self.adj_list = adj_list
        for v_index in self.adj_list.keys():
            self.AutomatonTable[self.G.vertex(v_index)]=automaton(self.G,self.G.vertex(v_index),self.adj_list,reward)
        self.MaxDeg=degree_constraint
        self.SpanningTree=[]
        self.CostTree=0
        self.BestTree=None
        self.MinTreeCost=999999999

    def displaySpanningTree(self,BestTree=False):
        display_string=""
        if (BestTree==False):
            for e in self.SpanningTree:
                display_string+=str([self.G.vertex_index[e.source()],self.G.vertex_index[e.target()]])
                display_string+=" "
        else:
            for e in self.BestTree:
                display_string += str([self.G.vertex_index[e.source()], self.G.vertex_index[e.target()]])
                display_string += ", "
        print display_string

    # Iteration results in a DegreeConstrainedspanningTree
    def start(self):
        # start_vert=self.G.vertex(random.SystemRandom().randint(0,self.G.num_vertices()-1))
        start_vert=self.G.vertex(random.choice(self.adj_list.keys()))
        current_automaton=self.AutomatonTable[start_vert]
        # reset spanning tree list and cost
        self.SpanningTree = []
        self.CostTree = 0
        verticesInTree=defaultdict(int)
        verticesInTree[start_vert]=1
        treeTraceList=[]
        treeTraceList.append(start_vert)
        action_probability=[]
        while(len(self.SpanningTree)!=len(self.adj_list.keys())-1):
            # update actionset to avoid cycle
            current_automaton.cycle_avoidance(verticesInTree)
            if (current_automaton.validateActionSet(self.MaxDeg)==True):
                current_automaton.biasActionSet()
                invited_vertex,selected_edge,edge_cost,prob=current_automaton.selectAction()
                self.SpanningTree.append(selected_edge)
                self.CostTree+=edge_cost
                verticesInTree[invited_vertex]=1
                treeTraceList.append(invited_vertex)
                action_probability.append(prob)
            # if previous automaton is not active or it has no more actions to select
            # trace back for a active automaton with a non-empty actions set
            else:
                chosen_vert=None
                for v in reversed(treeTraceList):
                    self.AutomatonTable[v].cycle_avoidance(verticesInTree)
                    if (self.AutomatonTable[v].validateActionSet(self.MaxDeg)==True):
                        chosen_vert=v
                        current_automaton=self.AutomatonTable[v]
                        current_automaton.biasActionSet()
                        break
                # if no vertex is active or have no enabled actions exit
                if (chosen_vert==None):
                    print("DCST is not possible for this Vertex")
                    break
                invited_vertex, selected_edge, edge_cost,prob = current_automaton.selectAction()
                self.SpanningTree.append(selected_edge)
                self.CostTree += edge_cost
                verticesInTree[invited_vertex] = 1
                treeTraceList.append(invited_vertex)
                action_probability.append(prob)
            current_automaton = self.AutomatonTable[invited_vertex]
        # # print the results from iteration
        # for v in verticesInTree.keys():
        #     print self.G.vertex_index[v]
        # for edge in self.SpanningTree:
        #     print edge
        # for vert in self.AutomatonTable.keys():
        #     print(self.AutomatonTable[vert].dynamicCost)
        #     self.AutomatonTable[vert].displayActionSet()
        # print("--------------------------------")
        # self.displaySpanningTree()
        # print("--------------------------------")
        # print(self.CostTree)
        # print("################################")
        if (self.CostTree < self.MinTreeCost):
            self.MinTreeCost = self.CostTree
            self.BestTree = self.SpanningTree
        # after every iteration all automatons should be refreshed
        for vert in self.AutomatonTable.keys():
            self.AutomatonTable[vert].refresh()
        return action_probability

    def iterateTree(self):
        counter=0
        while(counter<1):
            self.start()
            counter+=1
        # self.displaySpanningTree(BestTree=True)
        # print(self.MinTreeCost)
        return self.BestTree
        # action_prob=self.start()
        # flag=False
        # while (flag!=True):
        #     flag=True
        #     for p in action_prob:
        #         if (p < .9):
        #             flag=False
        #             break
        #     action_prob=self.start()



