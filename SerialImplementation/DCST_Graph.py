from graph_tool.all import *
import random,os
from operator import itemgetter
from collections import defaultdict
from gi.repository import Gtk, Gdk,GdkPixbuf, GObject

from lact_modified import *

class DCST:
    def __init__(self,graph_file,offscreen=False):
        self.G = load_graph(graph_file)
        # new edge property, for filtering edges
        edge_isDCST = self.G.new_edge_property("bool", False)
        dcst_edges = self.G.new_vertex_property("object")
        num_links = self.G.new_vertex_property("int",0)
        self.G.vertex_properties["num_links"] = num_links
        for v in self.G.vertices():
            # print results on console
            curr_vert_index = self.G.vertex_index[v]
            adj_list = self.G.vertex_properties["adj_list"][v]
            lact = LACT(self.G,adj_list,edge_isDCST,5,.2)
            dcst = lact.iterateTree(curr_vert_index)
            for e in dcst:
                edge_isDCST[e] = True
                self.G.vertex_properties["num_links"][e.source()]+=1
                self.G.vertex_properties["num_links"][e.target()] += 1
            print(str(lact.MinTreeCost)+" :: "+str(curr_vert_index))
            print("-----------------------------------")
            lact.displaySpanningTree(BestTree=True)
            print("-----------------------------------")

            # for visualization every vertex will have to store indices of edges part of its dcst
            dcst_edges[v] =[]
            for e in dcst:
                src = self.G.vertex_index[e.source()]
                dst = self.G.vertex_index[e.target()]
                dcst_edges[v].append((src,dst))
        # Will need an edge filter for selected edges
        self.G.edge_properties["edge_isDCST"] = edge_isDCST
        self.G.vertex_properties["dcst_edges"]= dcst_edges
        self.G.save("nx_20_DCST.xml.gz")
        self.initializeVisualization()

    def update_frame(self,widget,event):
        self.src = widget.picked
        if self.src is None:
            return True
        if isinstance(self.src, PropertyMap):
            self.src = [v for v in self.G.vertices() if self.src[v]]
            if len(self.src) == 0:
                return True
            self.src = self.src[0]
        if self.src == self.old_src:
            return True
        self.old_src = self.src

        for v in self.G.vertices():
            self.vcolor[v] = [0.8, 0.1, 0.1, .5]
        for e in self.G.edges():
            self.ecolor[e] = [0.1, 0.1, 0.8, 1]

        dcst_edges = self.G.vertex_properties["dcst_edges"][self.src]
        for (src,dst) in dcst_edges:
            e= self.G.edge(src,dst)
            self.vcolor[self.G.vertex(src)]= self.green
            self.vcolor[self.G.vertex(dst)]= self.green
            self.ecolor[e] = self.green

        widget.regenerate_surface()
        widget.queue_draw()


    def initializeVisualization(self,offScreen=False):
        self.pos = sfdp_layout(self.G)
        self.vertIdentifier = self.G.new_vertex_property("string")
        self.ecolor = self.G.new_edge_property("vector<double>")
        for e in self.G.edges():
            self.ecolor[e] = [0.1, 0.1, 0.8, 1]
        self.vcolor = self.G.new_vertex_property("vector<double>")
        for v in self.G.vertices():
            self.vertIdentifier[v]=str(self.G.vertex_index[v])
            self.vcolor[v] = [0.8, 0.1, 0.1, .5]
        win = GraphWindow(self.G, self.pos, geometry=(1000, 800),vprops={"text":self.vertIdentifier},
                  edge_color=self.ecolor,
                  vertex_fill_color=self.vcolor)
        self.green = [0.1, 0.9, 0.0,0.5]
        self.old_src = None
        self.count = 0
        # Bind the function above as a motion notify handler
        win.graph.connect("motion_notify_event", self.update_frame)

        # We will give the user the ability to stop the program by closing the window.
        win.connect("delete_event", Gtk.main_quit)

        # Actually show the window, and start the main loop.
        win.show_all()
        Gtk.main()

if __name__== "__main__":
    dcst = DCST("nx_20.adj_list.xml.gz",False)

