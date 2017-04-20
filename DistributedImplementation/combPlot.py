#!/usr/bin/env python
# a bar plot with errorbars
# http://stackoverflow.com/questions/11597785/setting-spacing-between-grouped-bar-plots-in-matplotlib
import numpy as np
import matplotlib.pyplot as plt

patterns = ('-', '+', 'x', '.', '*', 'o', 'O', '\\')
N = 5


ind = np.arange(N)  # the x locations for the groups
width = 0.15       # the width of the bars
fig = plt.figure()
ax1 = fig.add_subplot(1, 2, 1)
plt.sca(ax1)

oneHop = (83.35, 85.73, 45.76, 83.72, 46.23)
rects1 = ax1.bar(ind, oneHop, width, color='w',hatch=patterns[0])


twoHop = (5.27, 5.46, 26, 8.17, 23.4)
rects2 = ax1.bar(ind + width, twoHop, width, color='w',hatch=patterns[1])


threeHop = (7.13, 5.74, 18.14, 5.58, 22.8)
rects3 = ax1.bar(ind + 2*width, threeHop, width, color='w',hatch=patterns[2])

fourHop = (4.23,3.06,10.09, 2.5, 7.5)
rects4 = ax1.bar(ind + 3*width, fourHop, width, color='w',hatch=patterns[3])

# add some text for labels, title and axes ticks

ax1.set_ylabel('% of traffic',fontsize=22)
ax1.set_xticks(ind + width)
ax1.set_xticklabels(('FRUGAL', 'GREEDY', 'RANDOM','GREEDY-BS','RANDOM-BS'))
plt.xticks(fontsize=20)  
plt.yticks(fontsize=20) 

ax1.legend((rects1[0], rects2[0], rects3[0], rects4[0]), ('1-Hop', '2-Hop', '3-Hop', '4-Hop'),fontsize=20,bbox_to_anchor=(1.1, 1.05))
ax1.set_title('a. Network-wide traffic-hop distribution',fontsize=22)


ax2 = fig.add_subplot(1, 2, 2)
plt.sca(ax2)

width = 0.15       # the width of the bars


oneHop = (46.26, 32.94, 32.64, 42.9, 43.8)
rects1 = ax2.bar(ind, oneHop, width, color='w',hatch=patterns[0])


twoHop = (37.00, 42.67, 42.82, 39.5,40.7)
rects2 = ax2.bar(ind + width, twoHop, width, color='w',hatch=patterns[1])


threeHop = (16.14, 18.66, 17.20, 16.67, 15.13)
rects3 = ax2.bar(ind + 2*width, threeHop, width, color='w',hatch=patterns[2])

fourHop = (.45, 1.13, 1.20, 0.18, 0.17)
rects4 = ax2.bar(ind + 3*width, fourHop, width, color='w',hatch=patterns[3])

gtfourHop = (0.13, 4.57, 6.12, 2.51, 0.42)
rects5 = ax2.bar(ind + 4*width, gtfourHop, width, color='w',hatch=patterns[4])

# add some text for labels, title and axes ticks
ax2.set_ylabel('% of nodes',fontsize=22)
ax2.set_xticks(ind + width)
ax2.set_xticklabels(('FRUGAL', 'GREEDY', 'RANDOM','GREEDY-BS','RANDOM-BS'))
plt.xticks(fontsize=20)  
plt.yticks(fontsize=20) 

ax2.legend((rects1[0], rects2[0], rects3[0], rects4[0], rects5[0]), ('1 Hop', '2 Hop', '3 Hop', '4 Hop','4+ Hop'), fontsize=20, bbox_to_anchor=(1.1, 1.05))
ax2.set_title('b. Social-hop distribution of nodes',fontsize=22)

def autolabel(rects):
    # attach some text labels
    for rect in rects:
        height = rect.get_height()
        ax.text(rect.get_x() + rect.get_width()/2., 1.05*height,
                '%d' % int(height),
                ha='center', va='bottom')

'''autolabel(rects1)
autolabel(rects2)
autolabel(rects3)
autolabel(rects4)'''

plt.show()
