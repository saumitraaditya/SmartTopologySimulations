import numpy as np
import matplotlib.pyplot as plt

hatches = ['|','.','x', '\\', '//', 'o']
objects = ['FRUGAL', 'GREEDY', 'RANDOM']
edges1 = [201,1536,1959]
edges2 = [1, 77, 84]
fig = plt.figure()
ax1 = fig.add_subplot(1, 2, 1)
ax2 = fig.add_subplot(1, 2, 2)
y_pos = np.arange(len(edges1))
ax1.bar(y_pos, edges1, align='center', alpha=0.5,color='grey')
ax1.set_xlabel('KONECT',fontsize=20)
ax1.set_ylabel('# connected components',fontsize=20)
plt.sca(ax1)
plt.xticks(range(len(objects)), objects)

plt.xticks(fontsize=18)  
plt.yticks(fontsize=18) 

y_pos = np.arange(len(edges2))
ax2.bar(y_pos, edges2, align='center', alpha=0.5,color='grey')
ax2.set_xlabel('SNAP', fontsize=20)




plt.sca(ax2)
plt.xticks(range(len(objects)), objects)
plt.xticks(fontsize=18)  
plt.yticks(fontsize=18)
plt.show()
