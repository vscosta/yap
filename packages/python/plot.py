%matplotlib inline
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
sns.set(style="white", context="talk")
rs = np.random.RandomState(7)

pos={0:(0,0),
     1:(1,0),
     2:(0,1),
     3:(1,1),
     4:(0.1,0.9),
     5:(0.3,1.1),
     6:(0.9,0.9)
     }

names={4:'MMM',
     5:'XXX',
     6:'ZZZ'}

def plot1(y10,y20):
    def gen(f,f0):
        return [f[0],f[1],-f[2]]/max(f,f0)
    ax1 = plt.subplot2grid((1,2), (0,0), colspan=2)
    ax2 = plt.subplot2grid((1,2), (0,1), colspan=2)
    ax3 = plt.subplot2grid((2,2), (2,0), colspan=2, rowspan=2)

    xs = ["+-","++","--"]
    y1 = gen(y10, y20)
    sns.barplot(xs, y1, palette="RdBu_r", ax=ax1)
    y2 = gen(y20,y10)
    sns.barplot(xs, y2, palette="Set3", ax=ax2)
    # Finalize the plot
    # sns.despine(bottom=True)


    G=nx.Graph()
    i=0
    G.pos={} # location
    G.pop={} # size
    lpos={0:(0,0),1:(0,0),2:(0,0),3:(0,0)}
    last=len(pos)-1
    for i in range(4,len(pos)):
        G.pos[i]=pos[i]
        G.pop[i]=2000
        (x,y) = pos[i]
        lpos[i] = (x,y-0.05)
        if i > 4:
            G.add_edge(i-1,i)
        else:
            G.add_edge(2,i)
    G.add_edge(3,last)
    nx.draw_networkx_nodes(G,pos,nodelist=range(4,len(pos)),ax=ax3)
    nx.draw_networkx_nodes(G,pos,nodelist=[0,1,2,3],node_color='b',ax=ax3)
    nx.draw_networkx_edges(G,pos,alpha=0.5,ax=ax3)
    nx.draw_networkx_labels(G,lpos,names,alpha=0.5,ax=ax3)
    plt.axis('off')
    plt.tight_layout(h_pad=3)
    plt.savefig("house_with_colors.png") # save as png

plot1([20,30,10],[30,30,5])
