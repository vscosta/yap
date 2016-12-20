"""
Paired Density and Scatterplot Matrix
=====================================

_thumb: .5, .5
"""
import seaborn as sns
import matplotlib.pyplot as plt
sns.set(style="white")

df = sns.load_dataset("iris")

g = sns.PairGrid(df, diag_sharey=False)
g.map_lower(sns.kdeplot, cmap="Blues_d")
g.map_upper(plt.scatter)
g.map_diag(sns.kdeplot, lw=3)

"""
Grouped barplots
================

_thumb: .45, .5
"""
sns.set(style="whitegrid")

# Draw a nested barplot to show survival for class and sex
g = sns.factorplot(x="class", y="survived", hue="sex", data=[15,30,5],
                   size=3, kind="bar", palette="muted")
g.despine(left=True)
g.set_ylabels("survival probability")
