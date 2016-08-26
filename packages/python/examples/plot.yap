
:- [library(python)].

main :-
	    Plt = matplotlib.pyplot,
	:= import( Plt ),
	:= (
	    Plt.figure(figsize=(10,2.5)),
	    Plt.plot([1,2,3,4]),
	    Plt.ylabel(`some numbers`),
	    Plt.show()
	   ).


main2 :-
:= (	import( numpy),
import( matplotlib.mlab),
import( matplotlib.pyplot) ),
NP = numpy,
Mlab = matplotlib.mlab,
Plt = matplotlib.pyplot,

% example data
mu := 100,  % mean of distribution,
sigma := 15,  % standard deviation of distribution,
x := mu + sigma * NP.random.randn(10000),

num_bins := 50,
% the histogram of the data
(n, bins, patches) := Plt.hist(x, num_bins, normed=1, facecolor= `green`, alpha=0.5),
% add a `best fit` line
y := Mlab.normpdf(bins, mu, sigma),
:= (Plt.plot(bins, y, `r--`),
Plt.xlabel(`Smarts`),
Plt.ylabel(`Probability`),
Plt.title(`Histogram of IQ: $\\mu=100$, $\\sigma=15$`),
% Tweak spacing to prevent clipping of ylabel,
Plt.subplots_adjust(left=0.15),
Plt.show()).
