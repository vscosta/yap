yaoyao     python_import( numpy  as np ),
No (more) answers
?-    python_import( matplotlib.pyplot  as plt ),
No (more) answers
?- 
?-     Mu = 100.0,  /* mean of distribution, */
?- 
?-     Sigma = 15.0,  /* standard deviation of distribution, */
No (more) answers
?- 
?-     x := Mu + Sigma * np.random.randn(10000),
No (more) answers
?-     NumBins = 50,
No (more) answers
?- 
?-     /* the histogram of the data */
No (more) answers
?- 
?-     plt.hist(x, NumBins, density=1, facecolor= `green`, alpha=0.5),
No (more) answers
?-     plt.show().
No (more) answers
?- [vsc@fedora site-packages]$ cat >x.py
main2 :-
      /* example data */
    python_import( numpy  as np ),
   python_import( matplotlib.pyplot  as plt ),

    Mu = 100.0,  /* mean of distribution, */

    Sigma = 15.0,  /* standard deviation of distribution, */

    x := Mu + Sigma * np.random.randn(10000),
    NumBins = 50,

    /* the histogram of the data */

    plt.hist(x, NumBins, density=1, facecolor= `green`, alpha=0.5),
    plt.show().
