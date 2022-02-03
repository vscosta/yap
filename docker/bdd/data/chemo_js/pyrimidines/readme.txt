Dataset for Inductive Logic Programming (ILP)
Learning Quantitative Structure Activity Relationships (QSARs)
The Inhibition of Dihydrofolate Reductase by Pyrimidines

The data comes in 2 parts:
1) A 5-fold cross-validation series of 55 compounds
test_s[12345]*
train_s[12345]*
These correspond to the data described in:
King, Ross .D., Muggleton, Steven., Lewis, Richard. and Sternberg, Michael.J.E.
Drug Design by machine learning: the use of inductive logic programming 
to model the structure-activity relationships of trimethoprim analogues 
binding to dihydrofolate reductase. 
Proc. Natl. Acad. Sci. USA. 1992, 89, 11322-11326.

King, Ross .D., Muggleton, Steven., Lewis, Richard., Srinivasan, Ashwin., 
Feng, Cao. and Sternberg, Michael.J.E.
Drug design using inductive logic programming
Proceeding of the 26th Annual Hawaii International Conference on System 
Sciences, 1 pp. 646-655, Los Alamitos, CA: IEEE Computer Society Press.


2) An extra dataset of 19 compounds found after the original 55.
These compounds and more details of the above are described in:
King, Ross .D., Hurst, Jonathan. D.,  and Sternberg, Michael.J.E.
A comparison of artificial intelligence methods for modelling QSARs
Applied Artificial Intelligence, 1994 (in press).

Hurst, Jonathan. D., King, Ross .D. and Sternberg, Michael.J.E.
Quantitative Structure-Activity Relationships by neural networks and 
inductive logic programming: 1. The inhibition of dihydrofolate reductase by 
pyrimidines. Journal of Computer Aided Molecular Design 1994 (in press).

The positive facts are given in the files *.f
QSAR problems are in general regression problems, 
i.e. a real number must be predicted, not a class.
To get around this problem for ILP we used the hack of learning the 
greater activity relationship between pairs of compounds, i.e. 
The predicate 
great(Drug_no1, Drug_no2).
States that drug no. 1 has higher activity than drug no. 2.  
From  rules learnt for this relationship it is  possible 
to rank the drugs by activity 
(we used the David method, David, H.A.
Ranking from unbalanced paired-comparison data, Biometrika, 74, 432-436).


The negative facts are given in the files *.n.
These are the inverse of the facts in the *.f files.


The background facts are given in two files: struc.b and back.b.

struc.b
Contains the predicate 
struc(Drug_no, Chemical_group, Chemical_group, Chemical_group).
Drug_no.  is the number of the drug (see papers)
Subst_position[345] are the 3 places of possible chemical group substitution.
e.g
struc(d40,cf3,xoch3,h).
States that in drug 40 
there is a CF3 group substituted at position 3 on the pyrimidine ring and
there is a OCH3 group substituted at position 4 on the pyrimidine ring and
there is no substitution at position 5 on the pyrimidine ring (only Hydrogen).
The "x" is used to type the chemical groups, as substitutions at position 
4 are different from positions 3 and 5.


back.b
Contain the predicates for chemical properties of the chemical groups and 
arithmetical information.  The different chemical type are typed to 
avoid spurious comparison.  These predicates were not very well thought 
out and could be pruned.

polar(Group, Property).
size(Group, Property).
flex(Group, Property).
h_doner(Group, Property). (I can't spell)
h_acceptor(Group, Property).
pi_doner(Group, Property).
pi_acceptor(Group, Property).
polarisable(Group, Property).
sigma(Group, Property).

polar[012345](Group).
size[012345](Group).
flex[012345678](Group).
h_doner[012](Group).
h_acceptor[012](Group).
h_acceptor[012](Group).
pi_doner[012](Group).
pi_acceptor[012](Group).
polarisable[0123](Group).
sigma[012345](Group).

polar[012345]x(Group).
size[012345]x(Group).
flex[012345678x(Group).
h_doner[012]x(Group).
h_acceptor[012]x(Group).
h_acceptor[012]x(Group).
pi_doner[012]x(Group).
pi_acceptor[012]x(Group).
polarisable[0123]x(Group).
sigma[012345]x(Group).

great_polar(Property, Property).
great[01234567]_polar(Property).
less[01234567]_polar(Property).

great_size(Property, Property).
great[01234567]_size(Property).
less[01234567]_size(Property).

great_flex(Property, Property).
great[01234567]_flex(Property).
less[01234567]_flex(Property).

great_h_don(Property, Property). 
great[01234567]_h_don(Property).
less[01234567]_h_don(Property).

great_h_acc(Property, Property).
great[01234567]_h_acc(Property).
less[01234567]_h_acc(Property).

great_pi_don(Property, Property).
great[01234567]_pi_don(Property).
less[01234567]_pi_don(Property).

great_pi_acc(Property, Property).
great[01234567]_pi_acc(Property).
less[01234567]_pi_acc(Property).

great_polari(Property, Property).
great[01234567]_polari(Property).
less[01234567]_polari(Property).

great_sigma(Property, Property).
great[01234567]_sigma(Property).
less[01234567]_sigma(Property).


Ross D. King
Biomolecular Modelling Laboratory
Imperial Cancer Research Fund
P.O. Box 123
44 Lincoln's Inn Fields
London WC2A 3PX
U.K.
+44-71-242-0200 x3023
rd_king@icrf.ac.uk
