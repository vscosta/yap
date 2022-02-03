:- modeh(1,active(+drug)).

:- modeb(*,ames(+drug)).
:- modeb(*,mutagenic(+drug)).
:- modeb(*,has_property(+drug,#property,#propval)).
:- modeb(*,ashby_alert(#alert,+drug,-ring)).
:- modeb(*,ind(+drug,#alert,-nalerts)).

:- modeb(*,atm(+drug,-atomid,#element,#integer,-charge)).
:- modeb(*,symbond(+drug,+atomid,-atomid,#integer)).

:- modeb(1,gteq(+charge,#real)).
:- modeb(1,lteq(+charge,#real)).
:- modeb(1,((+charge) = (#charge))).
:- modeb(1,gteq(+nalerts,#integer)).
:- modeb(1,lteq(+nalerts,#integer)).
:- modeb(1,((+nalerts) = (#nalerts))).

:- modeb(*,nitro(+drug,-ring)).
:- modeb(*,sulfo(+drug,-ring)).
:- modeb(*,methyl(+drug,-ring)).
:- modeb(*,methoxy(+drug,-ring)).
:- modeb(*,amine(+drug,-ring)).
:- modeb(*,aldehyde(+drug,-ring)).
:- modeb(*,ketone(+drug,-ring)).
:- modeb(*,ether(+drug,-ring)).
:- modeb(*,sulfide(+drug,-ring)).
:- modeb(*,alcohol(+drug,-ring)).
:- modeb(*,phenol(+drug,-ring)).
:- modeb(*,carboxylic_acid(+drug,-ring)).
:- modeb(*,ester(+drug,-ring)).
:- modeb(*,amide(+drug,-ring)).
:- modeb(*,deoxy_amide(+drug,-ring)).
:- modeb(*,imine(+drug,-ring)).
:- modeb(*,alkyl_halide(+drug,-ring)).
:- modeb(*,ar_halide(+drug,-ring)).
:- modeb(*,benzene(+drug,-ring)).
:- modeb(*,hetero_ar_6_ring(+drug,-ring)).
:- modeb(*,non_ar_6c_ring(+drug,-ring)).
:- modeb(*,non_ar_hetero_6_ring(+drug,-ring)).
:- modeb(*,six_ring(+drug,-ring)).
:- modeb(*,carbon_5_ar_ring(+drug,-ring)).
:- modeb(*,hetero_ar_5_ring(+drug,-ring)).
:- modeb(*,non_ar_5c_ring(+drug,-ring)).
:- modeb(*,non_ar_hetero_5_ring(+drug,-ring)).
:- modeb(*,five_ring(+drug,-ring)).
:- modeb(1,connected(+ring,+ring)).
