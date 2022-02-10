% alzheimer''s data from j. med chem
% determinate background knowledge

:- modeh(great(+a,+a)).
:- modeb(*,x_subst(+a,-n,-b)).
:- modeb(*,alk_groups(+a,-n)).
:- modeb(*,r_subst_1(+a,-l)).
:- modeb(*,r_subst_2(+a,-m)).
:- modeb(*,r_subst_3(+a,-n)).
:- modeb(*,ring_substitutions(+a,-n)).
:- modeb(*,ring_subst_1(+a,-b)).
:- modeb(*,ring_subst_2(+a,-b)).
:- modeb(*,ring_subst_3(+a,-b)).
:- modeb(*,ring_subst_4(+a,-b)).
:- modeb(*,ring_subst_5(+a,-b)).
:- modeb(*,ring_subst_6(+a,-b)).
:- modeb(*,polar(+b,-c)).
:- modeb(*,size(+b,-d)).
:- modeb(*,flex(+b,-e)).
:- modeb(*,h_doner(+b,-f)).
:- modeb(*,h_acceptor(+b,-g)).
:- modeb(*,pi_doner(+b,-h)).
:- modeb(*,pi_acceptor(+b,-i)).
:- modeb(*,polarisable(+b,-j)).
:- modeb(*,sigma(+b,-k)).
:- modeb(*,n_val(+a,-n)).
:- modeb(*,gt(+n,-n)).
:- modeb(*,great_polar(+c,-c)).
:- modeb(*,great_size(+d,-d)).
:- modeb(*,great_flex(+e,-e)).
:- modeb(*,great_h_don(+f,-f)).
:- modeb(*,great_h_acc(+g,-g)).
:- modeb(*,great_pi_don(+h,-h)).
:- modeb(*,great_pi_acc(+i,-i)).
:- modeb(*,great_polari(+j,-j)).
:- modeb(*,great_sigma(+k,-k)).

:- set(cross_validation_folds, 10).

:- [background].
:- [acetyl_examples].
