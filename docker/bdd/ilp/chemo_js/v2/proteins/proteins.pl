:- modeh(1, alpha(+protein,+position)).

:- modeb(1,position(+protein,+position,-aminoacid)).
:- modeb(1,octf(-position,-position,-position,-position,+position,-position,-position,-position,-position)).
:- modeb(1,alpha_triplet(+position,-position,-position)).
:- modeb(1,alpha_pair3(+position,-position)).
%:- modeb(1,alpha_pair4(+position,-position)).

:- modeb(1,hydrophobic(+aminoacid)).
:- modeb(1,very_hydrophobic(+aminoacid)).
:- modeb(1,hydrophilic(+aminoacid)).
:- modeb(1,positive(+aminoacid)).
:- modeb(1,negative(+aminoacid)).
:- modeb(1,neutral(+aminoacid)).
:- modeb(1,large(+aminoacid)).
:- modeb(1,small(+aminoacid)).
:- modeb(1,tiny(+aminoacid)).
:- modeb(1,polar(+aminoacid)).
:- modeb(1,aliphatic(+aminoacid)).
:- modeb(1,aromatic(+aminoacid)).
:- modeb(1,hydro_b_don(+aminoacid)).
:- modeb(1,hydro_b_acc(+aminoacid)).
:- modeb(1,not_aromatic(+aminoacid)).
:- modeb(1,small_or_polar(+aminoacid)).
:- modeb(1,ar_or_al_or_m(+aminoacid)).
:- modeb(1,aromatic_or_very_hydrophobic(+aminoacid)).

:- modeb(1,lth(+aminoacid,+aminoacid)).
:- modeb(1,ltv(+aminoacid,+aminoacid)).

:- modeb(1,not_a(+aminoacid)).
:- modeb(1,not_c(+aminoacid)).
:- modeb(1,not_d(+aminoacid)).
:- modeb(1,not_e(+aminoacid)).
:- modeb(1,not_f(+aminoacid)).
:- modeb(1,not_g(+aminoacid)).
:- modeb(1,not_h(+aminoacid)).
:- modeb(1,not_i(+aminoacid)).
:- modeb(1,not_k(+aminoacid)).
:- modeb(1,not_l(+aminoacid)).
:- modeb(1,not_m(+aminoacid)).
:- modeb(1,not_n(+aminoacid)).
:- modeb(1,not_p(+aminoacid)).
:- modeb(1,not_q(+aminoacid)).
:- modeb(1,not_r(+aminoacid)).
:- modeb(1,not_s(+aminoacid)).
:- modeb(1,not_t(+aminoacid)).
:- modeb(1,not_v(+aminoacid)).
:- modeb(1,not_w(+aminoacid)).
:- modeb(1,not_y(+aminoacid)).

:- [octf, alphanum, classes, nclasses, lt, positions].
%:- [examples_fold], set(cross_validation_folds, 16).
%:- [examples_holdout], set(cross_validation_folds, 2).

:- [examples_train], set(cross_validation_folds, 1).
%:- [examples_test], set(cross_validation_folds, 1).

%:- set(clause_length, 10).
