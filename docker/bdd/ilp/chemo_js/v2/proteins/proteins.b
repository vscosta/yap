:- modeh(1, alpha(+protein,+position)).

:- modeb(1,position(+protein,+position,-aminoacid)).

:- modeb(1,octf(-position,-position,-position,-position,+position,-position,-position,-position,-position)).
:- modeb(1,alpha_triplet(+position,-position,-position)).
:- modeb(1,alpha_pair3(+position,-position)).
%:- modeb(*,alpha_pair4(+position,-position)).

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

:- determination(alpha/2,alpha_triplet/3).
:- determination(alpha/2,alpha_pair3/2).
%:- determination(alpha/2,alpha_pair4/2).
:- determination(alpha/2,octf/9).
:- determination(alpha/2,position/3).
:- determination(alpha/2,hydrophobic/1).
:- determination(alpha/2,very_hydrophobic/1).
:- determination(alpha/2,hydrophilic/1).
:- determination(alpha/2,positive/1).
:- determination(alpha/2,negative/1).
:- determination(alpha/2,neutral/1).
:- determination(alpha/2,large/1).
:- determination(alpha/2,small/1).
:- determination(alpha/2,tiny/1).
:- determination(alpha/2,polar/1).
:- determination(alpha/2,aliphatic/1).
:- determination(alpha/2,aromatic/1).
:- determination(alpha/2,hydro_b_don/1).
:- determination(alpha/2,hydro_b_acc/1).
:- determination(alpha/2,not_aromatic/1).
:- determination(alpha/2,small_or_polar/1).
:- determination(alpha/2,ar_or_al_or_m/1).
:- determination(alpha/2,aromatic_or_very_hydrophobic/1).

:- determination(alpha/2,lth/2).
:- determination(alpha/2,ltv/2).

:- determination(alpha/2,not_a/1).
:- determination(alpha/2,not_c/1).
:- determination(alpha/2,not_d/1).
:- determination(alpha/2,not_e/1).
:- determination(alpha/2,not_f/1).
:- determination(alpha/2,not_g/1).
:- determination(alpha/2,not_h/1).
:- determination(alpha/2,not_i/1).
:- determination(alpha/2,not_k/1).
:- determination(alpha/2,not_l/1).
:- determination(alpha/2,not_m/1).
:- determination(alpha/2,not_n/1).
:- determination(alpha/2,not_p/1).
:- determination(alpha/2,not_q/1).
:- determination(alpha/2,not_r/1).
:- determination(alpha/2,not_s/1).
:- determination(alpha/2,not_t/1).
:- determination(alpha/2,not_v/1).
:- determination(alpha/2,not_w/1).
:- determination(alpha/2,not_y/1).

:- [octf, alphanum, classes, nclasses, lt, positions].

:- set(train_pos, 'train.f').
:- set(train_neg, 'train.n').

:- set(test_pos, 'test.f').
:- set(test_neg, 'test.n').

:- set(search, heuristic).
:- set(evalfn, compression).
:- set(i, 2).
:- set(clauselength, 40).
:- set(minpos, 2).
:- set(noise, 30).
:- set(nodes, 100000).
