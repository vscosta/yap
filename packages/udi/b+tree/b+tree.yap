:- load_foreign_files(['libudi_b+tree'],[],udi_btree_init).

:- op(700,fx,max).
:- op(700,fx,min).
:- op(700,xfx,#==).
:- op(700,xfx,#>).
:- op(700,xfx,#<).
:- op(700,xfx,#>=).
:- op(700,xfx,#=<).

max X :- %%this overrides any previous att
        attributes:put_att_term(X,max(C)).

min X :- %%this overrides any previous att
        attributes:put_att_term(X,min(C)).

X #== Y :-%%this overrides any previous att
        attributes:put_att_term(X,eq(C,Y)).

%% range definition
X #> Y :-
        attributes:get_all_atts(X,C),
        c1(C,gt(_,Y),NC),
        attributes:put_att_term(X,NC).
X #>= Y :-
        attributes:get_all_atts(X,C),
        c1(C,ge(_,Y),NC),
        attributes:put_att_term(X,NC).
X #=< Y :-
        attributes:get_all_atts(X,C),
        c1(C,le(_,Y),NC),
        attributes:put_att_term(X,NC).
X #< Y :-
        attributes:get_all_atts(X,C),
        c1(C,lt(_,Y),NC),
        attributes:put_att_term(X,NC).

c1(A,X,X) :-
        var(A), !.

c1(gt(_,X),gt(_,Y),gt(_,Z)) :-
        Z is max(X,Y).
c1(gt(_,X),ge(_,Y),ge(_,Y)) :-
        Y is max(X,Y).
c1(gt(_,X),ge(_,Y),gt(_,X)) :-
        X is max(X,Y).
c1(ge(_,X),ge(_,Y),ge(_,Z)) :-
        Z is max(X,Y).
c1(ge(_,X),gt(_,Y),ge(_,X)) :-
        X is max(X,Y).
c1(ge(_,X),gt(_,Y),gt(_,Y)) :-
        Y is max(X,Y).

c1(lt(_,X),lt(_,Y),lt(_,Z)) :-
        Z is min(X,Y).
c1(lt(_,X),le(_,Y),le(_,Y)) :-
        Y is min(X,Y).
c1(lt(_,X),le(_,Y),lt(_,X)) :-
        X is min(X,Y).
c1(le(_,X),ge(_,Y),le(_,Z)) :-
        Z is min(X,Y).
c1(le(_,X),lt(_,Y),le(_,X)) :-
        X is min(X,Y).
c1(le(_,X),lt(_,Y),lt(_,Y)) :-
        Y is min(X,Y).

/*range construct*/
c1(gt(_,X),lt(_,Y),range(_,X,false,Y,false)).
c1(gt(_,X),le(_,Y),range(_,X,false,Y,true)).
c1(lt(_,Y),gt(_,X),range(_,X,false,Y,false)).
c1(lt(_,Y),ge(_,X),range(_,X,true,Y,false)).
c1(le(_,Y),gt(_,X),range(_,X,false,Y,true)).
c1(le(_,Y),ge(_,X),range(_,X,true,Y,true)).
c1(ge(_,X),lt(_,Y),range(_,X,true,Y,false)).
c1(ge(_,X),le(_,Y),range(_,X,true,Y,true)).
/*still needs to construct range +stuff*/