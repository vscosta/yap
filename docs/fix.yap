/** @file filter.yap
 *
 * This filter completes doxygen with YAP predicate indicator support..
 *
 */


:- use_module(('../library/lineutils'), [filter_string/3]).

:- set_prolog_flag(double_quotes, string).
 
:- include(docutils).

:- initialization(main).

main :-
    unix(argv([D])),
    directory_files(D,Fs),
    forall(member(F,Fs),do(D,F)).
main.

do(D,F) :-
   writeln(F),
    atom_concat([D,'/',F], File),
      atom_string(File,FString),
    generate(FString, Kind),
    open(File, read, Input),
    atom_concat(File,w,FW),
  open(FW, write, Output),  
  filter_string(Input, Output, process(Kind)),
  !,
  close(Input),
  close(Output),
  atom_concat(['mv ', FW, ' ', File], C),
  system(C).
do(_,_).

generate( File, class) :-
    abolish(replacement/2),
    string_concat([_Dir,"class", Name, ".html"], File),
    !,
   remove_dups(Name, InternalName),
   decode(InternalName, PI),
    string_concat(["Class ", InternalName], M1), 
    string_concat(["Predicate ", PI], PI1),
    string_concat(["<strong>", InternalName], M2), 
    string_concat(["<strong", PI], PI2),
    assert_static(replacement(M1, PI1)),
    assert_static(replacement(M2, PI2)).
generate( File, group) :-
    string_concat([_Dir,"group", _Name, ".html"], File),
    !.


process(group,M,PI) :-
    string_concat(["<td style=\"text-align: left;\"><a href=\"class", ClassRef], M),
    string_concat([C1,".html\"><strong>",M2],ClassRef),
    string_concat([C2,"</strong>",M3],M2),
    remove_dups(C2,CND),
    !,
    decode(CND, Pred),
    string_concat(["<td style=\"text-align: left;\"><a href=\"class", C1, ".html\"><strong>",Pred,"</strong>",M3],PI).

    
process(class,M, PI) :-
    replacement(M1, NM),
    string_concat([A,M1,B],M),
    !,
    string_concat([A,NM,B],PI).
process(_,M,M).




remove_dups(S,F) :-
    sub_string(S, Pref, 2, Suf, "__"),
    !,
     sub_string(S, _Pref, Suf, 0, More),
     remove_dups(More,ExtraDups),
     sub_string(S, 0, Pref, _, Start),
     string_concat([Start,"_",ExtraDups],F).
remove_dups(S,S).
