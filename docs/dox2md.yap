:- use_module(library(maplist)).
:- use_module(library(lists)).
:- use_module(library(xml2yap)).


main :-
    %unix(argv(_Opts)),
    xml_load('xml/index.xml',[doxygenindex(XMLTasks)]),
    run_xml(XMLTasks).

run_xml(XMLTasks) :-
    xml2tasks(XMLTasks,Tasks),
    !,
    maplist(fetch,Tasks),
    merge_nodes.

xml2tasks(Tasks, NewTasks) :-
    foldl( run_task, Tasks, NewTasks, []).


run_task(compound([[refid(Ref),kind(predicate)],name(_)|_])) -->
    !,
    [predicate(Ref)].
run_task(compound([[refid(_Ref),kind(class)],name(_)|_])) -->
    !.
run_task(compound([[refid(Ref),kind(group)],name(_)|_])) -->
    !,
    [group(Ref)].
run_task(compound([[refid(Ref),kind(concept)],name(_)|_])) -->
    !,
    [predicate(Ref)].
run_task(compound([[refid(_),kind(page)],name(_)|_])) -->
    !.
run_task(compound([[refid(_),kind(file)],name(_)|_])) --> !.
run_task(compound([[refid(_),kind(file)],name(_)|_])) --> !.
run_task(compound([[refid(_),kind(dir)],name(_)|_])) --> !.
run_task(compound([[refid(_),kind(namespace)],name(_)|_])) -->!.
run_task(compound([[refid(_Ref),kind(union)],name(_)|_])) -->
    !.
run_task(compound([[_,kind(variable)]|_])) --> !.
run_task(member([[_,kind(variable)]|_])) --> [].
run_task(compound([[_,kind(file)]])) --> [].
run_task(compound([[_,kind(dir)]])) --> [].
run_task(compound([[_,kind(struct)]|_])) --> [].
run_task(compound([[_,kind(function)|_],_])) --> [].
run_task(location([file(File),
				   line(Line),
				   column(_),
				   bodyfile(Source),
				   bodystart(_),
				   bodyend(_)])) -->
    [location(File,Line,Source)].
run_task(['xmlns:xsi'(_)|_]) --> [].
run_task(Task) --> {writeln(task:Task), abort}.


fetch(T) :-
    writeln(T),
    fail.
fetch(predicate(Ref)) :-
    atom_concat(['xml/',Ref,'.xml'],G),
    catch(load_xml(G,[doxygen([_|Info])]),_,fail),
    writeln(p:G),
    !,
    functor(Descriptor,predicate,8),
    maplist(def2txt(0,Descriptor),Info),
    assert( Descriptor  ).
fetch(group(Ref)) :-
    atom_concat(['xml/',Ref,'.xml'],G),
    catch(load_xml(G,[doxygen([_|Info])]),_,fail),
    writeln(g:G),
    !,
    functor(Descriptor,group,8),
    maplist(def2txt(0,Descriptor),Info),
    assert( Descriptor).

def2txt(U0,Info,compounddef([_|Ps])) :-
    maplist(xml2txt(U0,Info),Ps),
    !.
def2txt(_,_,Task) --> {writeln(txt:Task), abort}.

xml2txt(_U0,_Info,C) :-
    writeln(C),
    fail.
xml2txt(_U0,_Info,compounddef(_)).
xml2txt(_U0,_Info,compoundname(_)).
xml2txt(U0,Info,briefdescription([[]|Paras])) :-
    arg(6,Info,Out),
    foldl(xml2tex(U0),Paras,``,Out).
xml2txt(U0,Info,detaileddescription([[]|Paras])) :-
    arg(7,Info, Desc),
    foldl(xml2tex(U0),Paras,``,Desc).
xml2txt(_U0,GT,location([[file(File),line(Line),column(Column)|_]])) :-
    arg(3,GT,File),
    arg(4,GT,Line),
    arg(5,GT,Column).

xml2txt(_,GT,listofallmembers([[]|L])) :-
    arg(8,GT,L).
xml2txt(_,_GT,detaileddescription(_)).
xml2txt(_,_,Task) :- writeln(xml:Task), abort.

xml2tex(U0,para([[]|Seq]))-->
    add_space(U0),
    foldl(par_(U0),Seq),
    !,
    add_nl.

strc(A,S0,SF) :-
    string_concat(S0,A, SF).

par_(_,lsquo(_),S0,SF) :-
     !,
     string_concat(S0,`\``,SF).
par_(_,true,S0,SF) :-
     !,
     string_concat(S0,`true`,SF).
par_(_,false,S0,SF) :-
     !,
     string_concat(S0,`false`,SF).
par_(_,rsquo(_),S0,SF) :-
     !,
     string_concat(S0,`'`,SF).
par_(U,bold([_|Text])) -->
    !,
    strc(`*`),
    maplist(par_(U),Text),
    strc(`*`).
par_(U,emphasis([[]|Text])) -->
    !,
    strc(`_`),
    foldl(par_(U),Text),
    strc(`_`).
ppar_(U,computeroutput([[],Text])) -->
    !,
    strc(`\``),
    par_(U,Text),
    strc(`\``).
par_(U,verbatim([[]|Text])) -->
    !,
    strc(`\``),
    foldl(par_(U),Text),
    strc(`\``).
par_(_,linebreak(_),S0,SF) :-
    !,
     string_concat(S0,`\n`,SF).
par_(U0,simplesect([_,para([_|Seq])])) -->
    !,
    foldl(par_(U0),Seq).
par_(U0, programlisting([_|L]),S0,SF) :-
    !,
     string_concat([S0,`\n\`\`\`~n`],S1),
    foldl(codeline(U0),L,S1,S2),
     string_concat([S2,`\n\`\`\`~n`],SF).
par_(U0, itemizedlist([[]|L]),S0,SF) :-
    !,
	 foldl(item(U0),L,S0,SF).
par_(U0, orderedlist([[]|L]),S0,SF) :-
    !,
	 foldl(item(U0),L,S0,SF).
par_(_U0,ref(_)) -->
    !.
par_(_U0, parameterlist(_)) -->
    !.
/*    foldl(parameteritem(U0),L,S0,SF).
par_(U, parameterlist([[_|_]|Seq]),S0,SF) :-
    foldl(xml2tex(U),Seq,S0,SF).
*/
par_(_U, ref([[refid(R)|_]|Name]),S0,SF) :-
    string(Name),
    !,
    string_concat([S0,`[`,R,`](`,Name,`)`],SF).
par_(_U, ref([[refid(R)|_]|Name]),S0,SF) :-
    atom(Name),
    !,
    atom_string(Name,SName),
    string_concat([S0,`[`,R,`](`,SName,`)`],SF).

par_(_U0, sp(_),S,S) :-
    !.
par_(_U0, A,S0,SF) :-
    string(A),
    !,
     string_concat([S0,A],SF).
par_(_U0, A,S0,SF) :-
    atom(A),
    !,
    atom_string(A,S),
     string_concat([S0,S],SF).
par_(_U0, A,S0,SF) :-
    number(A),
    !,
    number_string(A,S),
     string_concat([S0,S],SF).
par_(_,Task) --> {writeln(par:Task), abort}.

item(U0,listitem([[]|Seq]),S0,SF) :-
    string_concat(S0,`- `,S1),
    U is U0+4,
    foldl(xml2tex(U),Seq,S1,SF).

oitem(U0,listitem([[]|Seq]),S0,SF) :-
    string_concat(S0,`1. `,S1),
    U is U0+4,
    foldl(xml2tex(U),Seq,S1,SF).

parameteritem(_U0,parameteritem([[]|_Seq]),S,S) :-
    !.
codeline(U0,codeline([_|Seq]),S0,SF) :-
    !,
    foldl(cline(U0),Seq,S0,SF).
codeline(_U0,Seq,S0,SF) :-
    string(Seq),
    string_concat(S0,Seq,SF).

cline(_,sp(_),S,S) :-
    !.
cline(U0,highlight([_|Codes]),S0,SF) :-
    !,
    foldl(par_(U0),Codes,S0,SF).
cline(_,S,S0,SF) :-
    !,
    string_concat(S0,S,SF).

add_space(0,S,S) :-
    !.
add_space(N,S0,SF) :-
    string_concat(S0,` `,S1),
    N1 is N-1,
    add_space(N1,S1,SF).

add_nl(S0,SF) :-
    string_concat(S0,`~n~n`,SF).

merge_nodes :-
    retract(group(Id,Name,File,Line,Column,Brief,Text,Members)),
    groups_vs_preds(Members,Groups,Preds),
    atom_concat(['mkdocs/docs/',Ref,'.md'],F),
    open(F,write,S),
    format(S,'~n~s{#~s}~n+++++~n~n~s~n',[Name,Id,Brief]),
    (Groups = [_|_]
      ->
      format(S,'## Summary~n~n SubGroups                        | Descriptions~n--------------------------------|---------------------------------------------~n', []),
      maplist(addsubg(S,Ref), Groups),
      format(S,'--------------------------------|---------------------------------------------~n~n', [])),
    (Preds = [_|_]
      ->
    format(S,'## Summary~n~n Predicates                        | Descriptions~n--------------------------------|---------------------------------------------~n', []), 
      maplist(addsubp(S,Ref), Preds),
      format(S,'--------------------------------|---------------------------------------------~n~n', [])),
    format(S,'~n~s~n~n~s~n',[Brief,Text]),
    maplist(output_pred(S), Preds),
    footer(File,Line,Column),
    close(S),
    fail.

addsubg(S,Ref,Id) :-
    group(Id,Name,_File,_Line,_Column,Brief,_Text,_Members),
    format(S,'[~s](#~s)|~s~n',[Name,Ref,Id,Brief]).

addsubp(S,Ref,Id) :-
    predicate(Id,Name,_File,_Line,_Column,Brief,_Text,_Members),
    format(S,'[~s](#~s)|~s~n',[Name,Ref,Id,Brief]).

output_pred(S,Id) :-
    retract(predicate(Id,Name,_F,_L,_C,Brief,Text,_)),
    format(S,'~n~s{#~s}~n====~s~n',[Name,Id,Brief]),
    format(S,'~n~n~s~n',[Text]).

groups_vs_preds([],[],[]).
groups_vs_preds([[Ref,kind(group)]|Members],
    [[Ref,kind(function)]|Groups],Preds) :-
    groups_vs_preds(Members,Groups,Preds).
groups_vs_preds([Id|Members],[Id|Groups],Preds) :-
    groups_vs_preds(Members,Groups,Preds).
