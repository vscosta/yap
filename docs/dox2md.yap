:- use_module(library(maplist)).
:- use_module(library(lists)).
:- use_module(library(xml2yap)).

:- dynamic group/8, predicate/8, extra/2, pred/8, pred/3, show/0.

main :-
    %unix(argv(_Opts)),
    xml_load('../xml/index.xml',[doxygenindex(XMLTasks)]),
    run_xml(XMLTasks).

run_xml(XMLTasks) :-
    xml2tasks(XMLTasks,Tasks),
    reverse(Tasks,RTasks),
    !,
    maplist(fetch,RTasks),
    merge_nodes.

xml2tasks(Tasks, NewTasks) :-
    foldl( run_task, Tasks, NewTasks, []).

run_task(compound([[refid(Ref),kind(`predicate`)],name([[],Name])|_])) -->
    !,
    [predicate(Ref,Name)].
run_task(compound([[refid(Ref),kind(`group`)],name([[],Name])|_])) -->
    !,
    [group(Ref,Name)].
run_task(compound([[refid(Ref),kind(`concept`)],name([[],Name])|_])) -->
    !,
    [predicate(Ref,Name)].
run_task(compound([[refid(Ref),kind(`concept`)],name([[]])])) -->
    !,
    [predicate(Ref,``)].
run_task(compound([[refid(_Ref),kind(`class`)],name(_Name)|_])) -->
    !.
run_task(compound([[refid(_),kind(`page`)],name(_)|_])) -->
    !.
run_task(compound([[refid(_),kind(`file`)],name(_)|_])) --> !.
run_task(compound([[refid(_),kind(`file`)],name(_)|_])) --> !.
run_task(compound([[refid(_),kind(`dir`)],name(_)|_])) --> !.
run_task(compound([[refid(_),kind(`namespace`)],name([[],_Name])|_])) -->
!.								    
run_task(compound([[refid(_Ref),kind(`union`)],name([[],_Name])|_])) -->
    !.
run_task(compound([[_,kind(`variable`)]|_])) --> !.
run_task(member([[_,kind(`variable`)]|_])) --> [].
run_task(compound([[_,kind(`file`)]])) --> [].
run_task(compound([[_,kind(`var`)]])) --> [].
run_task(compound([[_,kind(`dir`)]])) --> [].
run_task(compound([[_,kind(`struct`)]|_])) --> [].
run_task(compound([[_,kind(`function`)|_],_])) --> [].
run_task(location([file(File),
				   line(Line),
				   column(_),
				   bodyfile(Source),
				   bodystart(_),
				   bodyend(_)])) -->
    [location(File,Line,Source)].
run_task(['xmlns:xsi'(_)|_]) --> !.
run_task(Task) --> {writeln(task:Task)}.

:- dynamic visited/1.

fetch(predicate(Ref,Name)) :-
	\+ visited(Ref),
	    assert(visited(Ref)),
    atom_concat(['../xml/',Ref,'.xml'],G),
    catch(load_xml(G,[doxygen([_|Info])]),_,fail),
    functor(Descriptor,predicate,8),
    arg(1,Descriptor,Ref),
    arg(2,Descriptor,Name),
    once(maplist(xml2txt(0,Descriptor),Info)),
    !,
    arg(7,Descriptor,Text),
%    add_tail(LText,Text),
    ( var(Text) -> Text = `` ; true ),
    assert( Descriptor  ).
fetch(group(Ref,Name)) :-
	\+ visited(Ref),
	    assert(visited(Ref)),
	atom_concat(['../xml/',Ref,'.xml'],G),
    catch(load_xml(G,[doxygen([_|Info])]),_,fail),
    functor(Descriptor,group,8),
    arg(1,Descriptor,Ref),
    arg(2,Descriptor,Name),
    once(maplist(xml2txt(0,Descriptor),Info)),
   arg(6,Descriptor,Brief),
   ( var(Brief) -> Brief = `` ; true ),
    arg(7,Descriptor,Text),
    ( var(Text) ->Text = `` ; true ),
   arg(8,Descriptor,Title),
    ( var(Title) -> Title = Name ; true ),
    !,
   assert( Descriptor).
fetch(_).

xml2txt(U0,Info,compounddef([_|Ps])) :-
    maplist(xml2txt(U0,Info),Ps),
    !.
xml2txt(_U0,_Info,compoundname(_)) :- !.
xml2txt(_U0,_Info,sectiondef([[kind(`func`)|_]|_])) :-
    !.
xml2txt(_U0,_Info,sectiondef([[kind(`define`)|_]|_])) :-
    !.
xml2txt(_U0,_Info,sectiondef([[kind(`def`)|_]|_])) :-
    !.
xml2txt(_U0,_Info,sectiondef([[kind(`enum`)|_]|_])) :-
    !.
xml2txt(_U0,_Info,sectiondef([[kind(`typedef`)|_]|_])) :-
    !.
xml2txt(_U0,_Info,sectiondef([[kind(`user-defined`)|_]|_])) :-
    !.
xml2txt(U0,Info,sectiondef([_|Paras])) :-
    foldl(par(U0),Paras,``,D),
    arg(7,Info,D0),
(var(D0)->D0=D;arg(1,Info,Id),assert(extra(Id,D))).
xml2txt(U0,Info,sect1([[id(Id)],title([[],S])|Paras])) :-
    (Id == `group__AttributedVariables_1autotoc_md7` -> assert(show);true),
    !,
    mcstr([`## `,S,`               {#`,Id,`};`],``,S0),
    add_nl(U0,S0,S1),
    foldl(par(U0),Paras,S1,D),
    arg(7,Info,D0),
    (var(D0)->D0=D;arg(1,Info,Id0),assert(extra(Id0,D))).
xml2txt(U0,Info,sect2([[id(Id)],title([[],S])|Paras])) :-
    !,
    mcstr([`### `,S,`               {#`,Id,`};`],``,S0),
    add_nl(U0,S0,S1),
    foldl(par(U0),Paras,S1,D),
    arg(7,Info,D0),
    (var(D0)->D0=D;arg(1,Info,Id0),assert(extra(Id0,D))).
xml2txt(_U0,Info,innergroup([[refid(Ref)],Name])) :-
    !,
    arg(1,Info,Ref0),
    assert(group(Ref0,Ref,Name)).
xml2txt(_U0,Info,innerclass([[refid(Ref)|_],Name])) :-
    !,
    arg(1,Info,Ref0),
	assert(pred(Ref0,Ref,Name)).

xml2txt(_U0,_Info,innerfile(_)).
xml2txt(U0,Info,
	briefdescription([[]|Paras])) :-
    !,
    arg(6,Info,Out),
    foldl(xml2tex(U0),Paras,``,Out).
xml2txt(U0,Info,detaileddescription([[]|Paras])) :-
    !,
    arg(7,Info, D0),
    foldl(xml2tex(U0),Paras,``,Desc),
    (var(D0)->D0=Desc;arg(1,Info,Id),assert(extra(Id,Desc))).
xml2txt(_U0,GT,location([[file(File),line(Line),column(Column)|_]])) :-
    !,
    arg(3,GT,File),
    arg(4,GT,Line),	
    arg(5,GT,Column).
xml2txt(_,GT,title([[],Title])) :-
     !,
     arg(8,GT,Title).
xml2txt(_,_GT,initializer([[]|_L])) :- !.
xml2txt(_,_GT,listofallmembers([[]|_L])) :-
    !.
xml2txt(_,_GT,includes(_)).
xml2txt(_,_,Task) :- writeln(xml:Task), !.

add2tail([V|_],V) :- !.
add2tail([_|L], V) :-
    add2tail(L,V).

compacttail([V]) -->
    !,
    cstr(V).
compacttail([V|Vs]) -->
    !,
    cstr(V),
    compacttail(Vs).

xml2tex(U0,para([[]|Seq]))-->
    add_space(U0),
    foldl(par(U0),Seq),
    !,
    add_nl(U0).
xml2tex(_U0,Task)-->{writeln(xml2tex:Task), !, fail}.


    

cstr(A,S0,SF) :-
    string_concat(S0,A, SF).

mcstr(A,S0,SF) :-
    string_concat([S0|A], SF).

rpar(U,Ts) -->
    foldl(par(U),Ts),
         cstr(`\n`).

par(U0,C)--> {show,writeln(xxxxxxxxxxxxxxx:C), fail}.
par(U0,memberdef([_|Seq]))-->
    !,
    { U is U0+4 },
    cstr(`* ` ),
    foldl(par(U),Seq),
    add_nl(U).
par(U,lsquo(_)) -->
     !,
     par(U,`\``).
par(_,param(_)) -->
!.	
par(U,mdash(_)) -->
     !,
     par(U,`-`).
par(U,ndash(_)) -->
     !,
     par(U,`-`).
par(_,true) -->
     !,
     cstr(`true`).
par(_,false) -->
     !,
    cstr(`false`).
par(_,ulink([[url(_)],_Text])) -->
     !.
par(U,sectiondef([[kind(`user-defined`)]|Text])) -->
    !,
    add_nl(U),
    foldl(par(U),Text).
par(U,sect1([[id(Id)],title([],S)|Text])) -->
    !,
    mcstr([`## `,S,`               {#`,Id,`};`]),
    add_nl(U),
    foldl(par(U),Text).
par(U,sect2([[id(Id)],title([],S)|Text])) -->
    !,
    mcstr([`### `,S,`               {#`,Id,`};`]),
    add_nl(U),
    foldl(par(U),Text).
par(_,rsquo(_)) -->
     !,
     cstr(`\'`).
par(_,zwj(_)) -->
     !.
par(U, definition([_,Name])) -->
    cstr(Name),
    !,
    cstr(`:`),
    add_nl(U).
par(_U, argsstring(_)) -->
    !.
par(_U, initializer(_)) -->
    !.
par(_U, name(_)) -->
    !.
par(_U, type(_)) -->
    !.
par(_U, location(_)) -->
    !.
par(_U, inbodydescription([])) -->
    !.
par(U, inbodydescription([_|Paras])) -->
    !,
      foldl(par(U),Paras).
par(_U, briefdescription([])) -->
    !.
par(U, briefdescription([_|Paras])) -->
    !,
      foldl(par(U),Paras).
par(_U, detaileddescription([])) -->
    !.
par(U, detaileddescription([_|Paras])) -->
    !,
    foldl(par(U),Paras).
%:- start_low_level_trace.
par(U,emphasis([[]|Text])) -->
    !,
    cstr(`_`),
    foldl(par(U),Text),
    cstr(`_`).
par(U,computeroutput([_|Text])) -->
    !,
    cstr(`\``),
    foldl(par(U),Text),
    cstr(`\``).
par(U,verbatim([[]|Text])) -->
    !,
    cstr(`\``),
    foldl(par(U),Text),
    cstr(`\``).
par(U,bold([[]|Text])) -->
    !,
    cstr(`*`),
    foldl(par(U),Text),
    cstr(`*`).
par(U,anchor([[_Id]|Text])) -->
    !,
    foldl(par(U),Text).
par(_,linebreak(_)) -->
    !,
    cstr(`\n`).
par(U0,listitem([_|Text])) -->
    !,
    add_nl(U),
    {U is U0+4},
    cstr(`*`),
foldl(par(U),Text).
par(_U0,para([_,Seq]))-->
    {string(Seq)},
    !,
    cstr(Seq).
par(U0,para([_|Seq]))-->
    !,
        foldl(par(U0),Seq).
par(U0,simplesect([_,para([_|Seq])])) -->
    !,
    foldl(par(U0),Seq).
par(U0,xrefsect([_|Seq])) -->
    !,
    foldl(par(U0),Seq).
par(U0,xrefdescription([_|Seq])) -->
    !,
    foldl(par(U0),Seq).
par(U0,xreftitle([[],Seq])) -->
    !,
    foldl(par(U0),Seq).
par(U0, programlisting([_|L])) -->
    !,
    mcstr([`\n\`\`\`\n`]),
    foldl(codeline(U0),L),
    mcstr([`\n\`\`\`\n`]).
par(U0, itemizedlist([[]|L])) -->
    !,
    foldl(item(U0),L).
par(U0,orderedlist([[]|L])) -->
    !,
    foldl(oitem(U0),L).
par(_U0, parameterlist(_)) -->
    !.
par(_U0, bodystart(_)) -->
    !.
par(_U0, bodylist(_)) -->
    !.
par(_U0, bodyend(_)) -->
    !.
par(_U0, enumvalue(_)) -->
    !.
par(U0, codeline([_|Par])) -->
    !,
    foldl(cline(U0),Par).
par(U0,highlight([_|Seq])) -->
    !,
    foldl(par(U0),Seq).
par(U0, par(_,Par)) -->
    !,
    par(U0,Par).
/*    foldl(parameteritem(U0),L,S0,SF).
par(U, parameterlist([[_|_]|Seq])) -->
    foldl(par(U),Seq,S0,SF).
*/
par(_U, ref([[refid(R)|_],Name])) -->
    { string(Name) },
    !,
    mcstr([`[`,Name,`](#`,R,`)`]).
par(_U, ref([[refid(R)|_T]|Name])) -->
    !,
    { atom_string(Name,SName) },
    mcstr([`[`,SName,`](#`,R,`)`]).
par(_U0, sp(_)) -->
    !,
    cstr(`.`).
par(U0, blockquote([_|Pars])) -->
    !,
    cstr(`\``),
    foldl(par(U0),Pars),
    cstr(`\``).
par(_U0, A) -->
    { string(A) },
    !,
     cstr(A).
par(_U0, A) -->
    { atom(A) },
    !,
    { atom_string(A,S) },
    cstr(S).
par(_U0, A) -->
    { number(A) },
    !,
    { number_string(A,S) },
    cstr(S).
par(_,Task) --> {writeln(par:Task)}.

item(U0,listitem([[]|Seq]),S0,SF) :-
    string_concat(S0,`- `,S1),
    U is U0+4,
    foldl(par(U),Seq,S1,SF).

oitem(U0,listitem([[]|Seq]),S0,SF) :-
    string_concat(S0,`1. `,S1),
    U is U0+4,
    foldl(par(U),Seq,S1,SF).

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
    foldl(par(U0),Codes,S0,SF).
cline(_,S,S0,SF) :-
    !,
    string_concat(S0,S,SF).

add_space(0,S,S) :-
    !.
add_space(N,S0,SF) :-
    string_concat(S0,` `,S1),
    N1 is N-1,
    add_space(N1,S1,SF).

add_nl(U,S0,SF) :-
    add_space(U,S0,SI),
    string_concat(SI,`\n\n`,SF).

merge_nodes :-
    group(Id,_Name,File,Line,Column,Brief,Text,Title),
    atomic_concat(['docs/',Id,'.md'],F),
    open(F,write,S),
 format(S,'# ~s~n~n~s~n',[Title,Brief]),
    (group(Id,_,_)
      ->
	  format(S,'## Summary~n~n### SubGroups~n~n' ,[]),
     forall(group(Id,Ref,_),addsubg(S,Ref))

    ;
      true
    ),
    (pred(Id,_,_)
    ->
    format(S,'### Predicates ~n~n', []), 
    format(S,'|Predicate      |Description              |~n', []), 
    format(S,'|:----------------|----------------------:|~n', []), 
    forall(pred(Id,Ref,_),addsubp(S,Ref))						     ;
      true
      ),
			
    format(S,'~n*~s*~n~n~s~n',[Brief,Text]),
    forall(extra(Id,Extra),format(S,'~s~n',[Extra])),
    forall(pred(Id,Ref,_),output_pred(S,Ref)),
    footer(S,File,Line,Column),
    close(S),
    fail.
merge_nodes.


preds(Id, S) :-
(
pred(Id,_,_)
    ->
    format(S,'## List of Predicates~n~n',[])
    ;
    true
    ).

addsubg(S,Id) :-
    group(Id,_Name,_File,_Line,_Column,Brief,_Text, Title),
    strip_late_blanks(Brief,Brieffer),
  format(S,'1. [*~s*](~s.md).          ~s~n',[Title,Id,Brieffer]).

addsubp(S,Id) :-
    predicate(Id,Name,_File,_Line,_Column,Brief,_Text,_Members),
    strip_late_blanks(Brief,Brieffer),
    format(S,'|*[~s](#~s)*      |    ~s|~n',[Name,Id,Brieffer]).
    		 
strip_late_blanks(Brief,Brieffer) :-
    string(Brief),
    sub_string(Brief,Brief1,1,0,C),
    string_codes(C,[SC]),
    code_type_white(SC),
    !,
    sub_string(Brief,0,Brief1,1,Brieffie),
    strip_late_blanks(Brieffie,Brieffer).
strip_late_blanks(Brief,Brieffer) :-
    atom(Brief),
    sub_atom(Brief,Brief1,1,0,C),
    atom_codes(C,[SC]),
    code_type_white(SC),
    !,
    sub_atom(Brief,0,Brief1,1,Brieffie),
    strip_late_blanks(Brieffie,Brieffer).
strip_late_blanks(Brief,Brief).

output_pred(S,Id) :-
    (predicate(Id,Name,_F,_L,_C,Brief,Text,_)),
    format(S,'~n### ~s          {#~s}~n~s~n',[Name,Id,Brief]),	

    format(S,'~n~n~s~n',[Text]).

footer(_S,_,_,_).


																
