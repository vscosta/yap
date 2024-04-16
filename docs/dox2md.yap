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
run_task(compounddef([[id(Ref),kind(`group`)],compoundname([[],Name])|_])) -->
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
    once(foldl(par(0,Descriptor),Info,``,Text0)),
    !,
    arg(7,Descriptor,Text),
%    add_tail(LText,Text),
    ( var(Text) -> Text = Text0 ; true ),
    assert( Descriptor  ).
fetch(group(Ref,Name)) :-
    \+ visited(Ref),
    assert(visited(Ref)),
    atom_concat(['../xml/',Ref,'.xml'],G),
    catch(load_xml(G,[doxygen([_|Info])]),_,fail),
    functor(Descriptor,group,8),
    arg(1,Descriptor,Ref),
    arg(2,Descriptor,Name),
    once(foldl(par(0,Descriptor),Info,``,Text0)),
    arg(6,Descriptor,Brief),
    ( var(Brief) -> Brief = `` ; true ),
    arg(7,Descriptor,Text),
    ( var(Text) ->Text = Text0 ; true ),	
    arg(8,Descriptor,Title),
    ( var(Title) -> Title = Name ; true ),
    writeln(Name),
    
    !,
   assert( Descriptor).

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


    

cstr(A,S0,SF) :-
    string_concat(S0,A, SF).

mcstr(A,S0,SF) :-
    string_concat([S0|A], SF).

rpar(U,Ts) -->
    foldl(par(U),Ts),
         cstr(`\n`).

%par(_U0,_,C)--> {writeln(xxxxxxxxxxxxxxx:C), fail}.
par(U0,Info,sectiondef([_|Paras])) -->
    foldl(par(U0,Info),Paras),
    add_nl(0).
par(U0,Info,sect1([[id(Id)],title([[],Title])|Paras])) -->
    mcstr([`## `,Title,`               {#`,Id,`};`]),
    add_nl(U0),
    foldl(par(U0,Info),Paras),
    add_nl(0).
par(U0,Info,sect2([[id(Id)],title([[],Title])|Paras])) -->
    !,
    mcstr([`### `,Title,`               {#`,Id,`};`]),
    add_nl(U0),
    foldl(par(U0,Info),Paras),
    add_nl(0).
par(U0,Info,compounddef([_|Ps])) -->
    !,
   foldl(par(U0,Info),Ps) ,
    add_nl(0).
par(_U0,Info,innergroup([[refid(Ref)],Name])) -->
    !,
    {arg(1,Info,Ref0),
    assert(group(Ref0,Ref,Name))}.
par(_U0,Info,innerclass([[refid(Ref),_],Name])) -->
    !,
    {arg(1,Info,Ref0),
    assert(pred(Ref0,Ref,Name))}.

par(U0,Info,
	briefdescription([[]|Paras])) -->
    !,
    {arg(6,Info,Out),
    foldl(par(U0,Info),Paras,``,Out)},
    add_nl(0).
par(U0,Info,detaileddescription([[]|Paras]))-->
    {
      arg(7,Info, D0),
      foldl(par(U0,Info),Paras,``,Desc),
      (var(D0)->D0=Desc;arg(1,Info,Id),
	assert(extra(Id,Desc)))
      }.
par(_U0,GT,location([[file(File),line(Line),column(Column)|_]])) -->
    !,
    {
      arg(3,GT,File),
    arg(4,GT,Line),	
      arg(5,GT,Column)
      }.
par(_,GT,title([[],Title])) -->
     !,
     {arg(8,GT,Title) }.
par(_,_GT,initializer([[]|_L])) -->
    !.
par(_,_GT,listofallmembers([[]|_L])) -->
    !.
par(_,_GT,includes(_)) --> !.
par(_,_,memberdef([[kind(`variable`)|_Seq]|_]))-->
    !.
par(_,_,memberdef([[kind(`function`)|_Seq]|_]))-->
    !.
par(U0,Info,memberdef([_|Seq]))-->
    !,
    { U is U0+4 },
    cstr(`* ` ),
    foldl(par(U,Info),Seq),
    add_nl(U).
par(U,Info,lsquo(_)) -->
     !,
     par(U,Info,`\``).
par(_,_,param(_)) -->
!.	
par(_U0,_Info,compoundname(_)) -->
    !.
par(_U0,_Info,innerfile(_)) -->
    !.
par(U,Info,mdash(_)) -->
     !,
     par(U,Info,`-`).
par(U,Info,ndash(_)) -->
     !,
     par(U,Info,`-`).
par(_,_,true) -->
     !,
     cstr(`true`).
par(_,_,false) -->
     !,
    cstr(`false`).
par(_,_,ulink([[url(_)],_Text])) -->
     !.
par(_U0,_Info,sectiondef([[kind(`func`)|_]|_])) -->
    !.
par(_U0,_Info,sectiondef([[kind(`define`)|_]|_])) -->
    !.
par(_U0,_Info,sectiondef([[kind(`def`)|_]|_])) -->
    !.
par(_U0,_Info,sectiondef([[kind(`enum`)|_]|_])) -->
    !.
    par(_U0,_Info,sectiondef([[kind(`struct`)|_]|_])) -->
    !.
par(_U0,_Info,sectiondef([[kind(`typedef`)|_]|_])) -->
    !.
par(U,Info,sectiondef([[kind(_)]|Text])) -->
    !,
    add_nl(U),
    foldl(par(U,Info),Text).
par(_,_,rsquo(_)) -->
     !,
     cstr(`\'`),
    add_nl(0).
par(_,_,zwj(_)) -->
     !.
par(U,_Info, definition([_,Name])) -->
    cstr(Name),
    !,
    cstr(`:`),
    add_nl(U).
par(_U,_Info, argsstring(_)) -->
    !.
par(_U,_Info, initializer(_)) -->
    !.
par(_U,_Info, name(_)) -->
    !.
par(_U, _Info, type(_)) -->
    !.
par(_U, _Info, location(_)) -->
    !.
par(_U, _Info, inbodydescription([])) -->
    !.
par(U, Info, inbodydescription([_|Paras])) -->
    !,
      foldl(par(U,Info),Paras),
    add_nl(0).
par(_U, _Info, briefdescription([])) -->
    !.
par(U,Info, briefdescription([_|Paras])) -->
    !,
      foldl(par(U,Info),Paras),
    add_nl(0).
par(_U, _Info, detaileddescription([])) -->
    !,
    add_nl(0).
par(U,Info, detaileddescription([_|Paras])) -->
    !,
    foldl(par(U,Info),Paras),
    add_nl(0).
%:- start_low_level_trace.
par(U,Info,emphasis([[]|Text])) -->
    !,
    cstr(`_`),
    foldl(par(U,Info),Text),
    cstr(`_`).
par(U,Info,computeroutput([_|Text])) -->
    !,
    cstr(`\``),
    foldl(par(U,Info),Text),
    cstr(`\``).
par(U,Info,verbatim([[]|Text])) -->
    !,
    cstr(`\``),
    foldl(par(U,Info),Text),
    cstr(`\``).
par(U,Info,bold([[]|Text])) -->
    !,
    cstr(`*`),
    foldl(par(U,Info),Text),
    cstr(`*`).
par(U,Info,anchor([[_Id]|Text])) -->
    !,
    foldl(par(U,Info),Text).
par(_,_,linebreak(_)) -->
    !,
    cstr(`\n`).
par(U0,Item,listitem([_|Text])) -->
    !,
    {U is U0+4},
    add_nl(U),
    cstr(`*`),
    foldl(par(U,Item),Text).
par(_U0,_,para([_,Seq]))-->
    {string(Seq)},
    !,
    cstr(Seq),
    cstr(`.`),
    add_nl(0).
par(U0,Item,para([_|Seq]))-->
    !,
        foldl(par(U0,Item),Seq),
    cstr(`.`),
	add_nl(0).
par(U0,Item,simplesect([_,para([_|Seq])])) -->
    !,
    foldl(par(U0,Item),Seq),
    cstr(`.`),
    add_nl(0).
par(U0,Item,xrefsect([[id(ID)],xreftitle([[],Title]),xrefdescription([[]|Seq])])) -->
    !,
    add_nl(U0),
    {foldl(par(U0,Item),Seq,``,Text) },
    mcstr([`[`,Title,`](#`,ID,`)      `,Text]).
par(U0,Item,xrefdescription([_|Seq])) -->
    !,
    foldl(par(U0,Item),Seq).
par(U0,Item,xreftitle([[],Seq])) -->
    !,
    foldl(par(U0,Item),Seq).
par(U0,Item, programlisting([_|L])) -->
    !,
    mcstr([`\n\`\`\`\n`]),
    foldl(codeline(U0,Item),L),
    mcstr([`\n\`\`\`\n`]),
    add_nl(0).
par(U0,Item, itemizedlist([[]|L])) -->
    !,
    foldl(item(U0,Item),L).
par(U0,Item,orderedlist([[]|L])) -->
    !,
    foldl(oitem(U0,Item),L).
par(_U0, _, parameterlist(_)) -->
    !.
par(_U0, _, bodystart(_)) -->
    !.
par(_U0, _, bodylist(_)) -->
    !.
par(_U0, _, bodyend(_)) -->
    !.
par(_U0, _, enumvalue(_)) -->
    !.
par(U0,Item, codeline([_|Par])) -->
    !,
    foldl(cline(U0,Item),Par).
par(U0,Item,highlight([_|Seq])) -->
    !,
    foldl(par(U0,Item),Seq).
par(U0,Item, par(_,Par)) -->
    !,
    par(U0,Item,Par).
             /*    foldl(parameteritem(U0),L,S0,SF).
par(U,Info, parameterlist([[_|_]|Seq])) -->
    foldl(par(U),Seq,S0,SF).
*/
par(_U,_Info, ref([[refid(R)|_],Name])) -->
    { string(Name) },
    !,
    mcstr([`[`,Name,`](#`,R,`)`]).
par(_U,_, ref([[refid(R)|_T]|Name])) -->
    !,
    { atom_string(Name,SName) },
    mcstr([`[`,SName,`](#`,R,`)`]).
par(_U0,_, sp(_)) -->
    !,
    cstr(`.`).
par(U0,Item, blockquote([_|Pars])) -->
    !,
    cstr(`\``),
    foldl(par(U0,Item),Pars),
    cstr(`\``).
par(_U0,_, A) -->
    { string(A) },
    !,
     cstr(A).
par(_U0,_, A) -->
    { atom(A) },
    !,
    { atom_string(A,S) },
    cstr(S).
par(_U0, _,A) -->
    { number(A) },
    !,
    { number_string(A,S) },
    cstr(S).
par(_,_,Task) --> {writeln(par:Task)}.

item(U0,Item,listitem([[]|Seq]),S0,SF) :-
    string_concat(S0,`- `,S1),
    U is U0+4,
    foldl(par(U,Item),Seq,S1,SF).

oitem(U0,Item,listitem([[]|Seq]),S0,SF) :-
    string_concat(S0,`1. `,S1),
    U is U0+4,
    foldl(par(U,Item),Seq,S1,SF).

parameteritem(_U0,_Item,parameteritem([[]|_Seq]),S,S) :-
    !.
codeline(U0,Item,codeline([_|Seq]),S0,SF) :-
    !,
    foldl(cline(U0,Item),Seq,S0,SF).
codeline(_U0,_Item,Seq,S0,SF) :-
    string(Seq),
    string_concat(S0,Seq,SF).

cline(_,_,sp(_),S,S) :-
    !.
cline(U0,Item,highlight([_|Codes]),S0,SF) :-
    !,
    foldl(par(U0,Item),Codes,S0,SF).
cline(_,_,S,S0,SF) :-
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
    (stream_property(S0,[alias(group)])->close(S0);true),
    open(F,write,S,[alias(group)]),
 format(S,'# ~s~n~n~s~n',[Title,Brief]),
    writeln(Name),
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
    char_type_white(C),
    !,
    sub_atom(Brief,0,Brief1,1,Brieffie),
    strip_late_blanks(Brieffie,Brieffer).
strip_late_blanks(Brief,Brief).

output_pred(S,Id) :-
    (predicate(Id,Name,_F,_L,_C,Brief,Text,_)),
    format(S,'~n### ~s          {#~s}~n~s~n',[Name,Id,Brief]),	

    format(S,'~n~n~s~n',[Text]).

footer(_S,_,_,_).


																
