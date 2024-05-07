:- use_module(library(maplist)).
:- use_module(library(lists)).
:- use_module(library(xml2yap)).

%:- dynamic group/8, predicate/8, extra/2, pred/8, pred/3, show/0.

:- dynamic group/2, predicate/2, class/2.

add_group(A,B,C) --> {retract(group(A,``)),assert(group(A,B))}, [group(A,C)].
add_group(A,_B,_C) --> {group(A,_)}, !.
add_group(A,B,C) --> {assert(group(A,B))}, [group(A,C)].

add_predicate(A,B,_C) --> {predicate(A,B)}, !.
add_predicate(A,B,C) --> {assert(predicate(A,B))}, [predicate(A,C)].

add_class(A,B,_C) --> {class(A,B)}, !.
add_class(A,B,C) --> {assert(class(A,B))}, [clas(sA,C)].

main :-
    unix(argv([Input,_Output])),
    atom_concat(Input,'/index.xml',Index),
    xml_load(Index,[doxygenindex(XMLTasks)]),
    run_xml(XMLTasks).

run_xml(XMLTasks) :-
    foldl( run_task( ``), XMLTasks, Tasks, []),
    !,
    maplist(fetch,Tasks),
    merge_nodes.

xml2tasks(Tasks, NewTasks) :-
    foldl( run_task(``), Tasks, NewTasks, []).


run_task(_,compound(Compound)) --> {writeln(Compound),fail}.

run_task(_P,compound([[refid(_),kind(`concept`)],name([[]])])) --> !.
run_task(P,compound([[refid(Ref),kind(`group`)|_],name([[],Name])|_Members])) -->
!,
    add_group(Ref,P,Name),
%    members(Members, Ref),
    sub_tasks(Ref).

run_task(_P,compound([[refid(_Ref),kind(`predicate`)|_],name([[],_Name])])) -->
!.
%run_task(_P,compound([[refid(_Ref),kind(`class`)|_],name([[],_Name])])) -->
%!.
run_task(_,compound(_)) -->
    !.
run_task(P,compounddef([[id(Ref),Kind|More],compoundname([[],Name])|Members])) -->
    !,
    run_task(P,compound([[refid(Ref),Kind|More],name([[],Name])|Members])).
run_task(_,location([file(File),
				   line(Line),
				   column(_),
				   bodyfile(Source),
				   bodystart(_),
				   bodyend(_)])) -->
    {assert_static(location(File,Line,Source))}.
run_task(_,['xmlns:xsi'(_)|_]) --> !.
run_task(_,Task) --> {writeln(task:Task),fail}.

sub_tasks(Ref) -->
    { unix(argv([Input,_Output])),
    atom_concat([Input,'/',Ref,'.xml'],G),
    catch(load_xml(G,[doxygen([_|Info])]),_,fail),
    Info = [compounddef([_,_,_|Inner])]
	 },
    foldl(inner_tasks(Ref),Inner).

inner_tasks(Parent,sectiondef([_|L])) -->
!,
foldl(inner_tasks(Parent),L).
inner_tasks(Parent,innerclass([[refid(Ref)|_],Name|_])) -->
    {  sub_string(Ref,0,_,_,`predicate`) },
    !,
       add_predicate(Ref,Parent,Name).
inner_tasks(Parent,innerclass([[refid(Ref)|_],Name|_])) -->
    {  sub_string(Ref,0,_,_,`class`) },
    !,
       add_class(Ref,Parent,Name).
inner_tasks(Parent,innergroup([[refid(Ref)|_],Name|_])) -->
!,
    add_group(Ref,Parent,Name).
inner_tasks(_,_) --> [].


%members([M|_], _FromType, FromRef) -->
%    {writeln(M),fail}.

members([],_) --> [].
members([member([[refid(Ref),kind(Kind)],name([[],Name])|InnerMembers])|Members], FromRef) -->
    %{writeln(Name:InnerMembers)},
    ({Kind==`predicate`} -> add_predicate(Ref,FromRef,Name) ;
     {Kind==`class`} ->add_class(Ref,FromRef,Name),
     {Kind==`group`} ->add_group(Ref,FromRef,Name)
    ),
	!,
   members(InnerMembers,Ref),
     members(Members,FromRef).
members([_|Members],FromRef)-->		 
     members(Members,FromRef).
		 
    
%[`concept`,`define`,`dir`,`enum`,`enumvalue`,`file`,`function`,`group`,`namespace`,`page`,`predicate`,`struct`,`typedef`,`union`,`variable`]

:- dynamic visited/1.
fetch(T) :- writeln(T),fail.
fetch(T) :-
    visited(T),
    !.
fetch(T) :-
    assert_static(visited(T)),
%    writeln(T),
    fail.
fetch(class(Ref,Name)) :-
    !,
    functor(Descriptor,class,8),
     fill(Descriptor,Ref,Name).
fetch(concept(Ref,Name)) :-
    sub_string(Name,_,1,D,`/`),
     sub_string(Name,_,D,0,N),
     string_number(N,_),
    functor(Descriptor,predicate,8),
    fill(Descriptor,Ref,Name ),
    !.
fetch(predicate(Ref,Name)) :-
    !,
    functor(Descriptor,predicate,8),
     fill(Descriptor,Ref,Name).
fetch(group(Ref,Name)) :-
    !,
    functor(Descriptor,group,8),
    fill(Descriptor,Ref,Name).
/*
fetch(enum(Ref,Name)) :-
    functor(Descriptor,enum,8),
     fill(Descriptor,Ref,Name ).
fetch(struct(Ref,Name)) :-
    functor(Descriptor,struct,8),
     fill(Descriptor,Ref,Name).
fetch(union(Ref,Name)) :-
    functor(Descriptor,union,8),
    fill(Descriptor,Ref,Name).
*/
fetch(_).

fill(Descriptor,Ref,Name) :-
    unix(argv([Input,_Output])),
    string_atom(Ref,ARef),
    atom_concat([Input,'/',ARef,'.xml'],G),
    catch(load_xml(G,[doxygen([_|Info])]),_,fail),
    !,
    arg(1,Descriptor,Ref),
    arg(2,Descriptor,Name),
        ( var(Name) ->Text = Ref ; true ),	
    once(foldl(top_par(0,Descriptor),Info,``,Text0)),
    arg(6,Descriptor,Brief),
    ( var(Brief) -> Brief = `` ; true ),
    arg(7,Descriptor,Text),
    ( var(Text) ->Text = Text0 ; true ),	
    arg(8,Descriptor,Title),
    ( var(Title) -> Title = Name ; true ),    
    !,
   assert_static( Descriptor).
fill(Descriptor,Ref,Name) :-
    functor(Descriptor,N,_),
    writeln(N+Name+Ref+not_found).
    
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

top_par(U0,Info,C)--> %{functor(C,N,_), writeln(xxxxxxxxxxxxxxx:N), fail},
		      ( par(U0,Info,C) -> [] ; {writeln(C)} ).
par(U0,Info,sectiondef([_|Paras])) -->
    foldl(par(U0,Info),Paras),
    add_nl(0).
par(_U0,_Info,collaborationgraph(_)) -->
    !.
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
par(U0,Info,sect3([[id(Id)],title([[],Title])|Paras])) -->
    !,
    mcstr([`#### `,Title,`               {#`,Id,`};`]),
    add_nl(U0),
    foldl(par(U0,Info),Paras),
    add_nl(0).
par(_U0,Info,innergroup([[refid(Ref)],Name])) -->
    { arg(1, Info, Parent) },
    !,
	 add_group(Ref,Parent,Name).
par(_U0,Info,innerclass([[refid(Ref),_],Name])) -->
    {  sub_string(Ref,0,_,_,`predicate`) },
       !,
       {  arg(1, Info, Parent)},
	 add_predicate(Ref,Parent,Name).
par(_U0,Info,innerclass([[refid(Ref),_],Name])) -->
    {  sub_string(Ref,0,_,_,`class`) },
       !,
       {  arg(1, Info, Parent)},
	 add_class(Ref,Parent,Name).
par(_U0,_Info,innerclass([[refid(_Ref),_],_Name])) -->
!.
par(_U0,_Info,image([[type(`html`),name(File),alt(Alt),inline(true)]])) -->
	!,
	mcstr([`![`,Alt,`](`,File,`)`]).
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
	assert_static(extra(Id,Desc)))
      }.
par(U0,Info,detaileddescription([[]|Paras]))-->
    {
      arg(7,Info, D0),
      foldl(par(U0,Info),Paras,``,Desc),
      (var(D0)->D0=Desc;arg(1,Info,Id),
	assert_static(extra(Id,Desc)))
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
par(U0,Info,compounddef([_|Par])) -->
    !,
    foldl(par(U0,Info),Par  ).
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
par(U0,Item, codeline(P)) -->
    !,
    {P = [_Head|Par]},
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
    cstr(` `).
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

codeline(U0,_Item,Seq) -->
    {string(Seq)},
    !,
    cstr(Seq),
    add_nl(U0).
codeline(U0,Item,codeline([_|Seq]))-->
    !,
    foldl(cline(U0,Item),Seq),
    add_nl(U0).

cline(_,_,Codes) -->
    {string(Codes)},
    !,
    cstr(Codes).
cline(_,_,sp(_)) -->
    !,
    cstr(` `).
cline(_U0,_Item,highlight([[class(_)],sp(_)])) -->
    !,
    cstr(` `).
cline(_U0,_Item,highlight([[class(_)],Codes])) -->
    {string(Codes)},
    !,
    cstr(Codes).
 cline(U0,Item,highlight([_|Codes])) -->
    !,
    foldl(par(U0,Item),Codes).
cline(_,_,_S) -->
    !.

add_space(0,S,S) :-
    !.
add_space(N,S0,SF) :-
    string_concat(S0,` `,S1),
    N1 is N-1,
    add_space(N1,S1,SF).

add_nl(U,S0,SF) :-
    add_space(U,S0,SI),
    string_concat(SI,`\n\n`,SF).

is_parent(P) :-
    group(_Id,P).

grp(Id,Name) :-
    group(Id,Name,_File,_Line,_Column,_Brief,_Text,_Title).
    
merge_nodes :-    
    unix(argv([_Input,Output])),
    grp(Id,_Name),
    ( stream_property(S0,alias(group)) -> close(S0) ; true ),
    atomic_concat([Output,'/',Id,'.md'],F),
    open(F,write,S,[alias(group)]),
%    writeln(F),
    catch( one_group(S,Id), _, true),
    close(S),
    fail.
 
one_group(S,Id) :-
    group(Id,_Name,File,Line,Column,Brief,Text,Title),
 format(S,'# ~s~n~n~s~n',[Title,Brief]),
    (is_parent(Id)
      ->
	  format(S,'## Summary~n~n### SubGroups~n~n' ,[]),
     forall(group(Ref,Id),(addsubg(S,Ref)))

    ;
      true
    ),
    (predicate(_,Id)
    ->
    format(S,'## Predicates~n~n', []), 
    format(S,'|Predicate~20|| Description~40+|~n', []), 
    format(S,'|~`-t~20||~`-t~40+|~n', []), 
    forall(predicate(Ref,Id),(addsubp(S,Ref)))						     ;
      true
      ),
    (class(_,Id)
    ->
    format(S,'## Class~n', []), 
    format(S,'|Class~20|| Description~40+|~n', []), 
    format(S,'|~`-t~20||~`-t~40+|~n', []), 
    forall(class(Ref,Id),(addsubc(S,Ref)))						     ;
      true
      ),
			
    format(S,'~n*~s*~n~n~s~n',[Brief,Text]),
    forall(extra(Id,Extra),format(S,'~s~n',[Extra])),
    (predicate(_,Id)
    ->
    forall(predicate(Ref,Id),(output_predicate(S,Ref)))
      ;
      true
      ),
    (class(_,Id)
    ->
    forall(class(Ref,Id),(output_class(S,Ref)))
      ;
      true
      ),
    footer(S,File,Line,Column),
    !.
one_group(_,_).

preds(Id, S) :-
(
predicate(_,Id)
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
    predicate(Id,Name,_File,_Line,_Column,Brief,_Text,_Title),
    strip_late_blanks(Brief,Brieffer),
    format(S,'|*[~s](#~s)*      |    ~s|~n',[Name,Id,Brieffer]).

addsubc(S,Id) :-
    class(Id,Name,_File,_Line,_Column,Brief,_Text,_Title),
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

output_predicate(S,Id) :-
    predicate(Id,Name,_F,_L,_C,Brief,Text,_),
    format(S,'~n### ~s          {#~s}~n~s~n',[Name,Id,Brief]),	

    format(S,'~n~n~s~n',[Text]).

output_class(S,Id) :-
    class(Id,Name,_F,_L,_C,Brief,Text,_),
    format(S,'~n### ~s          {#~s}~n~s~n',[Name,Id,Brief]),	

    format(S,'~n~n~s~n',[Text]).

footer(_S,_,_,_).

kinds :-
    unix(argv([Input,_Output])),
    atom_concat(Input,'/index.xml',Index),
    xml_load(Index,[doxygenindex(XMLTasks)]),
    setof(Kind, kind_in(XMLTasks,Kind), Kinds),
    writeln(Kinds).

kind_in([A|_B], K) :-
    kind_in(A, K).
kind_in([_A|B], K) :-
    !,
    kind_in(B, K).
kind_in(kind(A), A) :- !.
kind_in(S, A) :-
    S=..[_,B],
    kind_in(B, A).
