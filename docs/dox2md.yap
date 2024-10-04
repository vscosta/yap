:- use_module(library(maplist)).
:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(xml2yap)).

%:- dynamic group/8, predicate/8, pred/8, pred/3, show/0.

:- multifile extrabrief/2.
:- dynamic group/2, predicate/2, class/2, subclass/2.

main :-
    unix(argv([Input,_Output])),
    atom_concat(Input,'/index.xml',Index),
    xml_load(Index,[doxygenindex([_|XMLTasks])]),
    xml(XMLTasks).

xml(XMLTasks) :-
    foldl( run_task( ``), XMLTasks, Tasks0, []),
    sort(Tasks0,Tasks),
    !,
    maplist(fetch,Tasks),
    merge_nodes.

add_task(Kind, A,B,C) -->
    { string_atom(Kind,Atom),
    Linkage =.. [Atom, A, B],
    AnyLinkage =.. [Atom,A,_],
    BaseLinkage =.. [Atom, A, ``],
    Name =.. [Atom,A,C],
      Linkage =.. [Atom,A,B] },
    add_kind_of_task(Name, Linkage, BaseLinkage,AnyLinkage).

extra_task(Kind,Ref,Parent,Name) -->
    { add_task(Kind,Ref,Parent,Name,[], Tasks),
      Tasks = [New]
      ->
      fetch(New)
;
true
}.

add_kind_of_task(Name, Linkage, BaseLinkage,_) -->
 {retract(BaseLinkage),assert(Linkage)},
[Name].
add_kind_of_task(_,_,_,AnyLinkage) --> {AnyLinkage}, !.
add_kind_of_task(Name,Linkage,_,_) --> {assert(Linkage)}, [Name].

xml2tasks(Tasks, NewTasks) :-
    foldl( run_task(``), Tasks, NewTasks, []).



deref(Link, New) :-
    sub_string(Link,0,_,_,`group`),
    !,
    string_concat([Link,`.md`],New).
deref(Link, New) :-
    predicate(Link,_Group),
    !,
    string_concat([Link,`.md`],New).
deref(Link, New) :-
    class(Link,_Group),
    !,
    string_concat([Link,`.md`],New).
deref(Link, Link).


%run_task(_,compound(Compound)) --> {writeln(Compound),fail}.
%    members(Members, Ref),

run_task(P,compound([[refid(Ref),kind(Kind)|_],name([[],Name])|_Members])) -->
    !,
    add_task(Kind,Ref,P,Name),
    sub_tasks(Kind,Ref).
run_task(_,compound(_)) -->
    !.
run_task(P,compounddef([[id(Ref),Kind|More]|Members])) -->
    !,
    run_task(P,compound([[refid(Ref),Kind|More]|Members])).
run_task(_,location(_)) -->
    !.
run_task(_,['xmlns:xsi'(_)|_]) --> !.
run_task(_,Task) --> {writeln(task:Task),fail}.

sub_tasks(`group`,Ref) -->
    { unix(argv([Input,_Output])),
    atom_concat([Input,'/',Ref,'.xml'],G),
    catch(load_xml(G,[doxygen([_|Info])]),_,fail),
    Info = [compounddef([_,_,_|Inner])]
	 },
    foldl(inner_tasks(Ref),Inner).
sub_tasks(_,_Ref) --> !.


inner_tasks(Parent,sectiondef([_|L])) -->
    !,
    foldl(inner_tasks(Parent),L).
inner_tasks(Parent,innerclass([[refid(Ref)|_],Name|_])) -->
    {  sub_string(Ref,0,_,_,`predicate`) },
    !,
       add_task(`predicate`,Ref,Parent,Name).
inner_tasks(Parent,innerclass([[refid(Ref)|_],Name|_])) -->
    {  sub_string(Ref,0,_,_,`concept`) },
    !,
       add_task(`concept`,Ref,Parent,Name).
inner_tasks(Parent,innerclass([[refid(Ref)|_],Name|_])) -->
    {  sub_string(Ref,0,_,_,`class`) },
    !,
       add_task(`class`,Ref,Parent,Name).
inner_tasks(Parent,innergroup([[refid(Ref)|_],Name|_])) -->
!,
    add_task(`group`,Ref,Parent,Name).
inner_tasks(_,_) --> [].


%members([M|_], _FromType, FromRef) -->
%    {writeln(M),fail}.

members([],_) --> [].
members([member([[refid(Ref),kind(Kind)],name([[],Name])|InnerMembers])|Members], FromRef) -->
    add_task(Kind,Ref,FromRef,Name),
    !,
    members(InnerMembers,Ref),
    members(Members,FromRef).
members([_|Members],FromRef)-->		 
     members(Members,FromRef).
		 
    

:- dynamic visited/1.
fetch(T) :-
    T=..[_,K,_L],
    (
    visited(K) -> !;
    assert(visited(K)),
writeln(K),
fail
    ).

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
/*fetch(enum(Ref,Name)) :-
    !,
    functor(Descriptor,enum,8),
     fill(Descriptor,Ref,Name ).
fetch(struct(Ref,Name)) :-
    !,
    functor(Descriptor,struct,8),
     fill(Descriptor,Ref,Name).
fetch(union(Ref,Name)) :-
    !,
    functor(Descriptor,union,8),
    fill(Descriptor,Ref,Name).
*/
fetch(_).

fill(Descriptor,Ref,Name) :-
    unix(argv([Input,_Output])),
    string_atom(Ref,ARef),
    atom_concat([Input,'/',ARef,'.xml'],G),
    catch(load_xml(G,[doxygen([_,compounddef([_|Info])|_])]),_,fail),
    !,
    arg(7,Descriptor,``),
    arg(1,Descriptor,Ref),
    arg(2,Descriptor,Name),
 %       ( var(Name) ->Text = Ref ; true ),	
    ignore(foldl(top_par(0,Descriptor),Info,``,Text0)),
    arg(6,Descriptor,Brief),
    ( var(Brief) -> Brief = `` ; true ),
   assert(extrabrief(Ref,Text0)),
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
   catch( string_concat(S0,A, SF),_,(atomic_concat(S0,A,SA),atom_string(SA,SF))).

mcstr(A,S0,SF) :-
     string_concat([S0|A], SF).

rpar(U,Ts) -->
    foldl(par(U,[]),Ts),
         cstr(`\n`).

top_par(U0,_Info,C) -->
%    {writeln(C)},
    par(U0,_Info,C).
top_par(_U0,_Info,C) -->
    { writeln(failed:C) }.

% par(U0,_Info,C) -->
%     {writeln(C),
%     fail}.
par(_,_,[]) --> !.
par(U0,Info,sectiondef([[kind(`var`)|_]|Paras])) -->
    !,
    {
    arg(1,Info,Id),
    foldl(var_member(U0,Info),Paras, ``, Desc),
    assert_static(v(Id,Desc))
      }.
par(U0,Info,sectiondef([[kind(`friend`)|_]|Paras])) -->
    !,
    {
    arg(1,Info,Id),
format(string(S), `\n#### Friends:\n`, []),
    foldl(friend_member(U0,Info),Paras, S, Desc),
    assert(v(Id,Desc))
      }.
par(U0,Info,sectiondef([[kind(Kind)|_Opts]|Paras])) -->
    !,{
arg(1,Info,Id),
string_concat([`\n#### `,Kind, `.\n`], S0),
	foldl(function_member(U0,Info),Paras, S0, Desc),
    assert(f(Id,Desc))
      }.
par(U0,Info,sectiondef([[kind(`public-func`)|_Opts]|Paras])) -->
    !,{
arg(1,Info,Id),
	foldl(function_member(U0,Info),Paras, `\n##### Public Functions:\n`, Desc),
    assert(f(Id,Desc))
      }.
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
    foldl(par(U,Info),Text).
par(_U0,_Info,collaborationgraph(_)) -->
    !.
par(_U0,_Info,reimplements(_)) -->
    !.
par(_U0,_Info,reimplementedby(_)) -->
    !.
par(_U0,_Info,inheritancegraph(_)) -->
    !.
par(_U0,_Info,bitfield(_)) -->
    !.
par(_U0,_Info,exceptions(_)) -->
    !.
par(_U0,_Info,initializexbr(_)) -->
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
	 extra_task(`group`,Ref,Parent,Name).
par(_U0,_Info,ref([[refid(`structF`),kindref(`compound`)],false ])) -->
    !.
par(_U0,_Info,basecompoundref([[_|_],_])) -->
    !.
par(_U0,Info,innerclass([[refid(Ref),_],Name])) -->
    {  sub_string(Ref,0,_,_,`predicate`) },
       !,
       {  arg(1, Info, Parent)},
	 extra_task(`predicate`,Ref,Parent,Name).
par(_U0,Info,innerclass([[refid(Ref),_],Name])) -->
    {  sub_string(Ref,0,_,_,`class`) },
       !,
       {  arg(1, Info, Parent)},
	 extra_task(`class`,Ref,Parent,Name).
par(_U0,_Info,innerclass([[refid(_Ref),_],_Name])) -->
!.
par(_U0,_Info,image([[type(`html`),name(File),alt(Alt),inline(true)]])) -->
	!,
	mcstr([`![`,Alt,`](`,File,`)`]).
par(_U, _Info, briefdescription([[]])) -->
    !.
par(U,Info, briefdescription([_|Paras])) -->
    !,
    foldl(par(U,Info),Paras),
    add_nl(0).
par(U0,Info,
	briefdescription([[]|Paras])) -->
    !,
    {arg(6,Info,D0) },
 foldl(par(U0,[]),Paras,``,Desc),
      {
(Desc == ``-> true
; var(D0)->D0=Desc;arg(1,Info,Id),
	assert(extra(Id,Desc)))
      }.
par(_U, _Info, inbodydescription([[]])) -->
    !.
par(U0,Info,
	inbodydescription([[]|Paras])) -->
    !,
    {arg(6,Info,D0) },
 foldl(par(U0,[]),Paras,``,Desc),
      {
(Desc == ``-> true
; var(D0)->D0=Desc;arg(1,Info,Id),
	assert(extra(Id,Desc)))
      }.
par(_U, _Info, detaileddescription([[]])) -->
    !.
par(U0,Info,detaileddescription([[]|Paras]))-->
    !,
    {
      arg(7,Info, D0),
      foldl(par(U0,[]),Paras,``,Desc),
      (
	  var(D0)
      ->
      D0=Desc;
	  true
      ),
      (Desc == ``  ->
	   true
       ;
       arg(1,Info,Id),
       assert_static(extra(Id,Desc))
      )
           }.
par(_U0,GT,location([[file(File),line(Line),column(Column)|_]])) -->
    {
      arg(3,GT,File),
    arg(4,GT,Line),	
      arg(5,GT,Column)
      },
    !.
par(_U0,_GT, location(_)) -->
    !.
par(_,[],title([[],_Title])) -->
     !.
par(_,GT,title([[],Title])) -->
     !,
     {arg(8,GT,Title) }.
par(_,_GT,initializer([[]|_L])) -->
    !.
par(_,_GT,listofallmembers([[]|_L])) -->
    !.
par(_,_GT,templateparamlist([[]|_L])) -->
    !.
par(_,_GT,includes(_)) --> !.
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
par(_,_,rsquo(_)) -->
     !,
     cstr(`\'`),
    add_nl(0).
par(_,_,zwj([[],Text])) -->
    !,
    cstr(Text).
par(_,_,zwj([[]])) -->
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
    mcstr([`[`,Title,`](`,ID,`)      `]),
    foldl(par(U0,Item),Seq).
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
par(U0,_Item, par(_,Par)) -->
    !,
    foldl(parameteritem(U0),Par).
par(U,_Info, parameterlist([[_|_]|Seq])) -->
    foldl(par(U),Seq).
par(_U,_Info,ref([[refid(`classT`),kindref(`compound`)],true])) -->
    !.
par(_U,_Info, ref([[refid(R)|_],Name])) -->
    { string(Name),
      deref(R,DR) },
    !,
    mcstr([`[`,Name,`](`,DR,`)`]).
par(_U,_Info, basecompoundref([[refid(R)|_],Name])) -->
    { string(Name),
      deref(R,DR) },
    !,
    mcstr([`[`,Name,`](`,DR,`#)`]).
par(_U,_Info, basecompoundref([[refid(R)|_],Name])) -->
    { string(Name),
      deref(R,DR) },
    !,
    mcstr([`[`,Name,`](`,DR,`#)`]).
par(_U,_Info, derivedcompoundref([_,Name])) -->
    { string(Name) },
!.
 par(_U,_Info, derivedcompoundref([[refid(R)|_],true])) -->
    { deref(R,DR) },
    !,
     mcstr([`[T](`,DR,`#)`]).
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
is_parent(P) :-
    class(_Id,P).

ent(Id,class,Name, File,Line,Column,Brief,Text,Title) :-
    class(Id,Name, File,Line,Column,Brief,Text,Title).
ent(Id,group,Name, File,Line,Column,Brief,Text,Title) :-
    group(Id,Name,File,Line,Column,Brief,Text,Title).
ent(Id,predicate,Name, File,Line,Column,Brief,Text,Title) :-
    predicate(Id,Name,File,Line,Column,Brief,Text,Title).
  
merge_nodes :-
    unix(argv([_Input,Output])),
    ent(Id,Type,Name, File,Line,Column,Brief,Text,Title),
    ( stream_property(S0,[alias(Type)]) -> close(S0) ; true ),
    atomic_concat([Output,'/',Id,'.md'],F),
    open(F,write,S,[alias(Type)]),
    ignore(one(Type,S,Id,Name, File,Line,Column,Brief,Text,Title)),
    close(S),
    fail.
 merge_nodes.

one(class,S,Id,_Name, File,Line,Column,Brief,Text,Title) :-
    format(S,'# ~s\n\n~s\n',[Title,Brief]),
   forall(extrabrief(Id,Extra),format(S,'~s\n',[Extra])),
    process_class(S,Id,File,Line,Column,Text).
one(predicate,S,Id, _Name, File,Line,Column,Brief,Text,Title) :-
    format(S,'# ~s\n\n~s\n',[Title,Brief]),
    forall(extrabrief(Id,Extra),format(S,'~s\n',[Extra])),
    process_predicate(S,Id,File,Line,Column,Text).
one(group, S,Id, _Name, File,Line,Column,Brief,Text,Title) :-
 format(S,'# ~s\n\n~s\n',[Title,Brief]),
 forall(extrabrief(Id,Extra),format(S,'~s\n',[Extra])),
 subgroups(S,Id),
process_group(S,Id,File,Line,Column,Text).

subgroups(S,Id) :-
    is_parent(Id),
     !,
    format(S,'### SubGroups\n\n' ,[]),
     forall(group(Ref,Id),(addsubg(S,Ref))).
subgroups(_S,_Id).

process_group(S,Id,_File,_Line,_Column,_Text) :-
    once(predicate(_,Id)),
    format(S,'## Predicates\n\n', []), 
    format(S,'|Predicate~t|~20|Description~t|~40|\n', []), 
    format(S,'|:---|:---~|\n', []), 
    forall(predicate(Ref,Id),(addsubp(S,Ref))),
    format(S,'\n\n',[]),
    fail.
    %format(S,'~s\n',[Brief]),

process_group(S,Id,_File,_Line,_Column,_Text) :-
    once(class(_,Id)),
    format(S,'## Classes\n\n', []), 
    format(S,'|Class~t|~20|Description~t|~40|\n', []), 
    format(S,'|:---|:---~|\n', []), 
    forall(class(Ref,Id),(addsubc(S,Ref))),
    format(S,'\n\n',[]),
    fail.
    %format(S,'~s\n',[Brief]),
process_group(S,Id,File,Line,Column,Text) :-
%    forall(predicate(Ref,Id),(output_predicate(S,Ref))),
    load_all_text(Id,Text,AllText),
    format(S,'~s',[AllText]),
     nl(S),
    footer(S,File,Line,Column).

process_predicate(S,Id,File,Line,Column,Text) :-
    %format(S,'~s\n',[Brief]),
    load_all_text(Id,Text,AllText),
    format(S,'~s',[AllText]),
    forall(predicate(Ref,Id),(output_predicate(S,Ref))),
   nl(S),
    footer(S,File,Line,Column).

process_class(S,GId,File,Line,Column,Text) :-
    load_all_text(GId,Text,AllText),
    format(S,'~s',[AllText]),
   nl(S),
   footer(S,File,Line,Column), !.
    
load_all_text(GId,Text,AllText) :-
    findall(T,collect_txt(GId,Text,T),Ts),
    maplist(strip_late_blanks,Ts,NTs),
    drop_dups(NTs,RTs),
    string_concat(RTs,AllText).

collect_txt(Id,_,Text) :-
    extra(Id,Text), Text \= ``.
collect_txt(Id,_,Text) :-
    v(Id,Text), Text \= ``.
collect_txt(Id,_,Text) :-
    f(Id,Text).

drop_dups([],[]).
drop_dups([X,X|L],NL) :-
    !,
    drop_dups([X|L],NL).
drop_dups([X|L],[X|NL]) :-
    drop_dups(L,NL).

preds(Id, S) :-
    (
    predicate(_,Id)
    ->
    format(S,'## List of Predicates\n',[])
    ;
    true
    ).

addsubg(S,Id) :-
    group(Id,_Name,_File,_Line,_Column,Brief,_Text, Title),
    strip_late_blanks(Brief,Brieffer),
  format(S,'###### [*~s*](~s.md).          ~s\n',[Title,Id,Brieffer]).

addsubp(S,Id) :-
    predicate(Id,_Name,_File,_Line,_Column,Brief,_Text,Title),
    strip_late_blanks(Brief,Brieffer),
    format(S,'|*[~s](~s.md)*      |    ~s|\n',[Title,Id,Brieffer]).

addsubc(S,Id) :-
    class(Id,_Name,_File,_Line,_Column,Brief,_Text,Title),
    strip_late_blanks(Brief,Brieffer),
    format(S,'|*[~s](~s.md)*      |    ~s|\n',[Title,Id,Brieffer]).

output_predicate(S,Id) :-
    predicate(Id,Name,_F,_L,_C,Brief,Text,_),
    format(S,'### ~s          {#~s}\n~s\n',[Name,Id,Brief]),	
    format(S,'\n~s\n',[Text]),
    forall(extra(Id,Extra) ,  format(S,'~s',[Extra])).

  
strip_late_blanks(``,``) :-
    !.
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


var_member(U0,Info,memberdef([[kind(_Kind),id(MyId)|_]|Text])) -->
{ member(definition([[],Def]), Text),
%member(name([[],Name]), Text),
!,
 rel_id(MyId,Info,Id),
      format(string(St),`######  ~s  {#~s}\n`,[Def,Id])
},
cstr(St),
add_comments(U0,Text).

add_comments(U,Text) -->
    {
member(briefdescription([[]|Brief]),Text),
member(inbodydescription([[]|InBody]),Text),
member(detaileddescription([[]|Detailed]),Text)
},
    rpar(U,Brief),
    rpar(U,InBody),
    rpar(U,Detailed).


friend_member(U0,Info,memberdef([[kind(_Kind),id(MyId)|_]|Text])) -->
{ member(definition([[],Def]), Text),
!,
rel_id(MyId,Info,Id),
      format(string(St),`###### ~s  {#~s}\n`,[Def,Id])
},
cstr(St),
add_comments(U0,Text).

function_member(U0,Info,memberdef([[kind(_),id(MyId)|_]|Text])) -->
    {
member(definition([[],Def]), Text),
( member(argsstring([[],As]), Text) -> true ; As = ``),

rel_id(MyId,Info,Id),
      format(string(St),`###### ~s~s  {#~s}\n`,[Def,As,Id])
},
cstr(St),
    add_comments(U0,Text),
    !.
function_member(_U0,_Info,memberdef([[kind(_),id(_MyId)|_]|_Text])) -->
    [].
    
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

rel_id(MyId,Info,Id) :-
    arg(1,Info,PId),
    string_concat(PId,`_`,LId),
    string_concat(LId,Id,MyId),
    !.
rel_id(Id,_Info,Id).


