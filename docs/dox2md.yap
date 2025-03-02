/**
 *
 * @brief parse a XNL generated directory to
 * a MD directpry/
  */ 

/* #t4970 nE44T This program first */
:- set_prolog_flag(double_quotes, string).

:- include(docutils).

:- multifile  brief/2, visited/1.


:- 
    unix(argv([_,_,D])),
   path_concat([D,'packages/xml2yap/libYAPxml'],Lib),
   load_foreign_files([Lib],[],libxml_yap_init).

:-  initialization(main).

main :-
    unix(argv(Params)),
    main_process(Params).

main_process([IDir,ODir,_]) :-
    exists_directory(IDir),
    !,
    (
	sub_atom(IDir,_,1,0,'/')
    ->
    IDir= InputDir
    ;
    atom_concat(IDir,'/',InputDir)
    ),
    atom_concat(InputDir,'index.xml',Index),
    xml_load(Index,[doxygenindex(_,XMLTasks)]),
    (
	sub_atom(ODir,_,1,0,'/')
    ->
    ODir= OutputDir
    ;
    atom_concat(ODir,'/',OutputDir)
    ),
    forall((enum(XMLTasks,ID),group(ID)),(scan(ID,InputDir,OutputDir))),
    forall((enum(XMLTasks,ID),group(ID)),(trl( ID,InputDir,OutputDir))),
    forall((enum(XMLTasks,ID),class(ID)),(trl( ID,InputDir,OutputDir))).

enum([_|XMLTasks],ID) :-
    member(ID,XMLTasks).


group(compound( OAtts,_OProps)) :-
    key_in(kind(Kind),OAtts),
    (
	Kind == "group"
    ->
    true
    ;
    Kind == "page"
    
    ).

class(compound( OAtts,_OProps)) :-
    key_in(kind(Kind),OAtts),
    (
	Kind == "class"
    
    ).

scan(compound(OAtts,_OProps) , IDir,_ODir) :-
    key_in(refid(Id),OAtts),
    atom_concat([IDir,Id,'.xml'], IFile),
    catch(xml_load(IFile,XML),Error,(printf("failed while processsing ~w: ~w",[Id,Error]), fail)),
    XML = [doxygen(_,XMLData)],
    XMLData = [compounddef(_Atts,Children)],
    (
	key_in(briefdescription(BArgs,Paras),Children),
	briefs([briefdescription(BArgs,Paras)],Brief,[]),
	assert_static(brief(Id,Brief))
    ;
    true
    ).

trl(compound( OAtts,_OProps) ,_IDir,_ODir) :-
    key_in(refid(Id),OAtts),
    visited(Id),
    !.

trl(compound( OAtts,_OProps) ,IDir,ODir) :-
    key_in(refid(Id),OAtts),
    assert_static(visited(Id)),
    get_xml(IDir,Id, Atts,Children),
    key_in(kind(Kind),Atts),
    children2page([idir=IDir,odir=ODir,kind=Kind],Children,All),
    !,
    atom_concat([ODir,"/",Id,'.md'],OFile),
    open(OFile,write,O),
    format(O,'~s',[All]),
    close(O).
trl(compound( OAtts,_OProps),_,_):-
    key_in(refid(Id),OAtts),
    writeln(failed:Id).

get_xml(IDir,Id,Atts,Children) :-
    atom_concat([IDir,Id,'.xml'], IFile),
    catch(xml_load(IFile,XML),Error,(format(user_error,'failed while processsing ~w: ~w',[IFile,                      Error]),fail)),
    XML = [doxygen(_,XMLData)],
    XMLData = [compounddef(Atts,Children)].

children2page(State,Children,All) :-
    get_name(Children,Name),
as_title(Name,Children,Title),
	foldl(process_all(State),Children,[]-[]-[]-[]-[]-[]-[],AllRaw-Briefs-Details-Pages-Groups-Predicates-Locations),
    string_concat(AllRaw,Info),
    string_concat(Briefs,Bs),
    string_concat(Details,Ds),
    string_concat(Groups,Gs),
    string_concat(Predicates,Ps),
    string_concat(Pages,As),
    string_concat(Locations,Ls),
    string_concat(["# ",Title, "\n\n",Bs,"\n\n\n",As,Gs,"\n\n\n",Ds,"\n\n\n",Ps,"\n\n",Info,"\n\n",Ls],All).
 

process_all(State,Op,S0s,SFs) :-
    functor(Op,_N,_),
    process(State,Op,Strings,[]),
    string_concat(Strings,SC),
    add2strings(Op,SC,S0s,SFs),!.



add2strings(briefdescription(_,_),Strings,S-Sb-Sd-Sa-Sg-Sp-Sl,S-[Strings|Sb]-Sd-Sa-Sg-Sp-Sl)
 :-
    !.
add2strings(detaileddescription(_,_),Strings,S-Sb-Sd-Sa-Sg-Sp-Sl,S-Sb-[Strings|Sd]-Sa-Sg-Sp-Sl) :-
    !.
add2strings(innerclass(_,_),Strings,S-Sb-Sd-Sa-Sg-Sp-Sl,S-Sb-Sd-Sa-Sg-[Strings|Sp]-Sl) :-
    !.
add2strings(innergroup(_,_),Strings,S-Sb-Sd-Sa-Sg-Sp-Sl,S-Sb-Sd-Sa-[Strings|Sg]-Sp-Sl) :-
    !.
add2strings(innerpage(_,_),Strings,S-Sb-Sd-Sa-Sg-Sp-Sl,S-Sb-Sd-[Strings|Sa]-Sg-Sp-Sl) :-
    !.
add2strings(location(_,_),Strings,S-Sb-Sd-Sa-Sg-Sp-Sl,S-Sb-Sd-[Strings|Sa]-Sg-Sp-Sl) :-
    !.
add2strings(_Op,Strings,S-Sb-Sd-Sa-Sg-Sp-Sl,[Strings|S]-Sb-Sd-Sa-Sg-Sp-Sl).

process(_State,compoundname(_Atts,_Children)) -->
!.
process(_State,title(_Atts,_Children)) -->
!
.
process(State,basecompoundref(Atts,Children)) -->
    !,
    seq(State,basecompoundref(Atts,Children)).
process(State,derivedcompoundref(Atts,Children)) -->
    !,
    seq(State,derivedcompoundref(Atts,Children)).
% incType
% ignoreseq(NState,includes(_,_),Derivedmpoundref,Includes),
% ignoreseq(NState,includedby(_,_),Includes,Includedby),
% graphType
% ignoreseq(NState,incdepgraph(_,_),Includedby,Incdepgraph),
% ignoreseq(NState,invincdepgraph(_,_),Incdepgraph,Invincdepgraph),
% refType
% ignoreseq(NState,innermodule(_,_),Invincdepgraph,Innermodule),
% ignoreseq(NState,innerdir(_,_),Innermodule,Innerdir),
% ignoreseq(NState,innerfile(_,_),Innerdir,Innerfile),
% ignoreseq(NState,innerclass(_,_),Innerfile,Innerclass),
process(State,innerclass(Atts,Children)) -->
    !,
    innerclass([kind="class"|State],Atts,Children).    % ignoreseq(NState,innernamespace(_,_),Innerclass,Innernamespace),
process(State,innerpage(Atts,Children)) -->
    !,
    innerpage([kind="page"|State],Atts,Children).
process(State,innergroup(Atts,Children)) -->
    !,
    innergroup([kind="group"|State],Atts,Children).
% ignoreseq(NState,qualifier(_,_),Innergroup,Qualifier),
% ignoreseq(NState,templateparamlist(_,_),Qualifier,Templateparamlist),
process(_,sectiondef(Atts,Children)) -->
    !,
    sectiondef(Atts,Children).
% ignoreseq(NState,tableofcontents(_,_),Sectiondef,Tableofcontents),
% ignoreseq(NState,requiresclause(_,_),Tableofcontents,Requiresclause),
% ignoreseq(NState,initializer(_,_),Requiresclause,Initializer),
process(_,briefdescription(Atts,Children)) -->
    !,
    briefs(briefdescription(Atts,Children)).
process(_,detaileddescription(Atts,Children)) -->
    !,
    detaileds(detaileddescription(Atts,Children)),!.
process(_,location(_Atts,Children)) -->
    { key_in(file(_,[F]),Children) },
    !,
    { format(string(S), ' <small> generated by YAPDocs from ~s <small/>~n'  , [F]) },
[S].
process(_,location(_,_)) -->
[].
% ignoreseq(NState,exports(_,_),Detammmmtrnnmmmjjjjjileddescription,Exports),
% ignoreseq(NState,inheritancegraph(_,_),Exports,Inheritancegraph),

% ignoreseq(NState,collaborationgraph(_,_),Inheritancegraph,Collaborationgraph),
% ignoreseq(NState,programlisting(_,_),Collaborationgraph,Programlisting),
% ignoreseq(NState,location(_,_),Programlisting,Location),
% ignoreseq(NState,listofallmembers(_,_),Location,[]).
process(_,_)-->  [].

xtract_label([_,[Label]],Label) :-
    !.
xtract_label([Label],Label) :-
    !.
xtract_label(Label,Label).

innerclass(Status,Atts,AllLabel) -->
    ["\n* "], 
    {key_in(refid(Ref),Atts),
     xtract_label(AllLabel,Label),
     decode(Label,Pred)},
    link_inner(Status,Ref,Pred).

innerpage(Status,Atts,AllLabel) -->
    ["\n\n* "], 
    {key_in(refid(Ref),Atts),
     xtract_label(AllLabel,Label)},
    link_inner(Status,Ref,Label).

innergroup(Status,Atts,AllLabel) -->
    ["\n\n* "], 
    {key_in(refid(Ref),Atts),
     xtract_label(AllLabel,Label)},
    link_inner(Status,Ref,Label).

link_inner(Status,Ref,Label) -->
{ decode(Label,PLabel) },
    ref(Ref,PLabel),
    {
	key_in(idir=IDir,Status),
	key_in(odir=ODir,Status),
	key_in(kind=Kind,Status),
	trl(compound([refid(Ref),kind(Kind)],[]),IDir,ODir)
    }.


innermodule(Status,Atts,AllLabel) -->
    {key_in(refid(Ref),Atts),
     xtract_label(AllLabel,Label)},
    {
	key_in(modules=Found,Status),
	var(Found),
	!,
	Found=found
    },
    ["\n\n\n### Modules:\n"],
    ref(Ref,Label),
    ["\n."].
innermodule(_Status,Atts,AllLabel) -->
    {key_in(refid(Ref),Atts),
     xtract_label(AllLabel,Label)},
    ref(Ref,Label),
    ["\n."].





sectiondef(Atts,Els) -->
    {
	key_in(kind(Kind),Atts)
    },
    { top_sectiondef_name(Kind,Name)
    },
    !,
    ["## ",Name,":\n"],
    foldl(sectdef,Els).

v(Msg,S0,S0) :-
    writeln(Msg:S0).

sectdef(header([],[Text]))-->
{decode(Text,PText)},
    ["\n",PText,"\n"].
sectdef(member(Atts,Children))-->
    {
	key_in(refid(Ref),Atts),
	(
	    key_in(defname(_,[Name] ),Children)
	->
	true
	;
	key_in(name(_,[Name] ),Children)
	->
	true
	;
	Name = "" %writeln(Children)
	)
%         writeln(Children:Name)
    } ,
    ["%- " ],
{decode(Name,PName)},
    ref(Ref,PName).


sectdef(memberdef(Atts,Children))-->
    {

	( key_in(argsstring([],[Args]),Children) -> true ; Args = ""),
	key_in(id(Ref),Atts),
	get_name(Children,Name),
	decode(Name, PName),
	short_ref(Ref, PRef)
    },
    (
	{key_in(definition([],[Def0]),Children),
    	decode(Def0, Def)}


    ->

	{format(string(Header), '\n[](){#~s}\n1. **~s~s**: \n', [PRef,Def,Args])}
    ;
{format(string(Header), '\n[](){#~s}\n1. **~s~s**: \n', [PRef,PName,Args])}
    ),
    [Header],
    (	{ key_in(briefdescription([],Brief),Children) }

    ->
    [": "],
    description(Brief)
    ;
    []
    ),
    (
	{ key_in(inbodydescription([],InBody),Children) }
    ->
    [ "\n  "],
    description(InBody)
    ;
    []
    ),
    (
	{ key_in(detaileddescription([],Detailed),Children)}
    ->
    [ "\n\n"],
    description(Detailed)
    ;
    []
    ),
    !,
    [ "\n"].

%sectiondef(A,Remainder) --> {writeln(A),fail}.
briefs(briefdescription(_Atts,Els)) -->
    !,
    description(Els).

detaileds(detaileddescription(_Atts,Els)) -->
    !,
    [ "\n"],
    separatedescription(Els),
    [ "\n"].

separatedescription([]) --> [].
separatedescription([D|Detailed]) -->
    %    {writeln(D)},
    description(D),
    [ "\n\n"],
    separatedescription( Detailed).



mkraw(codeline(_,Line)) -->
    [ "\n"],
    rawl(Line).

rawl([]) -->
    !.
rawl([A|Text]) -->
    foldl(raw,[A|Text]).

raw(highlight(_,Text)) -->
    !,
    rawl( Text).

raw(Text) -->
{string(Text) },
[" "],
[Text].

%  foldl(para).

parlist(_Pars,Items) -->
["\n"],
    foldl(paritem,Items).

paritem(parameteritem(_,List)) -->
foldl(parameternamelist,List).

parameternamelist(parameternamelist(_,Args))  -->
  foldl(parameterargs, Args).
parameternamelist(parameterdescription(_,Args))  -->
  foldl(parameterargs, Args).

parameterargs(parametername(_,[Name])) -->
 ["  ",Name].
parameterargs(parameterdescription(_,Para)) -->
  foldl(description,Para),
["\n"].
parameterargs(para(_,Para)) -->
  foldl(description,Para),
["\n"].


doxolist(_Pars,Items) -->
["\n"],
    foldl(item("1"),Items).

itemlist(_Pars,Items) -->
["\n\n"],
    foldl(item("i"),Items).

varlist(_Pars,Items) -->

["\n"],
    foldl(varentry,Items).

varentry(varlistentry(_,Terms)) -->
    [ "\n"],
maplist(term,Terms).

term(term(_,[S|_])) -->
[S].

item(Type,listitem(_,Para)) -->
    typel(Type),
    description(Para),
    ["\n"].

typel("1") -->
    [  "1. "].
typel("a") -->
    [  "a. "].
typel("A") -->
    [  "A. "].
typel("i") -->
    [  "- "].
typel("I") -->
    [  "* "].


sect( Parms, Args, Level) -->
    (
	{  key_in(id(Id),Parms)  }
    ->
    [Id],["\n"]
    ;
[]
    ),

    [Level],
    (
	{ key_in(kind(Kind),Parms) }
    ->
    [Kind,":  "]
;
[]
    ),
    (
	{ Args = [title([],[T])|Body] }
    ->
{encode(T,TT)},
    [TT],["\n"]
    ;
{Body = Args},
    ["\n"]
    ),
    description(Body).

description(para([],S)) -->
    !,
    description(S),
 ["\n\n"].
description(S) -->
    { string(S) },
    !,
    [S].
description(title([],S)) -->
    { string(S),
      encode(S,T) },
    !,
    [T].
description(sect1(Parms,S)) -->
!,
    sect(Parms,S,"### ").
description(sect2(Parms,S)) -->
!,
    sect(Parms,S,"#### ").
description(sect3(Parms,S)) -->
!,
    sect(Parms,S,"##### ").
description(simplesect(Parms,S)) -->
!,
    sect(Parms,S," ").

description([]) -->
    !.
description([G|S]) -->
    !,
    description(G),
    description(S).
description(S) -->
    para(S).

seq(State,G0) -->
    %    v(G0),
    {
	G0=..[_N,Atts,[NameS|Els]],
	get_name(NameS, Name),
	key_in(id(Ref),Atts),
	!,
	seqhdr(State,Type)
    } ,
    ["## ",Type,": "],
    ref(Ref,Name),
    foldl(seqdef,Els),
    ["\n"].
seq(State,G0) -->
    {
	arg(2,G0,NameS),
	!,
	get_name(NameS, Name),
	seqhdr(State,Type)
    } ,
    ["## ",Type,": ",Name,"\n"].

seqhdr(State,Name) :-
    key_in(kind=Kind, State),
    top_seq_name( Kind, Name).

top_seq_name( "class", "Class" ).
top_seq_name( "struct", "Struct" ).
top_seq_name( "union", "Union" ).
top_seq_name( "interface", "Interface" ).
top_seq_name( "protocol", "Protocol" ).
top_seq_name( "category", "Category" ).
top_seq_name( "exception", "Exception" ).
top_seq_name( "service", "Service" ).
top_seq_name( "singleton", "Singleton" ).
top_seq_name( "module", "Module" ).
top_seq_name( "type", "Type" ).
top_seq_name( "file", "File" ).
top_seq_name( "namespace", "Namespace" ).
top_seq_name( "group", "Group" ).
top_seq_name( "page", "Page" ).
top_seq_name( "example", "Example" ).
top_seq_name( "dir", "Dir" ).
top_seq_name( "concept", "Concept" ).

top_sectiondef_name( "friend", "Friends").
top_sectiondef_name( "protected-attrib", "Protected Attributes").
top_sectiondef_name( "public-func", "Public Function").
top_sectiondef_name( "public-type", "Public Type").
top_sectiondef_name( "public-type", "Public Type").

top_sectiondef_name( "user-defined", "User-defined").
top_sectiondef_name( "public-type", "Public-type" ).
top_sectiondef_name( "public-func", "Public-func" ).
top_sectiondef_name( "public-attrib", "Public-attrib" ).
top_sectiondef_name( "public-slot", "Public-slot" ).
top_sectiondef_name( "signal", "Signal" ).
top_sectiondef_name( "dcop-func", "Dcop-func" ).
top_sectiondef_name( "property", "Property" ).
top_sectiondef_name( "event", "Event" ).
top_sectiondef_name( "public-static-func", "Public-static-func" ).
top_sectiondef_name( "public-static-attrib", "Public-static-attrib" ).
top_sectiondef_name( "protected-type", "Protected-type" ).
top_sectiondef_name( "protected-func", "Protected-func" ).
top_sectiondef_name( "protected-attrib", "Protected-attrib" ).
top_sectiondef_name( "protected-slot", "Protected-slot" ).
top_sectiondef_name( "protected-static-func", "Protected-static-func" ).
top_sectiondef_name( "protected-static-attrib", "Protected-static-attrib" ).
top_sectiondef_name( "package-type", "Package-type" ).
top_sectiondef_name( "package-func", "Package-func" ).
top_sectiondef_name( "package-attrib", "Package-attrib" ).
top_sectiondef_name( "package-static-func", "Package-static-func" ).
top_sectiondef_name( "package-static-attrib", "Package-static-attrib" ).
top_sectiondef_name( "private-type", "Private-type" ).
top_sectiondef_name( "private-func", "Private-func" ).
top_sectiondef_name( "private-attrib", "Private-attrib" ).
top_sectiondef_name( "private-slot", "Private-slot" ).
top_sectiondef_name( "private-static-func", "Private-static-func" ).
top_sectiondef_name( "private-static-attrib", "Private-static-attrib" ).
top_sectiondef_name( "friend", "Friend" ).
top_sectiondef_name( "related", "Related" ).
top_sectiondef_name( "define", "Define" ).
top_sectiondef_name( "prototype", "Prototype" ).
top_sectiondef_name( "typedef", "Typedef" ).
top_sectiondef_name( "enum", "Enum" ).
top_sectiondef_name( "func", "Functions" ).
top_sectiondef_name(   "var", "Var" ).

as_title(_,Props,PredTitle) :-
key_in(title(_,[Title]), Props),
!,
decode(Title, PredTitle).
as_title(Name,_,Name).

bd(blockquote,"\n~~~\n").
bd(bold,"**").
bd(cstrike, "~~").
bd(computeroutput, "\`").
bd(emphasis, "__").
bd(quot, "\`").
bd(verbatim, "\`").
bd(s, "~~").
bd(sp, "~~").
bd(underline, "<ins>").

para(P) -->
    {
	P=..[N,_,A],
	bd(N,H)
    },
    !,
    [H],
    (
	{string(A)}
    ->
    [A]
    ;
    description(A)
    ),
    [H].


para(ulink([url(Title)],[URL|_])) -->
{ decode(Title, DTitle) },
    !,
{   format(string(S),"[~s](~s)", [DTitle,URL])},
[S].
para(hruler([],_)) -->
    [ "\n- - -\n"].
para(preformatted([],Text)) -->
    unimpl(preformatted,Text). % docMarkupType
para(programlisting(_,Text)) -->

    [ "```\n"],
    foldl(mkraw,Text),
    ["\n```\n"].


para(javadocliteral([],Text)) -->
    unimpl(javadocliteral,Text). % xsd:unimpling
para(javadoccode([],Text)) -->
    unimpl(javadoccode,Text). % xsd:unimpling
para(indexentry([],Text)) -->
    unimpl(indexentry,Text). % docIndexEntryType
para(orderedlist(Atts,Text)) -->
    doxolist(Atts,Text). % docListType
para(parameterlist(Atts,Text)) -->
    parlist(Atts,Text). % docParamListType
para(itemizedlist(Atts,Text)) -->
    itemlist(Atts,Text). % docListType
para(variablelist(Atts,Text)) -->
    varlist(Atts,Text). % docVariableListType
para(simplesect([kind(Kind)|Text])) -->
!,
["\n\n"],
[Kind],
[" "],
description(Text).
para(table(_,Text)) -->
    unimpl(table,Text). % docTableType
para(heading(_,Text)) -->
    unimpl(heading,Text). % docHeadingType
para(dotfile(_,Text)) -->
    unimpl(dotfile,Text). % docImageFileType
para(mscfile(_,Text)) -->
    unimpl(mscfile,Text). % docImageFileType
para(diafile(_,Text)) -->
    unimpl(diafile,Text). % docImageFileType
para(toclist(_,Text)) -->
    unimpl(toclist,Text). % docTocListType
para(language(_,Text)) -->
    unimpl(language,Text). % docLanguageType
para(xrefsect(_,Text)) -->
    unimpl(xrefsect,Text). % docXRefSectType
para(copydoc(_,Text)) -->
    unimpl(copydoc,Text). % docCopyType
para(details(_,Text)) -->
    unimpl(details,Text). % docDetailsType
para(parblock(_,Text))-->
    paras(Text). % docParBlockType         
para(superscript(_,Text)) -->
    {string(Text)},
    !,
    ["<sup>"], [Text], ["<sup>"].
para(superscript([],Text)) -->
    ["<sup>"], para(Text), [ "</sup>"].
para(center([],Text)) --> % unsupported
    para(Text). % docMarkupType
para(small([],Text)) -->
    [ "<small>"], para(Text), [ "</small>"].
para(cite([],Text)) -->
    para(Text). % docMarkupType
para(del([],Text)) -->
    [ "<del>"], para(Text), [ "</del>"].
para(ins([],Text)) -->
    [ "<ins>"], para(Text), [ "</ins>"].
para(nonbreakablespace([],_)) -->
    [      "<nonbreakablespace/>"].
para('iexcl'(_,_))  -->
    [     "<iexcl/>"].
para('cent'(_,_))  -->
    [      "<cent/>"].
para('pound'(_,_))  -->
    [     "<pound/>"].
para('curren'(_,_))  -->
    [    "<curren/>"].
para('yen'(_,_))  -->
    [       "<yen/>"].
para('brvbar'(_,_))  -->
    [    "<brvbar/>"].
para('sect'(_,_))  -->
    [      "<sect/>"].
para('uml'(_,_))  -->
    [       "<umlaut/>"].
para('copy'(_,_))  -->
    [      "<copy/>"].
para('ordf'(_,_))  -->
    [      "<ordf/>"].
para('laquo'(_,_))  -->
    [     "<laquo/>"].
para('sup3'(_,_))  -->
    [      "<sup3/>"].
para('acute'(_,_))  -->
    [     "<acute/>"].
para('micro'(_,_))  -->
    [     "<micro/>"].
para('para'(_,_))  -->
    [      "<para/>"].
para('middot'(_,_))  -->
    [    "<middot/>"].
para('cedil'(_,_))  -->
    [     "<cedil/>"].
para('sup1'(_,_))  -->
    [      "<sup1/>"].
para('ordm'(_,_))  -->
    [      "<ordm/>"].
para('raquo'(_,_))  -->
    [     "<raqUo/>"].
para('frac14'(_,_))  -->
    [    "<frac14/>"].
para('frac12'(_,_))  -->
    [    "<frac12/>"].
para('frac34'(_,_))  -->
    [    "<frac34/>"].
para('iquest'(_,_))  -->
    [    "<iquest/>"].
para('Agrave'(_,_))  -->
    [    "<Agrave/>"].
para('Aacute'(_,_))  -->
    [    "<Aacute/>"].
para('Acirc'(_,_))  -->
    [     "<Acirc/>"].
para('Atilde'(_,_))  -->
    [    "<Atilde/>"].
para('Auml'(_,_))  -->
    [      "<Aumlaut/>"].
para('Aring'(_,_))  -->
    [     "<Aring/>"].
para('AElig'(_,_))  -->
    [     "<AElig/>"].
para('Ccedil'(_,_))  -->
    [    "<Ccedil/>"].
para('Egrave'(_,_))  -->
    [    "<Egrave/>"].
para('Eacute'(_,_))  -->
    [    "<Eacute/>"].
para('Ecirc'(_,_))  -->
    [     "<Ecirc/>"].
para('Euml'(_,_))  -->
    [      "<Eumlaut/>"].
para('Igrave'(_,_))  -->
    [    "<Igrave/>"].
para('Iacute'(_,_))  -->
    [    "<Iacute/>"].
para('Icirc'(_,_))  -->
    [     "<Icirc/>"].
para('Iuml'(_,_))  -->
    [      "<Iumlaut/>"].
para('ETH'(_,_))  -->
    [       "<ETH/>"].
para('Ntilde'(_,_))  -->
    [    "<Ntilde/>"].
para('Ograve'(_,_))  -->
    [    "<Ograve/>"].
para('Oacute'(_,_))  -->
    [    "<Oacute/>"].
para('Ocirc'(_,_))  -->
    [     "<Ocirc/>"].
para('Otilde'(_,_))  -->
    [    "<Otilde/>"].
para('Ouml'(_,_))  -->
    [      "<Oumlaut/>"].
para('times'(_,_))  -->
    [     "<times/>"].
para('Oslash'(_,_))  -->
    [    "<Oslash/>"].
para('Ugrave'(_,_))  -->
    [    "<Ugrave/>"].
para('Uacute'(_,_))  -->
    [    "<Uacute/>"].
para('Ucirc'(_,_))  -->
    [     "<Ucirc/>"].
para('Uuml'(_,_))  -->
    [      "<Uumlaut/>"].
para('Yacute'(_,_))  -->
    [    "<Yacute/>"].
para('THORN'(_,_))  -->
    [     "<THORN/>"].
para('szlig'(_,_))  -->
    [     "<szlig/>"].
para('agrave'(_,_))  -->
    [    "<agrave/>"].
para('aacute'(_,_))  -->
    [    "<aacute/>"].
para('acirc'(_,_))  -->
    [     "<acirc/>"].
para('atilde'(_,_))  -->
    [    "<atilde/>"].
para('auml'(_,_))  -->
    [      "<aumlaut/>"].
para('aring'(_,_))  -->
    [     "<aring/>"].
para('aelig'(_,_))  -->
    [     "<aelig/>"].
para('ccedil'(_,_))  -->
    [    "<ccedil/>"].
para('egrave'(_,_))  -->
    [    "<egrave/>"].
para('eacute'(_,_))  -->
    [    "<eacute/>"].
para('ecirc'(_,_))  -->
    [     "<ecirc/>"].
para('euml'(_,_))  -->
    [      "<eumlaut/>"].
para('igrave'(_,_))  -->
    [    "<igrave/>"].
para('iacute'(_,_))  -->
    [    "<iacute/>"].
para('icirc'(_,_))  -->
    [     "<icirc/>"].
para('iuml'(_,_))  -->
    [      "<iumlaut/>"].
para('eth'(_,_))  -->
    [       "<eth/>"].
para('ntilde'(_,_))  -->
    [    "<ntilde/>"].
para('ograve'(_,_))  -->
    [    "<ograve/>"].
para('oacute'(_,_))  -->
    [    "<oacute/>"].
para('ocirc'(_,_))  -->
    [     "<ocirc/>"].
para('otilde'(_,_))  -->
    [    "<otilde/>"].
para('ouml'(_,_))  -->
    [      "<oumlaut/>"].
para('divide'(_,_))  -->
    [    "<divide/>"].
para('oslash'(_,_))  -->
    [    "<oslash/>"].
para('ugrave'(_,_))  -->
    [    "<ugrave/>"].
para('uacute'(_,_))  -->
    [    "<uacute/>"].
para('ucirc'(_,_))  -->
    [     "<ucirc/>"].
para('uuml'(_,_))  -->
    [      "<uumlaut/>"].
para('yacute'(_,_))  -->
    [    "<yacute/>"].
para('thorn'(_,_))  -->
    [     "<thorn/>"].
para('yuml'(_,_))  -->
    [      "<yumlaut/>"].
para('fnof'(_,_))  -->
    [      "<fnof/>"].
para('Alpha'(_,_))  -->
    [     "<Alpha/>"].
para('Beta'(_,_))  -->
    [      "<Beta/>"].
para('Gamma'(_,_))  -->
    [     "<Gamma/>"].
para('Delta'(_,_))  -->
    [     "<Delta/>"].
para('Epsilon'(_,_))  -->
    [   "<Epsilon/>"].
para('Zeta'(_,_))  -->
    [      "<Zeta/>"].
para('Eta'(_,_))  -->
    [       "<Eta/>"].
para('Theta'(_,_))  -->
    [     "<Theta/>"].
para('Iota'(_,_))  -->
    [      "<Iota/>"].
para('Kappa'(_,_))  -->
    [     "<Kappa/>"].
para('Lambda'(_,_))  -->
    [    "<Lambda/>"].
para('Mu'(_,_))  -->
    [        "<Mu/>"].
para('Nu'(_,_))  -->
    [        "<Nu/>"].
para('Xi'(_,_))  -->
    [        "<Xi/>"].
para('Omicron'(_,_))  -->
    [   "<Omicron/>"].
para('Pi'(_,_))  -->
    [        "<Pi/>"].
para('Rho'(_,_))  -->
    [       "<Rho/>"].
para('Sigma'(_,_))  -->
    [     "<Sigma/>"].
para('Tau'(_,_))  -->
    [       "<Tau/>"].
para('Upsilon'(_,_))  -->
    [   "<Upsilon/>"].
para('Phi'(_,_))  -->
    [       "<Phi/>"].
para('Chi'(_,_))  -->
    [       "<Chi/>"].
para('Psi'(_,_))  -->
    [       "<Psi/>"].
para('Omega'(_,_))  -->
    [     "<Omega/>"].
para('alpha'(_,_))  -->
    [     "<alpha/>"].
para('beta'(_,_))  -->
    [      "<beta/>"].
para('gamma'(_,_))  -->
    [     "<gamma/>"].
para('delta'(_,_))  -->
    [     "<delta/>"].
para('epsilon'(_,_))  -->
    [   "<epsilon/>"].
para('zeta'(_,_))  -->
    [      "<zeta/>"].
para('eta'(_,_))  -->
    [       "<eta/>"].
para('theta'(_,_))  -->
    [     "<theta/>"].
para('iota'(_,_))  -->
    [      "<iota/>"].
para('kappa'(_,_))  -->
    [     "<kappa/>"].
para('lambda'(_,_))  -->
    [    "<lambda/>"].
para('mu'(_,_))  -->
    [        "<mu/>"].
para('nu'(_,_))  -->
    [        "<nu/>"].
para('xi'(_,_))  -->
    [        "<xi/>"].
para('omicron'(_,_))  -->
    [   "<omicron/>"].
para('pi'(_,_))  -->
    [        "<pi/>"].
para('rho'(_,_))  -->
    [       "<rho/>"].
para('sigmaf'(_,_))  -->
    [    "<sigmaf/>"].
para('sigma'(_,_))  -->
    [     "<sigma/>"].
para('tau'(_,_))  -->
    [       "<tau/>"].
para('upsilon'(_,_))  -->
    [   "<upsilon/>"].
para('phi'(_,_))  -->
    [       "<phi/>"].
para('chi'(_,_))  -->
    [       "<chi/>"].
para('psi'(_,_))  -->
    [       "<psi/>"].
para('omega'(_,_))  -->
    [     "<omega/>"].
para('thetasym'(_,_))  -->
    [  "<thetasym/>"].
para('upsih'(_,_))  -->
    [     "<upsih/>"].
para('piv'(_,_))  -->
    [       "<piv/>"].
para('bull'(_,_))  -->
    [      "<bull/>"].
para('hellip'(_,_))  -->
    [    "<hellip/>"].
para('prime'(_,_))  -->
    [     "<prime/>"].
para('Prime'(_,_))  -->
    [     "<Prime/>"].
para('oline'(_,_))  -->
    [     "<oline/>"].
para('frasl'(_,_))  -->
    [     "<frasl/>"].
para('weierp'(_,_))  -->
    [    "<weierp/>"].
para('image'(_,_))  -->
    [     "<imaginary/>"].
para('real'(_,_))  -->
    [      "<real/>"].
para('trade'(_,_))  -->
    [     "<trademark/>"].
para('alefsym'(_,_))  -->
    [   "<alefsym/>"].
para('larr'(_,_))  -->
    [      "<larr/>"].
para('uarr'(_,_))  -->
    [      "<uarr/>"].
para('rarr'(_,_))  -->
    [      "<rarr/>"].
para('darr'(_,_))  -->
    [      "<darr/>"].
para('harr'(_,_))  -->
    [      "<harr/>"].
para('crarr'(_,_))  -->
    [     "<crarr/>"].
para('lArr'(_,_))  -->
    [      "<lArr/>"].
para('uArr'(_,_))  -->
    [      "<uArr/>"].
para('rArr'(_,_))  -->
    [      "<rArr/>"].
para('dArr'(_,_))  -->
    [      "<dArr/>"].
para('hArr'(_,_))  -->
    [      "<hArr/>"].
para('forall'(_,_))  -->
    [    "<forall/>"].
para('part'(_,_))  -->
    [      "<part/>"].
para('exist'(_,_))  -->
    [     "<exist/>"].
para('empty'(_,_))  -->
    [     "<empty/>"].
para('nabla'(_,_))  -->
    [     "<nabla/>"].
para('isin'(_,_))  -->
    [      "<isin/>"].
para('notin'(_,_))  -->
    [     "<notin/>"].
para('ni'(_,_))  -->
    [        "<ni/>"].
para('prod'(_,_))  -->
    [      "<prod/>"].
para('sum'(_,_))  -->
    [       "<sum/>"].
para('minus'(_,_))  -->
    [     "<minus/>"].
para('lowast'(_,_))  -->
    [    "<lowast/>"].
para('radic'(_,_))  -->
    [     "<radic/>"].
para('prop'(_,_))  -->
    [      "<prop/>"].
para('infin'(_,_))  -->
    [     "<infin/>"].
para('ang'(_,_))  -->
    [       "<ang/>"].
para('and'(_,_))  -->
    [       "<and/>"].
para('or'(_,_))  -->
    [        "<or/>"].
para('cap'(_,_))  -->
    [       "<cap/>"].
para('cup'(_,_))  -->
    [       "<cup/>"].
para('int'(_,_))  -->
    [       "<int/>"].
para('there4'(_,_))  -->
    [    "<there4/>"].
para('sim'(_,_))  -->
    [       "<sim/>"].
para('cong'(_,_))  -->
    [      "<cong/>"].
para('asymp'(_,_))  -->
    [     "<asymp/>"].
para('ne'(_,_))  -->
    [        "<ne/>"].
para('equiv'(_,_))  -->
    [     "<equiv/>"].
para('le'(_,_))  -->
    [        "<le/>"].
para('ge'(_,_))  -->
    [        "<ge/>"].
para('sub'(_,_))  -->
    [       "<sub/>"].
para('sup'(_,_))  -->
    [       "<sup/>"].
para('nsub'(_,_))  -->
    [      "<nsub/>"].
para('sube'(_,_))  --> 
    [      "<sube/>"].
para('supe'(_,_))  -->
    [      "<supe/>"].
para('oplus'(_,_))  -->
    [     "<oplus/>"].
para('otimes'(_,_))  -->
    [    "<otimes/>"].
para('perp'(_,_))  -->
    [      "<perp/>"].
para('sdot'(_,_))  -->
    [      "<sdot/>"].
para('lceil'(_,_))  -->
    [     "<lceil/>"].
para('rceil'(_,_))  -->
    [     "<rceil/>"].
para('lfloor'(_,_))  -->
    [    "<lfloor/>"].
para('rfloor'(_,_))  -->
    [    "<rfloor/>"].
para('lang'(_,_))  -->
    [      "<lang/>"].
para('rang'(_,_))  -->
    [      "<rang/>"].
para('loz'(_,_))  -->
    [       "<loz/>"].
para('spades'(_,_))  -->
    [    "<spades/>"].
para('clubs'(_,_))  -->
    [     "<clubs/>"].
para('hearts'(_,_))  -->
    [    "<hearts/>"].
para('diams'(_,_))  -->
    [     "<diams/>"].
para('quot'(_,_))  -->
    [ "&quot;"].
para('amp'(_,_))  -->
    [       "&amp;"].
para('lt'(_,_))  -->
    [        "&lt;"].
para('gt'(_,_))  -->
    [        "&gt;"].
para('OElig'(_,_))  -->
    [     "<OElig/>"].
para('oelig'(_,_))  -->
    [     "<oelig/>"].
para('Scaron'(_,_))  -->
    [    "<Scaron/>"].
para('scaron'(_,_))  -->
    [    "<scaron/>"].
para('Yuml'(_,_))  -->
    [      "<Yumlaut/>"].
para('circ'(_,_))  -->
    [      "<circ/>"].
para('tilde'(_,_))  -->
    [     "<tilde/>"].
para(ensp(_,_)) -->
    [ "<ensp/>"].
para('emsp'(_,_))  -->
    [ "<emsp/>"].
para('thinsp'(_,_))  -->
    [   "<thinsp/>"].
para('zwnj'(_,_))  -->
    [  "<zwnj/>"].
para('zwj'(_,_)) -->
    [ "<zwj/>"].
para('lrm'(_,_)) -->
    [ "<lrm/>"].
para('rlm'(_,_)) -->
    [ "<rlm/>"].
para('ndash'(_,_)) -->
    [ "<ndash/>"].
para('mdash'(_,_)) -->
    [ "<mdash/>"].
para('lsquo'(_,_)) -->
    [ "<lsquo/>"].
para('rsquo'(_,_)) -->
    [ "<rsquo/>"].
para('sbquo'(_,_)) -->
    [ "<sbquo/>"].
para('ldquo'(_,_)) -->
    [ "<ldquo/>"].
para('rdquo'(_,_)) -->
    [ "<rdquo/>"].
para('bdquo'(_,_)) -->
    [ "<bdquo/>"].
para('dagger'(_,_)) -->
    [ "<dagger/>"].
para('Dagger'(_,_)) -->
    [ "<Dagger/>"].
para('permil'(_,_)) -->
    [ "<permil/>"].
para('lsaquo'(_,_)) -->
    [ "<lsaquo/>"].
para('rsaquo'(_,_)) -->
    [ "<rsaquo/>"].
para('euro'(_,_)) -->
    [ "<euro/>"].
%  // doxygen extension to the HTML4 table of HTML entities
para('tm'(_,_))  -->
    [    "<tm/>"].
para('apos'(_,_))  -->
    [ "&apos;"].

%  // doxygen commands represented as HTML entities
para('BSlash'(_,_)) -->
    [ "\\"].
para('BSlash'(_,_)) -->
    [ "@"].
para('Less'(_,_)) -->
    [ "&lt;"].
para('Greater'(_,_)) -->
    [ "&lt;"].
%<!-- end workaround for xsd.exe -->
para(center([],Text)) --> % unsupported
    para(Text). % docMarkupType
para(small([],Text)) -->
    [ "<small>"], para(Text), [ "</small>"].
para(cite([],Text)) -->
    para(Text). % docMarkupType
para(del([],Text)) -->
    [ "<del>"], para(Text), [ "</del>"].
para(ins([],Text)) -->
    [ "<ins>"], para(Text), [ "</ins>"].
para(htmlonly([],_Text)) -->
    []. % docHtmlOnlyType
para(manonly([],_Text)) -->
    [].
para(xmlonly([],Text)) -->
    para(Text).
para(rtfonly([],_Text)) -->
    []. % xsd:cstring
para(latexonly([],_Text)) -->
    []. % xsd:cstring
para(docbookonly([],_Text)) -->
    []. % xsd:cstring
para(image([],_Text)) -->
    para(_Text). % docImageType
para(dot([],_Text)) -->
    []. % docDotMscType
para(msc([],_Text)) -->
    []. % docDotMscType
para(plantuml([],_Text)) -->
    []. % docPlantumlType
para(anchor(Parms,Children))-->
anchor(Parms,Children).
para(ref(Atts,[Name])) -->
    {
        key_in(refid(Ref),Atts)
    },
    !,
    ref(Ref,Name).
para(linebreak([],_)) -->
    ["<br>"]. % docEmptyType
para('not'(_,_))  -->
    [       "<not/>"].
para('shy'(_,_))  -->
    [       "<shy/>"].
para('reg'(_,_))  -->
    [       "<registered/>"].
para('macr'(_,_))  -->
    [      "<macr/>"].
para('deg'(_,_))  -->
    [       "<deg/>"].
para('plusmn'(_,_))  -->
    [    "<plusmn/>"].
para('sup2'(_,_))  -->
    [     "<sup2/>"].
para('raquo'(_,_))  -->
    [     "<raquo/>"].
para('frac14'(_,_))  -->
    [    "<frac14/>"].
para('frac12'(_,_))  -->
    [    "<frac12/>"].
para('frac34'(_,_))  -->
    [    "<frac34/>"].
para('iquest'(_,_))  -->
    [    "<iquest/>"].
para('Agrave'(_,_))  -->
    [    "<Agrave/>"].
para('Aacute'(_,_))  -->
    [    "<Aacute/>"].
para('Acirc'(_,_))  -->
    [     "<Acirc/>"].
para('Atilde'(_,_))  -->
    [    "<Atilde/>"].
para('Auml'(_,_))  -->
    [      "<Aumlaut/>"].
para('Aring'(_,_))  -->
    [     "<Aring/>"].
para('AElig'(_,_))  -->
    [     "<AElig/>"].
para('Ccedil'(_,_))  -->
    [    "<Ccedil/>"].
para('Egrave'(_,_))  -->
    [    "<Egrave/>"].
para('Eacute'(_,_))  -->
    [    "<Eacute/>"].
para('Ecirc'(_,_))  -->
    [     "<Ecirc/>"].
para('Euml'(_,_))  -->
    [      "<Eumlaut/>"].
para('Igrave'(_,_))  -->
    [    "<Igrave/>"].
para('Iacute'(_,_))  -->
    [    "<Iacute/>"].
para('Icirc'(_,_))  -->
    [     "<Icirc/>"].
para('Iuml'(_,_))  -->
    [      "<Iumlaut/>"].
para('ETH'(_,_))  -->
    [       "<ETH/>"].
para('Ntilde'(_,_))  -->
    [    "<Ntilde/>"].
para('Ograve'(_,_))  -->
    [    "<Ograve/>"].
para('Oacute'(_,_))  -->
    [    "<Oacute/>"].
para('Ocirc'(_,_))  -->
    [     "<Ocirc/>"].
para('Otilde'(_,_))  -->
    [    "<Otilde/>"].
para('Ouml'(_,_))  -->
    [      "<Oumlaut/>"].
para('times'(_,_))  -->
    [     "<times/>"].
para('Oslash'(_,_))  -->
    [    "<Oslash/>"].
para('Ugrave'(_,_))  -->
    [    "<Ugrave/>"].
para('Uacute'(_,_))  -->
    [    "<Uacute/>"].
para('Ucirc'(_,_))  -->
    [     "<Ucirc/>"].
para('Uuml'(_,_))  -->
    [      "<Uumlaut/>"].
para('Yacute'(_,_))  -->
    [    "<Yacute/>"].
para('THORN'(_,_))  -->
    [     "<THORN/>"].
para('szlig'(_,_))  -->
    [     "<szlig/>"].
para('agrave'(_,_))  -->
    [    "<agrave/>"].
para('aacute'(_,_))  -->
    [    "<aacute/>"].
para('acirc'(_,_))  -->
    [     "<acirc/>"].
para('atilde'(_,_))  -->
    [    "<atilde/>"].
para('auml'(_,_))  -->
    [      "<aumlaut/>"].
para('aring'(_,_))  -->
    [     "<aring/>"].
para('aelig'(_,_))  -->
    [     "<aelig/>"].
para('ccedil'(_,_))  -->
    [    "<ccedil/>"].
para('egrave'(_,_))  -->
    [    "<egrave/>"].
para('eacute'(_,_))  -->
    [    "<eacute/>"].
para('ecirc'(_,_))  -->
    [     "<ecirc/>"].
para('euml'(_,_))  -->
    [      "<eumlaut/>"].
para('igrave'(_,_))  -->
    [    "<igrave/>"].
para('iacute'(_,_))  -->
    [    "<iacute/>"].
para('icirc'(_,_))  -->
    [     "<icirc/>"].
para('iuml'(_,_))  -->
    [      "<iumlaut/>"].
para('eth'(_,_))  -->
    [       "<eth/>"].
para('ntilde'(_,_))  -->
    [    "<ntilde/>"].
para('ograve'(_,_))  -->
    [    "<ograve/>"].
para('oacute'(_,_))  -->
    [    "<oacute/>"].
para('ocirc'(_,_))  -->
    [     "<ocirc/>"].
para('otilde'(_,_))  -->
    [    "<otilde/>"].
para('ouml'(_,_))  -->
    [      "<oumlaut/>"].
para('divide'(_,_))  -->
    [    "<divide/>"].
para('oslash'(_,_))  -->
    [    "<oslash/>"].
para('ugrave'(_,_))  -->
    [    "<ugrave/>"].
para('uacute'(_,_))  -->
    [    "<uacute/>"].
para('ucirc'(_,_))  -->
    [     "<ucirc/>"].
para('uuml'(_,_))  -->
    [      "<uumlaut/>"].
para('yacute'(_,_))  -->
    [    "<yacute/>"].
para('thorn'(_,_))  -->
    [     "<thorn/>"].
para('yuml'(_,_))  -->
    [      "<yumlaut/>"].
para('fnof'(_,_))  -->
    [      "<fnof/>"].
para('Alpha'(_,_))  -->
    [     "<Alpha/>"].
para('Beta'(_,_))  -->
    [      "<Beta/>"].
para('Gamma'(_,_))  -->
    [     "<Gamma/>"].
para('Delta'(_,_))  -->
    [     "<Delta/>"].
para('Epsilon'(_,_))  -->
    [   "<Epsilon/>"].
para('Zeta'(_,_))  -->
    [      "<Zeta/>"].
para('Eta'(_,_))  -->
    [       "<Eta/>"].
para('Theta'(_,_))  -->
    [     "<Theta/>"].
para('Iota'(_,_))  -->
    [      "<Iota/>"].
para('Kappa'(_,_))  -->
    [     "<Kappa/>"].
para('Lambda'(_,_))  -->
    [    "<Lambda/>"].
para('Mu'(_,_))  -->
    [        "<Mu/>"].
para('Nu'(_,_))  -->
    [        "<Nu/>"].
para('Xi'(_,_))  -->
    [        "<Xi/>"].
para('Omicron'(_,_))  -->
    [   "<Omicron/>"].
para('Pi'(_,_))  -->
    [        "<Pi/>"].
para('Rho'(_,_))  -->
    [       "<Rho/>"].
para('Sigma'(_,_))  -->
    [     "<Sigma/>"].
para('Tau'(_,_))  -->
    [       "<Tau/>"].
para('Upsilon'(_,_))  -->
    [   "<Upsilon/>"].
para('Phi'(_,_))  -->
    [       "<Phi/>"].
para('Chi'(_,_))  -->
    [       "<Chi/>"].
para('Psi'(_,_))  -->
    [       "<Psi/>"].
para('Omega'(_,_))  -->
    [     "<Omega/>"].
para('alpha'(_,_))  -->
    [     "<alpha/>"].
para('beta'(_,_))  -->
    [      "<beta/>"].
para('gamma'(_,_))  -->
    [     "<gamma/>"].
para('delta'(_,_))  -->
    [     "<delta/>"].
para('epsilon'(_,_))  -->
    [   "<epsilon/>"].
para('zeta'(_,_))  -->
    [      "<zeta/>"].
para('eta'(_,_))  -->
    [       "<eta/>"].
para('theta'(_,_))  -->
    [     "<theta/>"].
para('iota'(_,_))  -->
    [      "<iota/>"].
para('kappa'(_,_))  -->
    [     "<kappa/>"].
para('lambda'(_,_))  -->
    [    "<lambda/>"].
para('mu'(_,_))  -->
    [        "<mu/>"].
para('nu'(_,_))  -->
    [        "<nu/>"].
para('xi'(_,_))  -->
    [        "<xi/>"].
para('omicron'(_,_))  -->
    [   "<omicron/>"].
para('pi'(_,_))  -->
    [        "<pi/>"].
para('rho'(_,_))  -->
    [       "<rho/>"].
para('sigmaf'(_,_))  -->
    [    "<sigmaf/>"].
para('sigma'(_,_))  -->
    [     "<sigma/>"].
para('tau'(_,_))  -->
    [       "<tau/>"].
para('upsilon'(_,_))  -->
    [   "<upsilon/>"].
para('phi'(_,_))  -->
    [       "<phi/>"].
para('chi'(_,_))  -->
    [       "<chi/>"].
para('psi'(_,_))  -->
    [       "<psi/>"].
para('omega'(_,_))  -->
    [     "<omega/>"].
para('thetasym'(_,_))  -->
    [  "<thetasym/>"].
para('upsih'(_,_))  -->
    [     "<upsih/>"].
para('piv'(_,_))  -->
    [       "<piv/>"].
para('bull'(_,_))  -->
    [      "<bull/>"].
para('hellip'(_,_))  -->
    [    "<hellip/>"].
para('prime'(_,_))  -->
    [     "<prime/>"].
para('Prime'(_,_))  -->
    [     "<Prime/>"].
para('oline'(_,_))  -->
    [     "<oline/>"].
para('frasl'(_,_))  -->
    [     "<frasl/>"].
para('weierp'(_,_))  -->
    [    "<weierp/>"].
para('image'(_,_))  -->
    [     "<imaginary/>"].
para('real'(_,_))  -->
    [      "<real/>"].
para('trade'(_,_))  -->
    [     "<trademark/>"].
para('alefsym'(_,_))  -->
    [   "<alefsym/>"].
para('larr'(_,_))  -->
    [      "<larr/>"].
para('uarr'(_,_))  -->
    [      "<uarr/>"].
para('rarr'(_,_))  -->
    [      "<rarr/>"].
para('darr'(_,_))  -->
    [      "<darr/>"].
para('harr'(_,_))  -->
    [      "<harr/>"].
para('crarr'(_,_))  -->
    [     "<crarr/>"].
para('lArr'(_,_))  -->
    [      "<lArr/>"].
para('uArr'(_,_))  -->
    [      "<uArr/>"].
para('rArr'(_,_))  -->
    [      "<rArr/>"].
para('dArr'(_,_))  -->
    [      "<dArr/>"].
para('hArr'(_,_))  -->
    [      "<hArr/>"].
para('forall'(_,_))  -->
    [    "<forall/>"].
para('part'(_,_))  -->
    [      "<part/>"].
para('exist'(_,_))  -->
    [     "<exist/>"].
para('empty'(_,_))  -->
    [     "<empty/>"].
para('nabla'(_,_))  -->
    [     "<nabla/>"].
para('isin'(_,_))  -->
    [      "<isin/>"].
para('notin'(_,_))  -->
    [     "<notin/>"].
para('ni'(_,_))  -->
    [        "<ni/>"].
para('prod'(_,_))  -->
    [      "<prod/>"].
para('sum'(_,_))  -->
    [       "<sum/>"].
para('minus'(_,_))  -->
    [     "<minus/>"].
para('lowast'(_,_))  -->
    [    "<lowast/>"].
para('radic'(_,_))  -->
    [     "<radic/>"].
para('prop'(_,_))  -->
    [      "<prop/>"].
para('infin'(_,_))  -->
    [     "<infin/>"].
para('ang'(_,_))  -->
    [       "<ang/>"].
para('and'(_,_))  -->
    [       "<and/>"].
para('or'(_,_))  -->
    [        "<or/>"].
para('cap'(_,_))  -->
    [       "<cap/>"].
para('cup'(_,_))  -->
    [       "<cup/>"].
para('int'(_,_))  -->
    [       "<int/>"].
para('there4'(_,_))  -->
    [    "<there4/>"].
para('sim'(_,_))  -->
    [       "<sim/>"].
para('cong'(_,_))  -->
    [      "<cong/>"].
para('asymp'(_,_))  -->
    [     "<asymp/>"].
para('ne'(_,_))  -->
    [        "<ne/>"].
para('equiv'(_,_))  -->
    [     "<equiv/>"].
para('le'(_,_))  -->
    [        "<le/>"].
para('ge'(_,_))  -->
    [        "<ge/>"].
para('sub'(_,_))  -->
    [       "<sub/>"].
para('sup'(_,_))  -->
    [       "<sup/>"].
para('nsub'(_,_))  -->
    [      "<nsub/>"].
para('sube'(_,_))  --> 
    [      "<sube/>"].
para('supe'(_,_))  -->
    [      "<supe/>"].
para('oplus'(_,_))  -->
    [     "<oplus/>"].
para('otimes'(_,_))  -->
    [    "<otimes/>"].
para('perp'(_,_))  -->
    [      "<perp/>"].
para('sdot'(_,_))  -->
    [      "<sdot/>"].
para('lceil'(_,_))  -->
    [     "<lceil/>"].
para('rceil'(_,_))  -->
    [     "<rceil/>"].
para('lfloor'(_,_))  -->
    [    "<lfloor/>"].
para('rfloor'(_,_))  -->
    [    "<rfloor/>"].
para('lang'(_,_))  -->
    [      "<lang/>"].
para('rang'(_,_))  -->
    [      "<rang/>"].
para('loz'(_,_))  -->
    [       "<loz/>"].
para('spades'(_,_))  -->
    [    "<spades/>"].
para('clubs'(_,_))  -->
    [     "<clubs/>"].
para('hearts'(_,_))  -->
    [    "<hearts/>"].
para('diams'(_,_))  -->
    [     "<diams/>"].
para('amp'(_,_))  -->
    [       "&amp;"].
para('lt'(_,_))  -->
    [        "&lt;"].
para('gt'(_,_))  -->
    [        "&gt;"].
para('OElig'(_,_))  -->
    [     "<OElig/>"].
para('oelig'(_,_))  -->
    [     "<oelig/>"].
para('Scaron'(_,_))  -->
    [    "<Scaron/>"].
para('scaron'(_,_))  -->
    [    "<scaron/>"].
para('Yuml'(_,_))  -->
    [      "<Yumlaut/>"].
para('circ'(_,_))  -->
    [      "<circ/>"].
para('tilde'(_,_))  -->
    [     "<tilde/>"].
para(ensp(_,_)) -->
    [ "<ensp/>"].
para('emsp'(_,_))  -->
    [ "<emsp/>"].
para('thinsp'(_,_))  -->
    [   "<thinsp/>"].
para('zwnj'(_,_))  -->
    [  "<zwnj/>"].
para('zwj'(_,_)) -->
    [ "<zwj/>"].
para('lrm'(_,_)) -->
    [ "<lrm/>"].
para('rlm'(_,_)) -->
    [ "<rlm/>"].
para('ndash'(_,_)) -->
    [ "<ndash/>"].
para('mdash'(_,_)) -->
    [ "<mdash/>"].
para('lsquo'(_,_)) -->
    [ "<lsquo/>"].
para('rsquo'(_,_)) -->
    [ "<rsquo/>"].
para('sbquo'(_,_)) -->
    [ "<sbquo/>"].
para('ldquo'(_,_)) -->
    [ "<ldquo/>"].
para('rdquo'(_,_)) -->
    [ "<rdquo/>"].
para('bdquo'(_,_)) -->
    [ "<bdquo/>"].
para('dagger'(_,_)) -->
    [ "<dagger/>"].
para('Dagger'(_,_)) -->
    [ "<Dagger/>"].
para('permil'(_,_)) -->
    [ "<permil/>"].
para('lsaquo'(_,_)) -->
    [ "<lsaquo/>"].
para('rsaquo'(_,_)) -->
    [ "<rsaquo/>"].
para('euro'(_,_)) -->
    [ "<euro/>"].

%  // doxygen extension to the HTML4 table of HTML entities
para('tm'(_,_))  -->
    [    "<tm/>"].
para('apos'(_,_))  -->
    [ "&apos;"].

%  // doxygen commands represented as HTML entities
para('BSlash'(_,_)) -->
    [ "\\"].
para('BSlash'(_,_)) -->
    [ "@"].
para('Less'(_,_)) -->
    [ "&lt;"].
para('Greater'(_,_)) -->
    [ "&lt;"].
%<!-- end workaround for xsd.exe -->
unimpl(Cmd,Arg) -->
    { format(user_error,'unimplemented: ~w (called with ~w)',[Cmd,Arg]) }.



split_domains([],[],[],[]).
split_domains([briefdescription-A|All],[A|Bs],Ds,Ts):-
    !,
    split_domains(All,Bs,Ds,Ts).
split_domains([detaileddescription-A|All],Bs,[A|Ds],Ts):-
    !,
    split_domains(All,Bs,Ds,Ts).
split_domains([_-A|All],Bs,Ds,[A|Ts]):-
    !,
    split_domains(All,Bs,Ds,Ts).

inner(S,P) :-
    string_chars(S,Cs),
    append(_Prefix,['_'|Pos],Cs),
    maplist(char_type_xdigit, Pos),
    length(Pos,Len),
    Len> 30,
    !,
    string_chars(P,Pos).


short_ref(Ref,Short) :-
    inner(Ref,Short),
    !.
short_ref(Ref,Ref).


%% ref(+Link,+Name)
% -translate a ref to mkdocs
%
ref(S,W) -->
%     {writeln(S:W)},
    { inner(S,P)
    },
    !,
    { decode(W,L)
    },

    { format(string(Str),'[~s][~s]' ,[L,P]) },
    [Str].
ref(S,W)-->
    { decode(W,L)
    },
    { format(string(Str),'[~s](~s.md)' ,[L,S]) },
    [Str].

%% ref(+Link,+Name)
% create a new target
%

key_in(X,[X|_]) :- !.
key_in(X,[_|L]) :-
    key_in(X,L).

to_predicate(P,P).

strip_module_from_pred(ROS,EOS,Final):-
    sub_string(ROS,Left,3,Right,"::P"),
    !,
    sub_string(ROS,0,Left,_,Mod),
    sub_string(ROS,_,Right,0,Name),
    string_concat([Mod,":",Name,"/",EOS],Final).
strip_module_from_pred(ROS,EOS,Final) :-
    sub_string(ROS,0,1,Left,"P"),
    !,
    sub_string(ROS,1,Left,0,Name),
    string_concat([Name,"/",EOS],Final).
strip_module_from_pred(ROS,EOS,Final) :-
    sub_string(ROS,Left,_,Right,"::"),
    sub_string(ROS,0,Left,_,NMod),
    sub_string(ROS,_,Right,0,NPred),
    strip_module_from_pred(NPred,EOS,SemiFinal),
    !,
    string_concat([NMod,":",SemiFinal],Final).


get_safe_name(S,N) :-
get_name(S,N0),
   decode(N0,N).
get_name([Name],Name) :-
string(Name),
!.
get_name(Children,Name) :-
    key_in(qualifiedname(_,NameS ),Children),
    (
	NameS = [_,[Name]]
    ;
    NameS = [Name]
    ;
    NameS = Name
    ),
    string(Name),
    !.
get_name(Children,PName) :-
    key_in(compoundname(_,NameS ),Children),
    (
	NameS = [_,[Name]]
    ;
    NameS = [Name]
    ;
    NameS = Name
    ),
    string(Name),
decode(Name,PName),
    !.
get_name(Children,Name) :-
    key_in(name(_,NameS ),Children),
    (
	NameS = [_,[Name]]
    ;
    NameS = [Name]
    ;
    NameS = Name
    ),
    string(Name),
    !.


anchor([id(Ref)],[]) -->
  ["[](){#",Ref,"}\n"].

gengroup(Ref0) :-
abolish(visited/1),
string_concat("/group__",Ref0,Ref),
Kind="group",
unix(argv([IDir,ODir,_])),
    	trl(compound([refid(Ref),kind(Kind)],[]),IDir,ODir).

genclass(Ref0) :-
abolish(visited/1),
Kind="class",
unix(argv([IDir,ODir,_])),
    	trl(compound([refid(Ref0),kind(Kind)],[]),IDir,ODir).

 % 
