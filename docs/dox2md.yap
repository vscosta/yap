

:- use_module(library(maplist)).
:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(xml2yap)).

%:- dynamic group/8, predicate/8, show/0.

:- set_prolog_flag(double_quotes, string).

:- multifile extrabrief/2, class/8, predicate/8 , v/2, f/3, c/2,extra/2, brief/2.
:- dynamic parent/2.

in(X,[X|_]) :- !.
in(X,[_|L]) :-
    in(X,L).

main :-
    unix(argv(Params)),
    main_process(Params).

main_process([IDir,ODir]) :-
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
    classes(XMLTasks,InputDir,OutputDir),
    groups(XMLTasks,InputDir,OutputDir).

main_process([File ,_]) :-
    atom_concat(Input,'.xml',File),
    xml_load(File,XML),
    XML = [doxygen(_,XMLData)],
    XMLData = [compounddef(Atts,Children)],
    compounddef([input=Input,ofile=user_output],Atts,Children,_Id,Name,"[].",AllRaw),
    split_domains(AllRaw,[Brief],[Details],FullText),
    string_concat(["# ",Name, "   ", Brief,"\n\n",Details,"\n\n",FullText],All),
    atom_concat(Input,'.md',OFile),
    open(OFile,write,O),
    format(O,'~s',[All]),
    close(O).


classes(XMLTasks,InputDir,OutputDir) :-
    maplist(do_compound_d(1,InputDir,OutputDir),XMLTasks).

groups(XMLTasks,InputDir,OutputDir) :-
    maplist(do_compound_d(2,InputDir,OutputDir),XMLTasks).

do_compound_d(N2,InputDir,OutputDir,XMLTask) :-
    catch(
do_compound(N2, InputDir,OutputDir,XMLTask),
Error,
 format(user_error,'ERROR :- ~w crashed with error ~w',[XMLTask,Error])
    ),
    !.
do_compound_d(_N2,_InputDir,_OutputDir,XMLTask) :-
    format(user_error,'ERROR :- ~w failed.~n', [XMLTask]).

 do_compound(1,IDir,ODir,compound(OAtts,OProps)) :-
    in(kind("class"),OAtts),
    in(name([],[OName]),OProps),
    !,
    in(refid(Id),OAtts),
    atom_concat([IDir,Id,'.xml'], IFile),
    catch(xml_load(IFile,XML),Error,(format(user_error,'failed while processsing ~w: ~w',[IFile,                      Error]), fail)),
    XML = [doxygen(_,XMLData)],
    XMLData = [compounddef(_Atts,Children)],
      State = [id=Id,kind="class",name=OName],
      Children=[compoundname([],[Name])|Ch2],
writeln(OName),
     foldl(process_(State),Ch2,[],AllRaw),
    split_domains(AllRaw,[Brief],[Details],FullText),
    string_concat(["# ",Name, "   ", Brief,"\n\n",Details,"\n\n",FullText],All),
   atom_concat([ODir,"/",Id,'.md'],OFile),
    open(OFile,write,O),
    format(O,'~s',[All]),
    close(O).
do_compound(1,IDir,_ODir,compound(OAtts,OProps)) :-
    in(kind("group"),OAtts),
    in(name([],[OName]),OProps),
    !,
    in(refid(Id),OAtts),
    atom_concat([IDir,'/',Id,'/','.xml'], IFile),
    catch(xml_load(IFile,XML),Error,(printf("failed while processsing ~w: ~w",[OName,                   Error]), fail)),
    writeln(OName),
    XML = [doxygen(_,XMLData)],
    XMLData = [compounddef(_Atts,Children)],
    ( in(briefdescription(BArgs,Paras),Children) ->
      writeln(OName),
      briefs([briefdescription(BArgs,Paras)|_],[],"",Brief),
      writeln(Brief),
    assert_static(brief(Id,Brief))
      ;
      true
    ).
do_compound(2,IDir,ODir,compound(OAtts,OProps)) :-
    in(kind("group"),OAtts),
    in(name([],[OName]),OProps),
    !,
    in(refid(Id),Atts),
    atom_concat([IDir,Id,'.xml'], IFile),
    catch(xml_load(IFile,XML),Error,(printf("failed while processsing ~w: ~w",[OName,                      Error]), fail)),
    XML = [doxygen(_,XMLData)],
    XMLData = [compounddef(Atts,[_|Children])],
    State = [id=Id,kind="group",name=OName],
    process_(State,Children,[],AllRaw),
    split_domains(AllRaw,[Brief],[Details],FullText),
    compounddef([id=Id,kind="group",name=OName],Atts,Children,_Id,Name,[],AllRaw),
    split_domains(AllRaw,[Brief],[Details],FullText),
    string_concat(["# ",Name, "   ", Brief,"\n\n",Details,"\n\n",FullText],All),
    atom_concat([ODir,"/",Id,'.md'],OFile),
    open(OFile,write,O),
    format(O,'~s',[All]),
    close(O).
do_compound(_,_,_,_).



   

process_(State,Op) -->
    {
      functor(Op,N,_),
      writeln(N),
      process(State,Op,"",St)
     , writeln(N)
    },
    [N-St],
    !.
/*
process(State,basecompoundref(Atts,Children)) -->
    !,
    (State,basecompoundref(Atts,Children)).
process(State,derivedcompoundref(Atts,Children)) -->
    !,
    seq(State,derivedcompoundref(Atts,Children)).
  */  % incType
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
    % ignoreseq(NState,innernamespace(_,_),Innerclass,Innernamespace),
    % ignoreseq(NState,innerpage(_,_),Innernamespace,Innerpage),
    % ignoreseq(NState,innergroup(_,_),Innerpage,Innergroup),
    % ignoreseq(NState,qualifier(_,_),Innergroup,Qualifier),
    % ignoreseq(NState,templateparamlist(_,_),Qualifier,Templateparamlist),
process(_,sectiondef(Atts,Children)) -->
    !,
    sectiondef(sectiondef(Atts,Children)).
    % ignoreseq(NState,tableofcontents(_,_),Sectiondef,Tableofcontents),
    % ignoreseq(NState,requiresclause(_,_),Tableofcontents,Requiresclause),
    % ignoreseq(NState,initializer(_,_),Requiresclause,Initializer),
process(_,briefdescription(Atts,Children)) -->
    !,
    briefs(briefdescription(Atts,Children)).
process(_,detaileddescription(Atts,Children)) -->
    !,
    detaileds(detaileddescription(Atts,Children)).
    % ignoreseq(NState,exports(_,_),Detammmmtrnnmmmjjjjjileddescription,Exports),
    % ignoreseq(NState,inheritancegraph(_,_),Exports,Inheritancegraph),
    % ignoreseq(NState,collaborationgraph(_,_),Inheritancegraph,Collaborationgraph),
    % ignoreseq(NState,programlisting(_,_),Collaborationgraph,Programlisting),
    % ignoreseq(NState,location(_,_),Programlisting,Location),
    % ignoreseq(NState,listofallmembers(_,_),Location,[]).

process(_,_)--> [].

    %sectiondef(A,Remainder-) --> {writeln(A),fail}.
sectiondef(sectiondef(Atts,Els)) -->
    {
      in(kind(Kind),Atts)
    },
    { top_sectiondef_name(Kind,Name),writeln(Name)
    },
    !,
    mcstr(["## ",Name,":\n"]),
    foldl(sectdef,Els).


sectdef(description(_,Ds))-->
  descriptions(Ds).
sectdef(header([],[Text]))-->
  mcstr(["\n",Text,"\n"]).
sectdef(member(Atts,Children))-->
    {      in(refid(Ref),Atts),
      in(name(_,[Name] ),Children),writeln(Name)
    },
    
    !,
    ref(Ref,Name).
sectdef(memberdef,Children))-->
    {
      in(id(Ref),Atts),
      in(defname(_,[Name] ),Children),writeln(Name)
    },
    !,
    defref(Ref,Name),
   (
    { in(definition([],Def),Children) }
    ->
    descriptions([" ",Def])
    ;
    ({ in(type([],Type),Children),
     in(argsstring([],Args),Children) }
      )
      ->
      paras([" ",Type,Name,Args])
      ;
    true
      
    ),

   (	{ in(briefdescription([],Brief),Children) }
	->
	cstr(": "),
	descriptions(Brief)
	;
	true
      ),
      (
	{ in(inbodydescription([],InBody),Children) }
	->
	cstr("\n  "),
	descriptions(InBody)
	;
	true
      ),
      (
	{ in(detaileddescription([],Detailed),Children)}
	->
	cstr("\n\n"),
	descriptions(Detailed)
	;
	true
      ),
      !,
      cstr("\n").

%sectiondef(A,Remainder) --> {writeln(A),fail}.
briefs(briefdescription(_Atts,Els)) -->
    !,
    descriptions(Els).

detaileds(detaileddescription(_Atts,Els)) -->
    !,
    cstr("\n"),
    separatedescriptions(Els),
    cstr("\n").

separatedescriptions([]) --> [].
separatedescriptions([D|Detailed]) -->
    %    {writeln(D)},
    description(D),
    cstr("\n\n"),
    separatedescriptions( Detailed).

descriptions([]) --> [].
descriptions([D|Detailed]) -->
    %    {writeln(D)},
    description(D),
    descriptions( Detailed).

toraw(Items) -->
    cstr("~~~\n"),
    foldl(mkraw,Items),
    cstr("\n~~~\n").

mkraw(codeline(_,Line)) -->
    cstr("\n"),
    foldl(raw,Line).
    
    raw(highlight(_,Text)) -->
    mcstr(Text).
   
doxolist(_Pars,Items) -->
    foldl(item("1"),Items).

itemlist(_Pars,Items) -->
    foldl(item("i"),Items).

item(Type,listitem(_,Para)) -->
    cstr("\n"),
    typel(Type),
    descriptions(Para),
    cstr("\n").
cstr("\n").

typel("1") -->
    cstr( "1. ").
typel("a") -->
    cstr( "a. ").
typel("A") -->
    cstr( "A. ").
typel("i") -->
    cstr( "- ").
typel("I") -->
    cstr( "* ").

description(para([],S)) -->
    {string(S)},
    !,
    cstr(S).
description(S) -->
    { string(S) },
    !,
    cstr(S).
description(title([],S)) -->
    { string(S) },
    !,
    cstr(S).
description(sect1([],S)) -->
    cstr("### "),
    (
      in(title(_,T),S)
      ->
      mcstr([T,"\n"]);
      cstr("\n")
    ),
    descriptions(S).
description(sect2([],S)) -->
    cstr("#### "),
    (
      in(title(_,T),S)
      ->
      mcstr([T,"\n"]);
      cstr("\n")
    ),
    descriptions(S).
description(sect3([],S)) -->
    cstr("##### "),
    (
      in(title(_,T),S)
      ->
      mcstr([T,"\n"]);
      cstr("\n")
    ),
    descriptions(S).
description(para([],S)) -->
    descriptions(S).
description(S) -->
    para(S).

seq(State,G0) -->
    {
      G0=..[_N,Atts,[[[],Name]|Els]],
      in(refid(Ref),Atts),
      !,
      seqhdr(State,Type)
    } ,
    mcstr(["## ",Type,": "]),
    ref(Ref,Name),
    foldl(seqdef,Els),
    cstr("\n").
seq(State,G0) -->
    {
      arg(2,G0,Name),
      !,
      seqhdr(State,Type)
    } ,
    mcstr(["## ",Type,": ",Name,"\n"]).

seqhdr(State,Name) :-
    in(kind=Kind, State),
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
top_sectiondef_name( "func", "Func" ).
top_sectiondef_name(   "var", "Var" ).





para(bold([],Text)) -->
    {string(Text)},
    !,
    mcstr(["**", Text,"**"]).
para(bold([],Text)) -->
    cstr("**"), para(Text), cstr("**").
para(s([],Text)) -->
    {string(Text)},
    !,
    mcstr(["~~",Text,"~~"]).
para(s([],Text)) -->
    cstr("~~"), para(Text), cstr("~~").
para(cstrike([],Text)) -->
    {string(Text)},
    !,
    mcstr(["~~",Text,"~~"]).
para(cstrike([],Text)) -->
    !,
    cstr("~~"), para(Text), cstr("~~").
para(underline([],Text)) -->
    {string(Text)},
    !,
    mcstr(["<ins>",Text,"<ins>"]).
    para(underline([],Text)) -->
    cstr("<ins>"),
    para(Text),
    cstr("</ins>").
para(emphasis([],Text)) -->
    {string(Text)},
    !,
    mcstr(["__", Text, "__"]).
para(emphasis([],Text)) -->
    cstr("__"), para(Text), cstr("__").
para(hruler([],_)) -->
    cstr("\n- - -\n").
para(preformatted([],Text)) -->
    unimpl(preformatted,Text). % docMarkupType
para(programlisting([],Text)) -->
    toraw(Text). % listingType
para(verbatim([],Text)) -->
    {string(Text)},
    !,
    mcstr(["`",Text,"`"]).
para(verbatim([],Text)) -->
    cstr("`"),
    para(Text),
    cstr("`"). % 
para(javadocliteral([],Text)) -->
    unimpl(javadocliteral,Text). % xsd:unimpling
para(javadoccode([],Text)) -->
    unimpl(javadoccode,Text). % xsd:unimpling
para(indexentry([],Text)) -->
    unimpl(indexentry,Text). % docIndexEntryType
para(orderedlist(Atts,Text)) -->
    doxolist(Atts,Text). % docListType
para(itemizedlist(Atts,Text)) -->
    itemlist(Atts,Text). % docListType
para(simplesect([],Text)) -->
    unimpl(simplesect,Text). % docSimpleSectType
para(title([],Text)) -->
    unimpl(title,Text). % docTitleType
para(variablelist([],Text)) -->
    unimpl(variablelist,Text). % docVariableListType
para(table([],Text)) -->
    unimpl(table,Text). % docTableType
para(heading([],Text)) -->
    unimpl(heading,Text). % docHeadingType
para(dotfile([],Text)) -->
    unimpl(dotfile,Text). % docImageFileType
para(mscfile([],Text)) -->
    unimpl(mscfile,Text). % docImageFileType
para(diafile([],Text)) -->
    unimpl(diafile,Text). % docImageFileType
para(toclist([],Text)) -->
    unimpl(toclist,Text). % docTocListType
para(language([],Text)) -->
    unimpl(language,Text). % docLanguageType
para(parameterlist([],Text)) -->
    unimpl(eterlist,Text). % docParamListType
para(xrefsect([],Text)) -->
    unimpl(xrefsect,Text). % docXRefSectType
para(copydoc([],Text)) -->
    unimpl(copydoc,Text). % docCopyType
para(details([],Text)) -->
    unimpl(details,Text). % docDetailsType
para(blockquote([],Text)) -->
    cstr("\n~~~\n"),
    descriptions(Text),
    cstr("\n~~~\n"). % docBlockQuoteType
para(parblock([],Text))-->
    paras(Text). % docParBlockType         
para(computeroutput([],Text)) -->
    string(Text),
    !,
    mcstr(["`", Text, "`"]).
para(computeroutput([],Text)) -->
    cstr("`"), para(Text), cstr("`").
para(subscript([],Text)) -->
    string(Text),
    !,
    mcstr(["<sub>", Text, "<sub>"]).
para(subscript([],Text)) -->
    cstr("<sub>"), para(Text), cstr("</sub>").
para(superscript([],Text)) -->
    string(Text),
    !,
    mcstr(["<sup>", Text, "<sup>"]).
para(superscript([],Text)) -->
    cstr("<sup>"), para(Text), cstr("</sup>").
para(center([],Text)) --> % unsupported
    para(Text). % docMarkupType
para(small([],Text)) -->
    cstr("<small>"), para(Text), cstr("</small>").
para(cite([],Text)) -->
    para(Text). % docMarkupType
para(del([],Text)) -->
    cstr("<del>"), para(Text), cstr("</del>").
para(ins([],Text)) -->
    cstr("<ins>"), para(Text), cstr("</ins>").
para(htmlonly([],_Text)) -->
    []. % docHtmlOnlyType
para(manonly([],_Text)) -->
    [].
para(xmlonly([],Text)) -->
    para(Text). % xsd:cstring
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
para(anchor([],Text)) -->
    para(Text). % docAnchorType
para(ref(Atts,[Name])) -->
    {
  once(in(refid(Ref),Atts))
    },
    ref(Ref,Name).
para(linebreak([],_)) -->
    cstr("<br>"). % docEmptyType
para(nonbreakablespace([],_)) -->
    cstr(     "<nonbreakablespace/>").
para('iexcl'(_,_))  -->
    cstr(    "<iexcl/>").
para('cent'(_,_))  -->
    cstr(     "<cent/>").
para('pound'(_,_))  -->
    cstr(    "<pound/>").
para('curren'(_,_))  -->
    cstr(   "<curren/>").
para('yen'(_,_))  -->
    cstr(      "<yen/>").
para('brvbar'(_,_))  -->
    cstr(   "<brvbar/>").
para('sect'(_,_))  -->
    cstr(     "<sect/>").
para('uml'(_,_))  -->
    cstr(      "<umlaut/>").
para('copy'(_,_))  -->
    cstr(     "<copy/>").
para('ordf'(_,_))  -->
    cstr(     "<ordf/>").
para('laquo'(_,_))  -->
    cstr(    "<laquo/>").
para('not'(_,_))  -->
    cstr(      "<not/>").
para('shy'(_,_))  -->
    cstr(      "<shy/>").
para('reg'(_,_))  -->
    cstr(      "<registered/>").
para('macr'(_,_))  -->
    cstr(     "<macr/>").
para('deg'(_,_))  -->
    cstr(      "<deg/>").
para('plusmn'(_,_))  -->
    cstr(   "<plusmn/>").
para('sup2'(_,_))  -->
    cstr(     "<sup2/>").
para('sup3'(_,_))  -->
    cstr(     "<sup3/>").
para('acute'(_,_))  -->
    cstr(    "<acute/>").
para('micro'(_,_))  -->
    cstr(    "<micro/>").
para('para'(_,_))  -->
    cstr(     "<para/>").
para('middot'(_,_))  -->
    cstr(   "<middot/>").
para('cedil'(_,_))  -->
    cstr(    "<cedil/>").
para('sup1'(_,_))  -->
    cstr(     "<sup1/>").
para('ordm'(_,_))  -->
    cstr(     "<ordm/>").
para('raquo'(_,_))  -->
    cstr(    "<raquo/>").
para('frac14'(_,_))  -->
    cstr(   "<frac14/>").
para('frac12'(_,_))  -->
    cstr(   "<frac12/>").
para('frac34'(_,_))  -->
    cstr(   "<frac34/>").
para('iquest'(_,_))  -->
    cstr(   "<iquest/>").
para('Agrave'(_,_))  -->
    cstr(   "<Agrave/>").
para('Aacute'(_,_))  -->
    cstr(   "<Aacute/>").
para('Acirc'(_,_))  -->
    cstr(    "<Acirc/>").
para('Atilde'(_,_))  -->
    cstr(   "<Atilde/>").
para('Auml'(_,_))  -->
    cstr(     "<Aumlaut/>").
para('Aring'(_,_))  -->
    cstr(    "<Aring/>").
para('AElig'(_,_))  -->
    cstr(    "<AElig/>").
para('Ccedil'(_,_))  -->
    cstr(   "<Ccedil/>").
para('Egrave'(_,_))  -->
    cstr(   "<Egrave/>").
para('Eacute'(_,_))  -->
    cstr(   "<Eacute/>").
para('Ecirc'(_,_))  -->
    cstr(    "<Ecirc/>").
para('Euml'(_,_))  -->
    cstr(     "<Eumlaut/>").
para('Igrave'(_,_))  -->
    cstr(   "<Igrave/>").
para('Iacute'(_,_))  -->
    cstr(   "<Iacute/>").
para('Icirc'(_,_))  -->
    cstr(    "<Icirc/>").
para('Iuml'(_,_))  -->
    cstr(     "<Iumlaut/>").
para('ETH'(_,_))  -->
    cstr(      "<ETH/>").
para('Ntilde'(_,_))  -->
    cstr(   "<Ntilde/>").
para('Ograve'(_,_))  -->
    cstr(   "<Ograve/>").
para('Oacute'(_,_))  -->
    cstr(   "<Oacute/>").
para('Ocirc'(_,_))  -->
    cstr(    "<Ocirc/>").
para('Otilde'(_,_))  -->
    cstr(   "<Otilde/>").
para('Ouml'(_,_))  -->
    cstr(     "<Oumlaut/>").
para('times'(_,_))  -->
    cstr(    "<times/>").
para('Oslash'(_,_))  -->
    cstr(   "<Oslash/>").
para('Ugrave'(_,_))  -->
    cstr(   "<Ugrave/>").
para('Uacute'(_,_))  -->
    cstr(   "<Uacute/>").
para('Ucirc'(_,_))  -->
    cstr(    "<Ucirc/>").
para('Uuml'(_,_))  -->
    cstr(     "<Uumlaut/>").
para('Yacute'(_,_))  -->
    cstr(   "<Yacute/>").
para('THORN'(_,_))  -->
    cstr(    "<THORN/>").
para('szlig'(_,_))  -->
    cstr(    "<szlig/>").
para('agrave'(_,_))  -->
    cstr(   "<agrave/>").
para('aacute'(_,_))  -->
    cstr(   "<aacute/>").
para('acirc'(_,_))  -->
    cstr(    "<acirc/>").
para('atilde'(_,_))  -->
    cstr(   "<atilde/>").
para('auml'(_,_))  -->
    cstr(     "<aumlaut/>").
para('aring'(_,_))  -->
    cstr(    "<aring/>").
para('aelig'(_,_))  -->
    cstr(    "<aelig/>").
para('ccedil'(_,_))  -->
    cstr(   "<ccedil/>").
para('egrave'(_,_))  -->
    cstr(   "<egrave/>").
para('eacute'(_,_))  -->
    cstr(   "<eacute/>").
para('ecirc'(_,_))  -->
    cstr(    "<ecirc/>").
para('euml'(_,_))  -->
    cstr(     "<eumlaut/>").
para('igrave'(_,_))  -->
    cstr(   "<igrave/>").
para('iacute'(_,_))  -->
    cstr(   "<iacute/>").
para('icirc'(_,_))  -->
    cstr(    "<icirc/>").
para('iuml'(_,_))  -->
    cstr(     "<iumlaut/>").
para('eth'(_,_))  -->
    cstr(      "<eth/>").
para('ntilde'(_,_))  -->
    cstr(   "<ntilde/>").
para('ograve'(_,_))  -->
    cstr(   "<ograve/>").
para('oacute'(_,_))  -->
    cstr(   "<oacute/>").
para('ocirc'(_,_))  -->
    cstr(    "<ocirc/>").
para('otilde'(_,_))  -->
    cstr(   "<otilde/>").
para('ouml'(_,_))  -->
    cstr(     "<oumlaut/>").
para('divide'(_,_))  -->
    cstr(   "<divide/>").
para('oslash'(_,_))  -->
    cstr(   "<oslash/>").
para('ugrave'(_,_))  -->
    cstr(   "<ugrave/>").
para('uacute'(_,_))  -->
    cstr(   "<uacute/>").
para('ucirc'(_,_))  -->
    cstr(    "<ucirc/>").
para('uuml'(_,_))  -->
    cstr(     "<uumlaut/>").
para('yacute'(_,_))  -->
    cstr(   "<yacute/>").
para('thorn'(_,_))  -->
    cstr(    "<thorn/>").
para('yuml'(_,_))  -->
    cstr(     "<yumlaut/>").
para('fnof'(_,_))  -->
    cstr(     "<fnof/>").
para('Alpha'(_,_))  -->
    cstr(    "<Alpha/>").
para('Beta'(_,_))  -->
    cstr(     "<Beta/>").
para('Gamma'(_,_))  -->
    cstr(    "<Gamma/>").
para('Delta'(_,_))  -->
    cstr(    "<Delta/>").
para('Epsilon'(_,_))  -->
    cstr(  "<Epsilon/>").
para('Zeta'(_,_))  -->
    cstr(     "<Zeta/>").
para('Eta'(_,_))  -->
    cstr(      "<Eta/>").
para('Theta'(_,_))  -->
    cstr(    "<Theta/>").
para('Iota'(_,_))  -->
    cstr(     "<Iota/>").
para('Kappa'(_,_))  -->
    cstr(    "<Kappa/>").
para('Lambda'(_,_))  -->
    cstr(   "<Lambda/>").
para('Mu'(_,_))  -->
    cstr(       "<Mu/>").
para('Nu'(_,_))  -->
    cstr(       "<Nu/>").
para('Xi'(_,_))  -->
    cstr(       "<Xi/>").
para('Omicron'(_,_))  -->
    cstr(  "<Omicron/>").
para('Pi'(_,_))  -->
    cstr(       "<Pi/>").
para('Rho'(_,_))  -->
    cstr(      "<Rho/>").
para('Sigma'(_,_))  -->
    cstr(    "<Sigma/>").
para('Tau'(_,_))  -->
    cstr(      "<Tau/>").
para('Upsilon'(_,_))  -->
    cstr(  "<Upsilon/>").
para('Phi'(_,_))  -->
    cstr(      "<Phi/>").
para('Chi'(_,_))  -->
    cstr(      "<Chi/>").
para('Psi'(_,_))  -->
    cstr(      "<Psi/>").
para('Omega'(_,_))  -->
    cstr(    "<Omega/>").
para('alpha'(_,_))  -->
    cstr(    "<alpha/>").
para('beta'(_,_))  -->
    cstr(     "<beta/>").
para('gamma'(_,_))  -->
    cstr(    "<gamma/>").
para('delta'(_,_))  -->
    cstr(    "<delta/>").
para('epsilon'(_,_))  -->
    cstr(  "<epsilon/>").
para('zeta'(_,_))  -->
    cstr(     "<zeta/>").
para('eta'(_,_))  -->
    cstr(      "<eta/>").
para('theta'(_,_))  -->
    cstr(    "<theta/>").
para('iota'(_,_))  -->
    cstr(     "<iota/>").
para('kappa'(_,_))  -->
    cstr(    "<kappa/>").
para('lambda'(_,_))  -->
    cstr(   "<lambda/>").
para('mu'(_,_))  -->
    cstr(       "<mu/>").
para('nu'(_,_))  -->
    cstr(       "<nu/>").
para('xi'(_,_))  -->
    cstr(       "<xi/>").
para('omicron'(_,_))  -->
    cstr(  "<omicron/>").
para('pi'(_,_))  -->
    cstr(       "<pi/>").
para('rho'(_,_))  -->
    cstr(      "<rho/>").
para('sigmaf'(_,_))  -->
    cstr(   "<sigmaf/>").
para('sigma'(_,_))  -->
    cstr(    "<sigma/>").
para('tau'(_,_))  -->
    cstr(      "<tau/>").
para('upsilon'(_,_))  -->
    cstr(  "<upsilon/>").
para('phi'(_,_))  -->
    cstr(      "<phi/>").
para('chi'(_,_))  -->
    cstr(      "<chi/>").
para('psi'(_,_))  -->
    cstr(      "<psi/>").
para('omega'(_,_))  -->
    cstr(    "<omega/>").
para('thetasym'(_,_))  -->
    cstr( "<thetasym/>").
para('upsih'(_,_))  -->
    cstr(    "<upsih/>").
para('piv'(_,_))  -->
    cstr(      "<piv/>").
para('bull'(_,_))  -->
    cstr(     "<bull/>").
para('hellip'(_,_))  -->
    cstr(   "<hellip/>").
para('prime'(_,_))  -->
    cstr(    "<prime/>").
para('Prime'(_,_))  -->
    cstr(    "<Prime/>").
para('oline'(_,_))  -->
    cstr(    "<oline/>").
para('frasl'(_,_))  -->
    cstr(    "<frasl/>").
para('weierp'(_,_))  -->
    cstr(   "<weierp/>").
para('image'(_,_))  -->
    cstr(    "<imaginary/>").
para('real'(_,_))  -->
    cstr(     "<real/>").
para('trade'(_,_))  -->
    cstr(    "<trademark/>").
para('alefsym'(_,_))  -->
    cstr(  "<alefsym/>").
para('larr'(_,_))  -->
    cstr(     "<larr/>").
para('uarr'(_,_))  -->
    cstr(     "<uarr/>").
para('rarr'(_,_))  -->
    cstr(     "<rarr/>").
para('darr'(_,_))  -->
    cstr(     "<darr/>").
para('harr'(_,_))  -->
    cstr(     "<harr/>").
para('crarr'(_,_))  -->
    cstr(    "<crarr/>").
para('lArr'(_,_))  -->
    cstr(     "<lArr/>").
para('uArr'(_,_))  -->
    cstr(     "<uArr/>").
para('rArr'(_,_))  -->
    cstr(     "<rArr/>").
para('dArr'(_,_))  -->
    cstr(     "<dArr/>").
para('hArr'(_,_))  -->
    cstr(     "<hArr/>").
para('forall'(_,_))  -->
    cstr(   "<forall/>").
para('part'(_,_))  -->
    cstr(     "<part/>").
para('exist'(_,_))  -->
    cstr(    "<exist/>").
para('empty'(_,_))  -->
    cstr(    "<empty/>").
para('nabla'(_,_))  -->
    cstr(    "<nabla/>").
para('isin'(_,_))  -->
    cstr(     "<isin/>").
para('notin'(_,_))  -->
    cstr(    "<notin/>").
para('ni'(_,_))  -->
    cstr(       "<ni/>").
para('prod'(_,_))  -->
    cstr(     "<prod/>").
para('sum'(_,_))  -->
    cstr(      "<sum/>").
para('minus'(_,_))  -->
    cstr(    "<minus/>").
para('lowast'(_,_))  -->
    cstr(   "<lowast/>").
para('radic'(_,_))  -->
    cstr(    "<radic/>").
para('prop'(_,_))  -->
    cstr(     "<prop/>").
para('infin'(_,_))  -->
    cstr(    "<infin/>").
para('ang'(_,_))  -->
    cstr(      "<ang/>").
para('and'(_,_))  -->
    cstr(      "<and/>").
para('or'(_,_))  -->
    cstr(       "<or/>").
para('cap'(_,_))  -->
    cstr(      "<cap/>").
para('cup'(_,_))  -->
    cstr(      "<cup/>").
para('int'(_,_))  -->
    cstr(      "<int/>").
para('there4'(_,_))  -->
    cstr(   "<there4/>").
para('sim'(_,_))  -->
    cstr(      "<sim/>").
para('cong'(_,_))  -->
    cstr(     "<cong/>").
para('asymp'(_,_))  -->
    cstr(    "<asymp/>").
para('ne'(_,_))  -->
    cstr(       "<ne/>").
para('equiv'(_,_))  -->
    cstr(    "<equiv/>").
para('le'(_,_))  -->
    cstr(       "<le/>").
para('ge'(_,_))  -->
    cstr(       "<ge/>").
para('sub'(_,_))  -->
    cstr(      "<sub/>").
para('sup'(_,_))  -->
    cstr(      "<sup/>").
para('nsub'(_,_))  -->
    cstr(     "<nsub/>").
para('sube'(_,_))  --> 
    cstr(     "<sube/>").
para('supe'(_,_))  -->
    cstr(     "<supe/>").
para('oplus'(_,_))  -->
    cstr(    "<oplus/>").
para('otimes'(_,_))  -->
    cstr(   "<otimes/>").
para('perp'(_,_))  -->
    cstr(     "<perp/>").
para('sdot'(_,_))  -->
    cstr(     "<sdot/>").
para('lceil'(_,_))  -->
    cstr(    "<lceil/>").
para('rceil'(_,_))  -->
    cstr(    "<rceil/>").
para('lfloor'(_,_))  -->
    cstr(   "<lfloor/>").
para('rfloor'(_,_))  -->
    cstr(   "<rfloor/>").
para('lang'(_,_))  -->
    cstr(     "<lang/>").
para('rang'(_,_))  -->
    cstr(     "<rang/>").
para('loz'(_,_))  -->
    cstr(      "<loz/>").
para('spades'(_,_))  -->
    cstr(   "<spades/>").
para('clubs'(_,_))  -->
    cstr(    "<clubs/>").
para('hearts'(_,_))  -->
    cstr(   "<hearts/>").
para('diams'(_,_))  -->
    cstr(    "<diams/>").
para('quot'(_,_))  -->
    cstr("&quot;").
para('amp'(_,_))  -->
    cstr(      "&amp;").
para('lt'(_,_))  -->
    cstr(       "&lt;").
para('gt'(_,_))  -->
    cstr(       "&gt;").
para('OElig'(_,_))  -->
    cstr(    "<OElig/>").
para('oelig'(_,_))  -->
    cstr(    "<oelig/>").
para('Scaron'(_,_))  -->
    cstr(   "<Scaron/>").
para('scaron'(_,_))  -->
    cstr(   "<scaron/>").
para('Yuml'(_,_))  -->
    cstr(     "<Yumlaut/>").
para('circ'(_,_))  -->
    cstr(     "<circ/>").
para('tilde'(_,_))  -->
    cstr(    "<tilde/>").
para(ensp(_,_)) -->
    cstr("<ensp/>").
para('emsp'(_,_))  -->
    cstr("<emsp/>").
para('thinsp'(_,_))  -->
    cstr(  "<thinsp/>").
para('zwnj'(_,_))  -->
    cstr( "<zwnj/>").
para('zwj'(_,_)) -->
    cstr("<zwj/>").
para('lrm'(_,_)) -->
    cstr("<lrm/>").
para('rlm'(_,_)) -->
    cstr("<rlm/>").
para('ndash'(_,_)) -->
    cstr("<ndash/>").
para('mdash'(_,_)) -->
    cstr("<mdash/>").
para('lsquo'(_,_)) -->
    cstr("<lsquo/>").
para('rsquo'(_,_)) -->
    cstr("<rsquo/>").
para('sbquo'(_,_)) -->
    cstr("<sbquo/>").
para('ldquo'(_,_)) -->
    cstr("<ldquo/>").
para('rdquo'(_,_)) -->
    cstr("<rdquo/>").
para('bdquo'(_,_)) -->
    cstr("<bdquo/>").
para('dagger'(_,_)) -->
    cstr("<dagger/>").
para('Dagger'(_,_)) -->
    cstr("<Dagger/>").
para('permil'(_,_)) -->
    cstr("<permil/>").
para('lsaquo'(_,_)) -->
    cstr("<lsaquo/>").
para('rsaquo'(_,_)) -->
    cstr("<rsaquo/>").
para('euro'(_,_)) -->
    cstr("<euro/>").
%  // doxygen extension to the HTML4 table of HTML entities
para('tm'(_,_))  -->
    cstr(   "<tm/>").
para('apos'(_,_))  -->
    cstr("&apos;").

%  // doxygen commands represented as HTML entities
para('BSlash'(_,_)) -->
    cstr("\\").
para('BSlash'(_,_)) -->
    cstr("@").
para('Less'(_,_)) -->
    cstr("&lt;").
para('Greater'(_,_)) -->
    cstr("&lt;").
%<!-- end workaround for xsd.exe -->
para(computeroutput([],Text)) -->
    cstr("`"), para(Text), cstr("`").
para(subscript([],Text)) -->
    cstr("<sub>"), para(Text), cstr("</sub>").
para(superscript([],Text)) -->
    cstr("<sup>"), para(Text), cstr("</sup>").
para(center([],Text)) --> % unsupported
    para(Text). % docMarkupType
para(small([],Text)) -->
    cstr("<small>"), para(Text), cstr("</small>").
para(cite([],Text)) -->
    para(Text). % docMarkupType
para(del([],Text)) -->
    cstr("<del>"), para(Text), cstr("</del>").
para(ins([],Text)) -->
    cstr("<ins>"), para(Text), cstr("</ins>").
para(htmlonly([],_Text)) -->
    []. % docHtmlOnlyType
para(manonly([],_Text)) -->
    [].
para(xmlonly([],Text)) -->
    para(Text). % xsd:cstring
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
para(anchor([],Text)) -->
    para(Text). % docAnchorType
para(ref(Atts,[Name])) -->
    {
  once(in(refid(Ref),Atts))
    },
    ref(Ref,Name).
para(linebreak([],_)) -->
    cstr("<br>"). % docEmptyType
para(nonbreakablespace([],_)) -->
    cstr(     "<nonbreakablespace/>").
para('iexcl'(_,_))  -->
    cstr(    "<iexcl/>").
para('cent'(_,_))  -->
    cstr(     "<cent/>").
para('pound'(_,_))  -->
    cstr(    "<pound/>").
para('curren'(_,_))  -->
    cstr(   "<curren/>").
para('yen'(_,_))  -->
    cstr(      "<yen/>").
para('brvbar'(_,_))  -->
    cstr(   "<brvbar/>").
para('sect'(_,_))  -->
    cstr(     "<sect/>").
para('uml'(_,_))  -->
    cstr(  "<umlaut/>").
para('copy'(_,_))  -->
    cstr(     "<copy/>").
para('ordf'(_,_))  -->
    cstr(     "<ordf/>").
para('laquo'(_,_))  -->
    cstr(    "<laquo/>").
para('not'(_,_))  -->
    cstr(      "<not/>").
para('shy'(_,_))  -->
    cstr(      "<shy/>").
para('reg'(_,_))  -->
    cstr(      "<registered/>").
para('macr'(_,_))  -->
    cstr(     "<macr/>").
para('deg'(_,_))  -->
    cstr(      "<deg/>").
para('plusmn'(_,_))  -->
    cstr(   "<plusmn/>").
para('sup2'(_,_))  -->
    cstr(     "<sup2/>").
para('sup3'(_,_))  -->
    cstr(     "<sup3/>").
para('acute'(_,_))  -->
    cstr(    "<acute/>").
para('micro'(_,_))  -->
    cstr(    "<micro/>").
para('para'(_,_))  -->
    cstr(     "<para/>").
para('middot'(_,_))  -->
    cstr(   "<middot/>").
para('cedil'(_,_))  -->
    cstr(    "<cedil/>").
para('sup1'(_,_))  -->
    cstr(     "<sup1/>").
para('ordm'(_,_))  -->
    cstr(     "<ordm/>").
para('raquo'(_,_))  -->
    cstr(    "<raquo/>").
para('frac14'(_,_))  -->
    cstr(   "<frac14/>").
para('frac12'(_,_))  -->
    cstr(   "<frac12/>").
para('frac34'(_,_))  -->
    cstr(   "<frac34/>").
para('iquest'(_,_))  -->
    cstr(   "<iquest/>").
para('Agrave'(_,_))  -->
    cstr(   "<Agrave/>").
para('Aacute'(_,_))  -->
    cstr(   "<Aacute/>").
para('Acirc'(_,_))  -->
    cstr(    "<Acirc/>").
para('Atilde'(_,_))  -->
    cstr(   "<Atilde/>").
para('Auml'(_,_))  -->
    cstr(     "<Aumlaut/>").
para('Aring'(_,_))  -->
    cstr(    "<Aring/>").
para('AElig'(_,_))  -->
    cstr(    "<AElig/>").
para('Ccedil'(_,_))  -->
    cstr(   "<Ccedil/>").
para('Egrave'(_,_))  -->
    cstr(   "<Egrave/>").
para('Eacute'(_,_))  -->
    cstr(   "<Eacute/>").
para('Ecirc'(_,_))  -->
    cstr(    "<Ecirc/>").
para('Euml'(_,_))  -->
    cstr(     "<Eumlaut/>").
para('Igrave'(_,_))  -->
    cstr(   "<Igrave/>").
para('Iacute'(_,_))  -->
    cstr(   "<Iacute/>").
para('Icirc'(_,_))  -->
    cstr(    "<Icirc/>").
para('Iuml'(_,_))  -->
    cstr(     "<Iumlaut/>").
para('ETH'(_,_))  -->
    cstr(      "<ETH/>").
para('Ntilde'(_,_))  -->
    cstr(   "<Ntilde/>").
para('Ograve'(_,_))  -->
    cstr(   "<Ograve/>").
para('Oacute'(_,_))  -->
    cstr(   "<Oacute/>").
para('Ocirc'(_,_))  -->
    cstr(    "<Ocirc/>").
para('Otilde'(_,_))  -->
    cstr(   "<Otilde/>").
para('Ouml'(_,_))  -->
    cstr(     "<Oumlaut/>").
para('times'(_,_))  -->
    cstr(    "<times/>").
para('Oslash'(_,_))  -->
    cstr(   "<Oslash/>").
para('Ugrave'(_,_))  -->
    cstr(   "<Ugrave/>").
para('Uacute'(_,_))  -->
    cstr(   "<Uacute/>").
para('Ucirc'(_,_))  -->
    cstr(    "<Ucirc/>").
para('Uuml'(_,_))  -->
    cstr(     "<Uumlaut/>").
para('Yacute'(_,_))  -->
    cstr(   "<Yacute/>").
para('THORN'(_,_))  -->
    cstr(    "<THORN/>").
para('szlig'(_,_))  -->
    cstr(    "<szlig/>").
para('agrave'(_,_))  -->
    cstr(   "<agrave/>").
para('aacute'(_,_))  -->
    cstr(   "<aacute/>").
para('acirc'(_,_))  -->
    cstr(    "<acirc/>").
para('atilde'(_,_))  -->
    cstr(   "<atilde/>").
para('auml'(_,_))  -->
    cstr(     "<aumlaut/>").
para('aring'(_,_))  -->
    cstr(    "<aring/>").
para('aelig'(_,_))  -->
    cstr(    "<aelig/>").
para('ccedil'(_,_))  -->
    cstr(   "<ccedil/>").
para('egrave'(_,_))  -->
    cstr(   "<egrave/>").
para('eacute'(_,_))  -->
    cstr(   "<eacute/>").
para('ecirc'(_,_))  -->
    cstr(    "<ecirc/>").
para('euml'(_,_))  -->
    cstr(     "<eumlaut/>").
para('igrave'(_,_))  -->
    cstr(   "<igrave/>").
para('iacute'(_,_))  -->
    cstr(   "<iacute/>").
para('icirc'(_,_))  -->
    cstr(    "<icirc/>").
para('iuml'(_,_))  -->
    cstr(     "<iumlaut/>").
para('eth'(_,_))  -->
    cstr(      "<eth/>").
para('ntilde'(_,_))  -->
    cstr(   "<ntilde/>").
para('ograve'(_,_))  -->
    cstr(   "<ograve/>").
para('oacute'(_,_))  -->
    cstr(   "<oacute/>").
para('ocirc'(_,_))  -->
    cstr(    "<ocirc/>").
para('otilde'(_,_))  -->
    cstr(   "<otilde/>").
para('ouml'(_,_))  -->
    cstr(     "<oumlaut/>").
para('divide'(_,_))  -->
    cstr(   "<divide/>").
para('oslash'(_,_))  -->
    cstr(   "<oslash/>").
para('ugrave'(_,_))  -->
    cstr(   "<ugrave/>").
para('uacute'(_,_))  -->
    cstr(   "<uacute/>").
para('ucirc'(_,_))  -->
    cstr(    "<ucirc/>").
para('uuml'(_,_))  -->
    cstr(     "<uumlaut/>").
para('yacute'(_,_))  -->
    cstr(   "<yacute/>").
para('thorn'(_,_))  -->
    cstr(    "<thorn/>").
para('yuml'(_,_))  -->
    cstr(     "<yumlaut/>").
para('fnof'(_,_))  -->
    cstr(     "<fnof/>").
para('Alpha'(_,_))  -->
    cstr(    "<Alpha/>").
para('Beta'(_,_))  -->
    cstr(     "<Beta/>").
para('Gamma'(_,_))  -->
    cstr(    "<Gamma/>").
para('Delta'(_,_))  -->
    cstr(    "<Delta/>").
para('Epsilon'(_,_))  -->
    cstr(  "<Epsilon/>").
para('Zeta'(_,_))  -->
    cstr(     "<Zeta/>").
para('Eta'(_,_))  -->
    cstr(      "<Eta/>").
para('Theta'(_,_))  -->
    cstr(    "<Theta/>").
para('Iota'(_,_))  -->
    cstr(     "<Iota/>").
para('Kappa'(_,_))  -->
    cstr(    "<Kappa/>").
para('Lambda'(_,_))  -->
    cstr(   "<Lambda/>").
para('Mu'(_,_))  -->
    cstr(       "<Mu/>").
para('Nu'(_,_))  -->
    cstr(       "<Nu/>").
para('Xi'(_,_))  -->
    cstr(       "<Xi/>").
para('Omicron'(_,_))  -->
    cstr(  "<Omicron/>").
para('Pi'(_,_))  -->
    cstr(       "<Pi/>").
para('Rho'(_,_))  -->
    cstr(      "<Rho/>").
para('Sigma'(_,_))  -->
    cstr(    "<Sigma/>").
para('Tau'(_,_))  -->
    cstr(      "<Tau/>").
para('Upsilon'(_,_))  -->
    cstr(  "<Upsilon/>").
para('Phi'(_,_))  -->
    cstr(      "<Phi/>").
para('Chi'(_,_))  -->
    cstr(      "<Chi/>").
para('Psi'(_,_))  -->
    cstr(      "<Psi/>").
para('Omega'(_,_))  -->
    cstr(    "<Omega/>").
para('alpha'(_,_))  -->
    cstr(    "<alpha/>").
para('beta'(_,_))  -->
    cstr(     "<beta/>").
para('gamma'(_,_))  -->
    cstr(    "<gamma/>").
para('delta'(_,_))  -->
    cstr(    "<delta/>").
para('epsilon'(_,_))  -->
    cstr(  "<epsilon/>").
para('zeta'(_,_))  -->
    cstr(     "<zeta/>").
para('eta'(_,_))  -->
    cstr(      "<eta/>").
para('theta'(_,_))  -->
    cstr(    "<theta/>").
para('iota'(_,_))  -->
    cstr(     "<iota/>").
para('kappa'(_,_))  -->
    cstr(    "<kappa/>").
para('lambda'(_,_))  -->
    cstr(   "<lambda/>").
para('mu'(_,_))  -->
    cstr(       "<mu/>").
para('nu'(_,_))  -->
    cstr(       "<nu/>").
para('xi'(_,_))  -->
    cstr(       "<xi/>").
para('omicron'(_,_))  -->
    cstr(  "<omicron/>").
para('pi'(_,_))  -->
    cstr(       "<pi/>").
para('rho'(_,_))  -->
    cstr(      "<rho/>").
para('sigmaf'(_,_))  -->
    cstr(   "<sigmaf/>").
para('sigma'(_,_))  -->
    cstr(    "<sigma/>").
para('tau'(_,_))  -->
    cstr(      "<tau/>").
para('upsilon'(_,_))  -->
    cstr(  "<upsilon/>").
para('phi'(_,_))  -->
    cstr(      "<phi/>").
para('chi'(_,_))  -->
    cstr(      "<chi/>").
para('psi'(_,_))  -->
    cstr(      "<psi/>").
para('omega'(_,_))  -->
    cstr(    "<omega/>").
para('thetasym'(_,_))  -->
    cstr( "<thetasym/>").
para('upsih'(_,_))  -->
    cstr(    "<upsih/>").
para('piv'(_,_))  -->
    cstr(      "<piv/>").
para('bull'(_,_))  -->
    cstr(     "<bull/>").
para('hellip'(_,_))  -->
    cstr(   "<hellip/>").
para('prime'(_,_))  -->
    cstr(    "<prime/>").
para('Prime'(_,_))  -->
    cstr(    "<Prime/>").
para('oline'(_,_))  -->
    cstr(    "<oline/>").
para('frasl'(_,_))  -->
    cstr(    "<frasl/>").
para('weierp'(_,_))  -->
    cstr(   "<weierp/>").
para('image'(_,_))  -->
    cstr(    "<imaginary/>").
para('real'(_,_))  -->
    cstr(     "<real/>").
para('trade'(_,_))  -->
    cstr(    "<trademark/>").
para('alefsym'(_,_))  -->
    cstr(  "<alefsym/>").
para('larr'(_,_))  -->
    cstr(     "<larr/>").
para('uarr'(_,_))  -->
    cstr(     "<uarr/>").
para('rarr'(_,_))  -->
    cstr(     "<rarr/>").
para('darr'(_,_))  -->
    cstr(     "<darr/>").
para('harr'(_,_))  -->
    cstr(     "<harr/>").
para('crarr'(_,_))  -->
    cstr(    "<crarr/>").
para('lArr'(_,_))  -->
    cstr(     "<lArr/>").
para('uArr'(_,_))  -->
    cstr(     "<uArr/>").
para('rArr'(_,_))  -->
    cstr(     "<rArr/>").
para('dArr'(_,_))  -->
    cstr(     "<dArr/>").
para('hArr'(_,_))  -->
    cstr(     "<hArr/>").
para('forall'(_,_))  -->
    cstr(   "<forall/>").
para('part'(_,_))  -->
    cstr(     "<part/>").
para('exist'(_,_))  -->
    cstr(    "<exist/>").
para('empty'(_,_))  -->
    cstr(    "<empty/>").
para('nabla'(_,_))  -->
    cstr(    "<nabla/>").
para('isin'(_,_))  -->
    cstr(     "<isin/>").
para('notin'(_,_))  -->
    cstr(    "<notin/>").
para('ni'(_,_))  -->
    cstr(       "<ni/>").
para('prod'(_,_))  -->
    cstr(     "<prod/>").
para('sum'(_,_))  -->
    cstr(      "<sum/>").
para('minus'(_,_))  -->
    cstr(    "<minus/>").
para('lowast'(_,_))  -->
    cstr(   "<lowast/>").
para('radic'(_,_))  -->
    cstr(    "<radic/>").
para('prop'(_,_))  -->
    cstr(     "<prop/>").
para('infin'(_,_))  -->
    cstr(    "<infin/>").
para('ang'(_,_))  -->
    cstr(      "<ang/>").
para('and'(_,_))  -->
    cstr(      "<and/>").
para('or'(_,_))  -->
    cstr(       "<or/>").
para('cap'(_,_))  -->
    cstr(      "<cap/>").
para('cup'(_,_))  -->
    cstr(      "<cup/>").
para('int'(_,_))  -->
    cstr(      "<int/>").
para('there4'(_,_))  -->
    cstr(   "<there4/>").
para('sim'(_,_))  -->
    cstr(      "<sim/>").
para('cong'(_,_))  -->
    cstr(     "<cong/>").
para('asymp'(_,_))  -->
    cstr(    "<asymp/>").
para('ne'(_,_))  -->
    cstr(       "<ne/>").
para('equiv'(_,_))  -->
    cstr(    "<equiv/>").
para('le'(_,_))  -->
    cstr(       "<le/>").
para('ge'(_,_))  -->
    cstr(       "<ge/>").
para('sub'(_,_))  -->
    cstr(      "<sub/>").
para('sup'(_,_))  -->
    cstr(      "<sup/>").
para('nsub'(_,_))  -->
    cstr(     "<nsub/>").
para('sube'(_,_))  --> 
    cstr(     "<sube/>").
para('supe'(_,_))  -->
    cstr(     "<supe/>").
para('oplus'(_,_))  -->
    cstr(    "<oplus/>").
para('otimes'(_,_))  -->
    cstr(   "<otimes/>").
para('perp'(_,_))  -->
    cstr(     "<perp/>").
para('sdot'(_,_))  -->
    cstr(     "<sdot/>").
para('lceil'(_,_))  -->
    cstr(    "<lceil/>").
para('rceil'(_,_))  -->
    cstr(    "<rceil/>").
para('lfloor'(_,_))  -->
    cstr(   "<lfloor/>").
para('rfloor'(_,_))  -->
    cstr(   "<rfloor/>").
para('lang'(_,_))  -->
    cstr(     "<lang/>").
para('rang'(_,_))  -->
    cstr(     "<rang/>").
para('loz'(_,_))  -->
    cstr(      "<loz/>").
para('spades'(_,_))  -->
    cstr(   "<spades/>").
para('clubs'(_,_))  -->
    cstr(    "<clubs/>").
para('hearts'(_,_))  -->
    cstr(   "<hearts/>").
para('diams'(_,_))  -->
    cstr(    "<diams/>").
para('quot'(_,_))  -->
    cstr("&quot;").
para('amp'(_,_))  -->
    cstr(      "&amp;").
para('lt'(_,_))  -->
    cstr(       "&lt;").
para('gt'(_,_))  -->
    cstr(       "&gt;").
para('OElig'(_,_))  -->
    cstr(    "<OElig/>").
para('oelig'(_,_))  -->
    cstr(    "<oelig/>").
para('Scaron'(_,_))  -->
    cstr(   "<Scaron/>").
para('scaron'(_,_))  -->
    cstr(   "<scaron/>").
para('Yuml'(_,_))  -->
    cstr(     "<Yumlaut/>").
para('circ'(_,_))  -->
    cstr(     "<circ/>").
para('tilde'(_,_))  -->
    cstr(    "<tilde/>").
para(ensp(_,_)) -->
    cstr("<ensp/>").
para('emsp'(_,_))  -->
    cstr("<emsp/>").
para('thinsp'(_,_))  -->
    cstr(  "<thinsp/>").
para('zwnj'(_,_))  -->
    cstr( "<zwnj/>").
para('zwj'(_,_)) -->
    cstr("<zwj/>").
para('lrm'(_,_)) -->
    cstr("<lrm/>").
para('rlm'(_,_)) -->
    cstr("<rlm/>").
para('ndash'(_,_)) -->
    cstr("<ndash/>").
para('mdash'(_,_)) -->
    cstr("<mdash/>").
para('lsquo'(_,_)) -->
    cstr("<lsquo/>").
para('rsquo'(_,_)) -->
    cstr("<rsquo/>").
para('sbquo'(_,_)) -->
    cstr("<sbquo/>").
para('ldquo'(_,_)) -->
    cstr("<ldquo/>").
para('rdquo'(_,_)) -->
    cstr("<rdquo/>").
para('bdquo'(_,_)) -->
    cstr("<bdquo/>").
para('dagger'(_,_)) -->
    cstr("<dagger/>").
para('Dagger'(_,_)) -->
    cstr("<Dagger/>").
para('permil'(_,_)) -->
    cstr("<permil/>").
para('lsaquo'(_,_)) -->
    cstr("<lsaquo/>").
para('rsaquo'(_,_)) -->
    cstr("<rsaquo/>").
para('euro'(_,_)) -->
    cstr("<euro/>").

%  // doxygen extension to the HTML4 table of HTML entities
para('tm'(_,_))  -->
    cstr(   "<tm/>").
para('apos'(_,_))  -->
    cstr("&apos;").

%  // doxygen commands represented as HTML entities
para('BSlash'(_,_)) -->
    cstr("\\").
para('BSlash'(_,_)) -->
    cstr("@").
para('Less'(_,_)) -->
    cstr("&lt;").
para('Greater'(_,_)) -->
    cstr("&lt;").
%<!-- end workaround for xsd.exe -->
unimpl(Cmd,Arg) -->
    { format(user_error,'unimplemented: ~w (called with ~w)',[Cmd,Arg]) }.


    
 
cstr(A,S0,SF) :-
    string_concat(S0,A, SF).

mcstr(A,S0,SF) :-
    string_concat([S0|A], SF).



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



ref(S,W) -->
    {string_chars(S,Cs),
     length(Pos,34),
     append(_Prefix,['_'|Pos],Cs),
     maplist(char_type_alnum, Pos),
     !,
     string_chars(P,Pos)
    },
    mcstr(["[",W,"](#",P,")"]).
ref(S,W) -->
    mcstr(["[",W,"](",S,")"]).

 defref(S,W) -->
    {string_chars(S,Cs),
     length(Pos,34),
     append(_Prefix,['_'|Pos],Cs),
     maplist(char_type_alnum, Pos)
    },
    !,
    {string_chars(BS,Pos)},
    mcstr(["\n - ()[]{#",BS,"} ",W]).
defref(S,W) -->
    mcstr(["\n - ()[]{",S,"} ",W]).
   
    

