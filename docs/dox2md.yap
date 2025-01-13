v

:- use_module(library(maplist)).
:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(xml2yap)).

%:- dynamic group/8, predicate/8, show/0.

:- set_prolog_flag(double_quotes, string).

:- multifile extrabrief/2, class/8, predicate/8 , v/2, f/3, c/2,extra/2.
:- dynamic parent/2.

in(X,[X|_]) :- !.
in(X,[_|L]) :-
    in(X,L).

main :-
    retractall(visited(_)),
    retractall(parent(_,_)),
    retractall(predicate(_)),
    abolish(class/8),
    abolish(group/8),
    abolish(predicate/8),
    unix(argv(Params)),
    process(Params).

process([Index|_]) :-
    atom_concat(_Input,'/index.xml',Index),
    xml_load(Index,[doxygenindex([_|XMLTasks])]),
    !,
    xml(XMLTasks).
process([File|_]) :-
    atom_concat(Input,'.xml',File),
    xml_load(File,XML),
    XML = [doxygen(_,XMLData)],
    XMLData = [compounddef(Atts,Children)],
    compounddef([input=Input,ofile=user_output],Atts,Children,"",FullText),
    atom_concat(Input,'.md',OFile),
    open(OFile,write,O),
    format(O,'~s',[FullText]),
    close(O).

ref(S,W) -->
    {deref(S,R )},
    mcstr(["- [",W,"](",R,")\n"]).
 defref(S,W) -->
    {deref(S,R )},
    mcstr(["- [](){",R,"}\n",W]).
   

compounddef(State, Atts, Children) -->
    {
      in(id(Id),Atts),
      in(kind(Kind),Atts),
      NState = [id=Id,kind=Kind|State],
      Children=[compoundname([],[Name])|Ch2]
    },    
    mcstr(["# ",Name,"       {",Id,"}\n"]),
    seq(NState,basecompoundref(_,_),Ch2,Basecompoundref),
    seq(NState,derivedcompoundref(_,_),Basecompoundref,Derivedcompoundref),
    % incType
    ignoreseq(NState,includes(_,_),Derivedcompoundref,Includes),
    ignoreseq(NState,includedby(_,_),Includes,Includedby),
    % graphType
    ignoreseq(NState,incdepgraph(_,_),Includedby,Incdepgraph),
    ignoreseq(NState,invincdepgraph(_,_),Incdepgraph,Invincdepgraph),
    % refType
    ignoreseq(NState,innermodule(_,_),Invincdepgraph,Innermodule),
    ignoreseq(NState,innerdir(_,_),Innermodule,Innerdir),
    ignoreseq(NState,innerfile(_,_),Innerdir,Innerfile),
    ignoreseq(NState,innerclass(_,_),Innerfile,Innerclass),
    ignoreseq(NState,innernamespace(_,_),Innerclass,Innernamespace),
    ignoreseq(NState,innerpage(_,_),Innernamespace,Innerpage),
    ignoreseq(NState,innergroup(_,_),Innerpage,Innergroup),
    ignoreseq(NState,qualifier(_,_),Innergroup,Qualifier),
    ignoreseq(NState,templateparamlist(_,_),Qualifier,Templateparamlist),
    sectiondef(Templateparamlist,Sectiondef),
    ignoreseq(NState,tableofcontents(_,_),Sectiondef,Tableofcontents),
    ignoreseq(NState,requiresclause(_,_),Tableofcontents,Requiresclause),
    ignoreseq(NState,initializer(_,_),Requiresclause,Initializer),
    briefs(Initializer,Briefdescription),
    detaileds(Briefdescription,Detaileddescription),
    ignoreseq(NState,exports(_,_),Detaileddescription,Exports),
    ignoreseq(NState,inheritancegraph(_,_),Exports,Inheritancegraph),
    ignoreseq(NState,collaborationgraph(_,_),Inheritancegraph,Collaborationgraph),
    ignoreseq(NState,programlisting(_,_),Collaborationgraph,Programlisting),
    ignoreseq(NState,location(_,_),Programlisting,Location),
    ignoreseq(NState,listofallmembers(_,_),Location,[]).

%sectiondef(A,Remainder) --> {writeln(A),fail}.
sectiondef([sectiondef(Atts,Els)|Sections],Remainder) -->
    {
      in(kind(Kind),Atts)
    },
    { top_seq_name(sectiondef,Kind,Name) },
    !,
    mcstr(["## ",Name,":\n"]),
    sectdefs(Els),
    sectiondef(Sections,Remainder).
    
sectiondef(R,R) -->[].

sectdefs([])-->[].
sectdefs([member(Atts,Children)|Defs])-->
    {      in(refid(Ref),Atts),
      in(name(_,[Name] ),Children)
    },
    !,
    ref(Ref,Name),
    sectdefs(Defs).
sectdefs([memberdef(Atts,Children)|Defs])-->
    {
      in(id(Ref),Atts),
      in(name(_,[Name] ),Children)
%,writeln(Name),(Name="arg"->start_low_level_trace;true)
    },
    !,
    defref(Ref,Name), 
/*    (
      { in(type([],Type),Children) }
      ->
      paras([" "|Type])
      ;
      true
    ),    
  */  (
      { in(definition([],Def),Children) }
      ->
      descriptions([" "|Def])
      ;
      true    
    ),    
    (
      { in(argsstring([],Args),Children) }
      ->
      descriptions(Args)
      ;
      true
    ),
    {write(',')},
    (
      { in(briefdescription([],Brief),Children) }
      ->
      descriptions(Brief)
      ;
      true
    ),
    {write(',')},
    (
      { in(inbodydescription([],InBody),Children) }
      ->
      descriptions(InBody)
      ;
      true
    ),
    {write(',')},
    (
      { in(detaileddescription([],Detailed),Children)}
      ->
      descriptions(Detailed)
      ;
      true
    ),
    {write(',')},
    !,
    cstr("\n"),
    sectdefs(Defs).

%sectiondef(A,Remainder) --> {writeln(A),fail}.
briefs([briefdescription(_Atts,Els)|Sections],Remainder) -->
    !,
    descriptions(Els),
    briefs(Sections,Remainder).
briefs(R,R) -->[].

detaileds([detaileddescription(_Atts,Els)|Sections],Remainder) -->
    !,
    cstr("\n"),
    descriptions(Els),
    detaileds(Sections,Remainder).
detaileds(R,R) -->[].

descriptions([]) --> [].
descriptions([D|Detailed]) -->
%    {writeln(D)},
    description(D),
    descriptions( Detailed).


description(S) -->
   { string(S) },
    !,
    cstr(S).
description(title([],S)) -->
   { string(S) },
    !,
    cstr(S).
description(para([],S)) -->
    descriptions(S).
description(S) -->
    para(S).

seq(State,G0,[G1|Cs],Gf) -->
    {functor(G0,N,A),
     functor(G1,N,A),
     seqhdr(N,State,Name)
	} ,
    !,
     mcstr(["## ",Name,":\n"]),
     seq1(N/A,[G1|Cs],Gf).
seq(_,_,Gs,Gs) --> [].

seqhdr(Type,State,Name) :-
    in(kind=Kind, State),
    top_seq_name(Type, Kind, Name).

top_seq_name(basecompoundref, "class", "Superclass").
top_seq_name(basecompoundref, "group", "Parent Group").
top_seq_name(derivedcompoundref, "class", "Subclasses").
top_seq_name(derivedcompoundref, "group", "Subgroup").
top_seq_name(sectiondef, "friend", "Friends").
top_seq_name(sectiondef, "protected-attrib", "Protected Attributes").
top_seq_name(sectiondef, "public-func", "Public Function").
top_seq_name(sectiondef, "public-type", "Public Type").

seq1(N/A,[G|Gs],Gsf) -->
    {
      G=.. [N,Atts,[Name]],
      in(refid(Ref),Atts)
    }, 
    !, 
    defref(Ref,Name),
    seq1(N/A,Gs,Gsf).
 seq1(_,Gs,Gs) --> [].

ignoreseq(_State,G,[G1|Gs],Gsf) -->
    {functor(G,N,A) },
    ignoreseq1(N/A,[G1|Gs],Gsf),
    !.

ignoreseq1(N/A,[G1|Gs],Gsf) -->
    {functor(G1,N,A)},
    !,
    ignoreseq1(N/A,Gs,Gsf ).
ignoreseq1(_,Gs,Gs) --> [].


para(bold([],Text)) -->
    cstr("**"), para(Text), cstr("**").
para(s([],Text)) -->
    cstr("~~"), para(Text), cstr("~~").
para(cstrike([],Text)) -->
    cstr("~~"), para(Text), cstr("~~").
para(underline([],Text)) -->
    cstr("<ins>"), para(Text), cstr("</ins>").
para(emphasis([],Text)) -->
    cstr("__"), para(Text), cstr("__").
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
   cstr      "<umlaut/>").
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
para(hruler([],Text)) -->
    unimpl(Text). % docEmptyType
para(preformatted([],Text)) -->
    unimpl(Text). % docMarkupType
para(programlisting([],Text)) -->
   unimpl(Text). % listingType
para(verbatim([],Text)) -->
   unimpl(Text). % xsd:unimpling
para(javadocliteral([],Text)) -->
   unimpl(Text). % xsd:unimpling
para(javadoccode([],Text)) -->
   unimpl(Text). % xsd:unimpling
para(indexentry([],Text)) -->
   unimpl(Text). % docIndexEntryType
para(list([],Text)) -->
   unimpl(Text). % docListType
para(itemizedlist([],Text)) -->
   unimpl(Text). % docListType
para(simplesect([],Text)) -->
   unimpl(Text). % docSimpleSectType
para(title([],Text)) -->
   unimpl(Text). % docTitleType
para(variablelist([],Text)) -->
   unimpl(Text). % docVariableListType
para(table([],Text)) -->
   unimpl(Text). % docTableType
para(heading([],Text)) -->
   unimpl(Text). % docHeadingType
para(dotfile([],Text)) -->
   unimpl(Text). % docImageFileType
para(mscfile([],Text)) -->
   unimpl(Text). % docImageFileType
para(diafile([],Text)) -->
   unimpl(Text). % docImageFileType
para(toclist([],Text)) -->
   unimpl(Text). % docTocListType
para(language([],Text)) -->
   unimpl(Text). % docLanguageType
para(parameterlist([],Text)) -->
   unimpl(Text). % docParamListType
para(xrefsect([],Text)) -->
   unimpl(Text). % docXRefSectType
para(copydoc([],Text)) -->
   unimpl(Text). % docCopyType
para(details([],Text)) -->
   unimpl(Text). % docDetailsType
para(blockquote([],Text)) -->
    cstr("~~~"),
    descriptions(Text),
    cstr("~~~"). % docBlockQuoteType
para(parblock([],Text))-->
    paras(Text). % docParBlockType         



xml(XMLTasks) :-
    foldl( run_task( ``), XMLTasks, Tasks0, []),
    sort(Tasks0,Tasks),
    !,
    maplist(fetch,Tasks),
    merge_nodes.

add_task(Kind, A,B,Name0) -->
    { string_atom(Kind,Atom),
      Linkage =.. [parent, A, B],
      AnyLinkage =.. [parent,A,_],
      Name =.. [Atom,A,Name0] },
    add_kind_of_task(Name, Linkage,B,AnyLinkage).

extra_task(Kind,Ref,Parent,Name) -->
    { add_task(Kind,Ref,Parent,Name,[], Tasks),
 Tasks = [New]
      ->
      fetch(New)
      ;
      true
    }.

/* add_kind_of_task(Name, Linkage,_) -->
    {assert(Linkage)},
    [Name].
*/
add_kind_of_task(Name,Linkage,Parent,AnyLinkage) -->
    {
      Parent\= ``,
      call(AnyLinkage),
      arg(2,AnyLinkage,``),
      !,
      retractall(AnyLinkage),
      %  Linkage=parent(A,B),atom_atring(AB,B), NLinkage = parent(A,AB),
      assert(Linkage)
    }, [Name].
add_kind_of_task(_,_,_,AnyLinkage) --> {AnyLinkage}, !.
add_kind_of_task(Name,Linkage,_,_AnyLinkage) -->
    {
%      Linkage=parent(A,B),atom_atring(AB,B), NLinkage = parent(A,AB),
assert(Linkage)
    }, [Name].

xml2tasks(Tasks, NewTasks) :-
    foldl( run_task(``), Tasks, NewTasks, []).


deref(Link, Link) :-
    \+ string(Link),
    !,
    fail.
deref(Link, Link) :-
    sub_string(Link,_,_,0,`.md`),
    !.
deref(Link, NewLink) :-
    string_concat(Link,`.md`,NewLink).

%run_task(_,compound(Compound)) --> {writeln(Compound),fail}.
%    members(Memberxss, Ref),

run_task(P,compound([[refid(Ref),kind(`page`)|_],name([[],Name])|_Members])) -->
    !,
    { Kind=`group` },
    add_task(Kind,Ref,P,Name),
    sub_tasks(Kind,Ref).
run_task(P,compound([[refid(Ref),kind(Kind)|_],name([[],Name])|_Members])) -->
    { sub_string(Ref,0,_,_,`class`) },
    !,
    add_task(Kind,Ref,P,Name),
    sub_tasks(Kind,Ref).
run_task(P,compound([[refid(Ref),kind(Kind)|_],name([[],Name])|_Members])) -->
    { sub_string(Ref,0,_,_,`concept`) },
    !,
    add_task(Kind,Ref,P,Name),
    sub_tasks(Kind,Ref).
run_task(P,compound([[refid(Ref),kind(Kind)|_],name([[],Name])|_Members])) -->
    !,
    add_task(Kind,Ref,P,Name).
run_task(_,compound(_)) -->
    !.
run_task(P,compounddef([[id(Ref),`page`|More]|Members])) -->
    !,
    run_task(P,compound([[refid(Ref),`group`|More]|Members])).
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
    {  sub_string(Ref,0,_,_,`class`) },
    !,
    add_task(`class`,Ref,Parent,Name).
inner_tasks(Parent,innerclass([[refid(Ref)|_],Name|_])) -->
    !,
    add_task(`class`,Ref,Parent,Name).
inner_tasks(Parent,innergroup([[refid(Ref)|_],Name|_])) -->
    !,
    add_task(`group`,Ref,Parent,Name).
inner_tasks(Parent,innerpage([[refid(Ref)|_],Name|_])) -->
    !,
    add_task(`group`,Ref,Parent,Name).
inner_tasks(_Parent,member([[refid(_Ref)|_],_Name|_])) -->
    !.
inner_tasks(_,_) --> [].

name2pi(Name,PI) :-
    sub_string(Name,R,_,L,`::`),
    sub_string(Name,0,R,_,Module),
    %    mkunsafe(Module0,Module),
    sub_string(Name,_,L,0,NA),
    tofunctor(NA,NameF,A),
    %    mkunsafe(NB,NameF),
    string_concat([Module,`:`,NameF,`/`,A],PI).
name2pi(Name,PI) :-
    tofunctor(Name,N,A),
    %    mkunsafe(N0,N),
    string_concat([N,`/`,A],PI).

tofunctor(S,N,FA) :-
    string_length(S,L),
    string_char(L,S,Pt),
    char_type(Pt, digit),
    L1 is L-1,
    to_big_functor(L1, S, [Pt], N, FA).

    to_big_functor(L, S, As, N, FA)  :-
    string_char(L,S,Pt),
    char_type(Pt, digit),
    L1 is L-1,
    !,
    to_big_functor(L1, S, [Pt|As], N, FA).
        to_big_functor(_L, S, As, N, FA)  :-
    length(As,LA),
    string_chars(FA,As),

    sub_string(S,4,_,LA,N).




true_name([],[]).
true_name(['_','_',C],['/',C]) :-
    !.
true_name(['_',e,q|L],['='|NL]) :-
    !,
    true_name(L,NL).
true_name(['_',l,t|L],['<'|NL]) :-
    !,
    true_name(L,NL).
true_name(['_',g,t|L],['>'|NL]) :-
    !,
    true_name(L,NL).
true_name(['_','_'|L],['_'|NL]) :-
    !,
    true_name(L,NL).
true_name(['_',c,t|L],['!'|NL]) :-
    !,
    true_name(L,NL).
true_name(['_',m,n|L],['-'|NL]) :-
    !,
    true_name(L,NL).
true_name(['_',p,l|L],['+'|NL]) :-
    !,
    true_name(L,NL).
true_name(['_',s,t|L],['*'|NL]) :-
    !,
    true_name(L,NL).
true_name(['_',s,l|L],['/'|NL]) :-
    !,
    true_name(L,NL).
true_name(['_',d,l|L],['$'|NL]) :-
    !,
    true_name(L,NL).
true_name([C|L],[C|NL]) :-
    true_name(L,NL).

%members([M|_], _FromType, FromRef) -->
%    {writeln(M),fail}.

members([],_) --> [].
members([member([[refid(Ref),kind(`page`)],name([[],Name])|InnerMembers])|Members], FromRef) -->
    add_task(`group`,Ref,FromRef,Name),
    !,
    members(InnerMembers,Ref),
    members(Members,FromRef).
members([member([[refid(Ref),kind(Kind)],name([[],Name])|InnerMembers])|Members], FromRef) -->
    add_task(Kind,Ref,FromRef,Name),
    !,
    members(InnerMembers,Ref),
    members(Members,FromRef).
members([_|Members],FromRef)-->		 
    members(Members,FromRef).
		 
    

:- dynamic visited/1.
fetch(T) :-
    T=..[_,K,_],
    (
      visited(K) -> !;
      assert(visited(K)),
      fetch_(T)
    ).

fetch_(class(Ref,ModName)) :-
    sub_string(ModName,ModLen,3,NameLen,`::P`),
    L2 is ModLen+3+NameLen,
    string_char(L2,ModName,DC),
    char_type(DC,digit),
    !,
    M3 is ModLen+3,
    sub_string(ModName,M3,_,1,TNs),
    sub_string(ModName,0,ModLen,_,Module),
    string_chars(DS,[DC]),
    string_concat([Module,`:`,TNs,`/`,DS],TrueName),
    functor(Descriptor,predicate,8),
    assert(predicate(Ref)),
    fill(Descriptor,Ref,TrueName).
fetch_(class(Ref,Name)) :-
    string_char(1,Name,'P'),
    sub_string(Name,1,L,1,TNs),
    L2 is L+2,
    string_char(L2,Name,DC),
    char_type(DC,digit),
    !,
    string_concat([TNs,`/`,DC],TrueName),
    functor(Descriptor,predicate,8),
    assert(predicate(Ref)),
    fill(Descriptor,Ref,TrueName).
fetch_(class(Ref,Name)) :-
    !,
    assert(class(Ref)),
    functor(Descriptor,class,8),
    fill(Descriptor,Ref,Name).
fetch_(group(Ref,Name)) :-
    !,
    functor(Descriptor,group,8),
    assert(group(Ref)),
    writeln(Name),
    fill(Descriptor,Ref,Name).
fetch_(concept(Ref,Name)) :-
    !,
    functor(Descriptor,predicate,8),
    writeln(predicate:Ref),
    fill(Descriptor,Ref,Name).
/*fetch_(enum(Ref,Name)) :-
    !,
    functor(Descriptor,enum,8),
     fill(Descriptor,Ref,Name ).
fetch_(struct(Ref,Name)) :-
    !,
    functor(Descriptor,struct,8),
    fill(Descriptor,Ref,Name).
fetch_(union(Ref,Name)) :-
    !,
    functor(Descriptor,union,8),
    fill(Descriptor,Ref,Name).
*/
fetch_(_).

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
    assert( Descriptor).
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
    {
writeln(failed:C)
    }.

% par(U0,_Info,C) -->
%     {writeln(C),
%     fail}.
par(_,_,[]) --> !.
par(_,_,sectiondef([[kind(`private-attrib`)]|_])) -->
    !.
par(_,_,sectiondef([[kind(`private-static-attrib`)]|_])) -->
    !.
par(_,_,sectiondef([[kind(`private-static-func`)]|_])) -->
    !.
par(_,_,sectiondef([[kind(`protected-attrib`)]|_])) -->
    !.
par(_,_,sectiondef([[kind(`protected-func`)]|_])) -->
    !.
par(_,_,sectiondef([[kind(`protected-static-func`)]|_])) -->
    !.
par(_,_,sectiondef([[kind(`protected-type`)]|_])) -->
    !.
par(_,_,sectiondef([[kind(`private-func`)]|_])) -->
    !.
par(_,_,sectiondef([[kind(`user-defined`)]|_])) -->
    !.
par(_,_,sectiondef([[kind(`public-type`)]|_])) -->
    !.
par(U0,Info,sectiondef([[kind(`friend`)|_]|Paras])) -->
    !,
    {
      arg(1,Info,Id),
      format(string(S), `\n#### Friends:\n`, []),
      foldl(friend_member(U0,Info),Paras, S, Desc),
      assert(v(Id,Desc))
    }.
par(U0,Info,sectiondef([[kind(Funcs)|_Opts]|Paras])) -->
    {fmember(Funcs,_FType)},
    {
      arg(1,Info,Id),
      foldl(function_member(U0,Info),Paras, ``, Desc),
      !,
      assert(f(Funcs,Id,Desc))
    }.
par(_U0,_Info,sectiondef([[kind(_)|_Opts]|_Paras])) -->
    !.
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
    !,
    mcstr([`## `,Title,`               {#`,Id,`};`]),
    add_nl(U0),
    foldl(par(U0,Info),Paras),
    add_nl(0).
par(U0,Info,sect1([[id(Id)]|Paras])) -->
    mcstr([`## `,Id,`               {#`,Id,`};`]),
    add_nl(U0),
    foldl(par(U0,Info),Paras),
    add_nl(0).
par(U0,Info,sect2([[id(Id)],title([[],Title])|Paras])) -->
    !,
    mcstr([`### `,Title,`               {#`,Id,`};`]),
    add_nl(U0),
    foldl(par(U0,Info),Paras),
    add_nl(0).
par(U0,Info,sect2([[id(Id)],title([[],Title])|Paras])) -->
    !,
    mcstr([`### `,Title,`               {#`,Id,`};`]),
    add_nl(U0),
    foldl(par(U0,Info),Paras),
    add_nl(0).
par(U0,Info,sect3([[id(Id)]|Paras])) -->
    !,
    mcstr([`###               {#`,Id,`};`]),
    add_nl(U0),
    foldl(par(U0,Info),Paras),
    add_nl(0).
par(_U0,Info,innergroup([[refid(Ref)],Name])) -->
    { arg(1, Info, Parent) },
    !,
    extra_task(`group`,Ref,Parent,Name).
par(_U0,Info,innerpage([[refid(Ref)],Name])) -->
    { arg(1, Info, Parent) },
    !,
    extra_task(`group`,Ref,Parent,Name).
par(_U0,_Info,ref([[refid(`structF`),kindref(`compound`)],false ])) -->
    !.
par(_U0,_Info,basecompoundref([[_|_],_])) -->
    !.
par(_U0,Info,innerclass([[refid(Ref),_],Name])) -->
    !,
    {  arg(1, Info, Parent)},
    extra_task(`class`,Ref,Parent,Name).
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
      foldl(par(U0,[]),Paras,``,Desc),
      (Desc == ``  ->
       true
       ;
       arg(1,Info,Id),
       assert(extra(Id,Desc))
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
par(_,_GT,includes(_)) --> !.
par(U0,Info,memberdef([_|Seq]))-->
    !,
    { U is U0+4 },
    cstr(`* ` ),
    foldl(par(U,Info),Seq),
    add_nl(U).
par(U0,_Info,member([[refid(ID),kind(Kind)],name([[],Name])]))-->
    !,
    mcstr([`* `,Kind,`[`,ID,`](`,Name,`)`] ),
    add_nl(U0).
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
jpar(U,Info,verbatim([[]|Text])) -->
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
    cstr(Seq),cstr(`.`),
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
par(_,_GT,listofallmembers([[]|_L])) -->
    !.
par(_,_GT,templateparamlist([[]|_L])) -->
    !.
par(_U0, _, parameterlist(_)) -->
    !.
par(U0, Item, variablelist([[],varlistentry([[]|Text])|Items])) -->
    !,
    foldl(par(U0,Item),Text),
    foldl(item(U0,Item),Items).
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
par(U0,Item, term([[]|Par])) -->
    !,
    foldl(par(U0,Item),Par).
par(_U,_Info,ref([[refid(`class`),kindref(`compound`)],true])) -->
    !.
par(_U,_Info, ref([[refid(R)|_],Name])) -->
    { string(Name),
      deref(R,DR) },
    !,
    mcstr([`[`,Name,`](`,DR,`)`]).
par(_U,_Info, ref([[refid(R)|_],Name])) -->
    { string(Name),
      deref(R,DR) },
    !,
    mcstr([`[`,Name,`](`,DR,`)`]).
par(_U,_Info, qualifiedname([[],Name])) -->
    { string(Name),
      sub_string(Name,_,_,R,`::P`),
      deref(R,DR) },
    !,
    mcstr([DR]).
par(_U,_Info, qualifiedname([[],Name])) -->
    !,
    cstr(Name).
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

ventry(U0,Item,varlistentry([[]|Seq]),S0,SF) :-
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


ent(Id,class,Name, File,Line,Column,Brief,Text,Title) :-
    class(Id,Name, File,Line,Column,Brief,Text,Title).
ent(Id,group,Name, File,Line,Column,Brief,Text,Title) :-
    group(Id,Name,File,Line,Column,Brief,Text,Title).
ent(Id,predicate,Name, File,Line,Column,Brief,Text,Title) :-
    predicate(Id,Name,File,Line,Column,Brief,Text,Title).
  
merge_nodes :-
    unix(argv([_Input,Output])),
    ent(Id,Type,Name, File,Line,Column,Brief,Text,Title),
    atomic_concat([Output,'/',Id,'.md'],F),
    %    atomic_concat([Output,'/',Id],F),
    wirteln(Id),
    setup_call_cleanup(
    open(F,write,S,[alias(Type)]),
once(one(Type,S,Id,Name, File,Line,Column,Brief,Text,Title)),
    close(S)
    ),
    fail.
 merge_nodes.

one(class,S,Id,_Name, File,Line,Column,Brief,Text,Title) :-
    format(S,'# ~s\n\n~s\n',[Title,Brief]),
    forall(extrabrief(Id,Extra),format(S,'~s\n',[Extra])),
    %forall(extra(Id,Extra) ,  format(S,'~s',[Extra])),
    process_class(S,Id,File,Line,Column,Text).
one(predicate,S,Id, _Name, File,Line,Column,Brief,Text,Title) :-
    format(S,'# ~s\n\n~s\n',[Title,Brief]),
    forall(extrabrief(Id,Extra),format(S,'~s\n',[Extra])),
    % forall(extra(Id,Extra) ,  format(S,'~s',[Extra])),
    process_predicate(S,Id,File,Line,Column,Text).
one(group, S,Id, _Name, File,Line,Column,Brief,Text,Title) :-
    format(S,'# ~s\n\n~s\n',[Title,Brief]),
    forall(extrabrief(Id,Extra),format(S,'~s\n',[Extra])),
    %  forall(extra(Id,Extra) ,  format(S,'~s',[Extra])),
    subgroups(S,Id),
    process_group(S,Id,File,Line,Column,Text).

sub_group(I,N) :-
    parent(I,N),
    group(I).
sub_class(I,N) :-
    parent(I,N),
    class(I).
sub_predicate(I,N) :-
    parent(I,N),
    predicate(I).

subgroups(S,Id) :-
    sub_group(_,Id),
    !, 
    format(S,'### SubGroups\n\n' ,[]),
    forall(sub_group(Ref,Id),(addsubg(S,Ref))).
subgroups(_S,_Id).

process_group(S,Id,_File,_Line,_Column,_Text) :-
    once(sub_predicate(_Pred,Id)),
    
    format(S,'## Predicates\n\n', []), 
    format(S,'|Predicate~t|~20|Description~t|~40|\n', []), 
    format(S,'|:---|:---~|\n', []), 
    forall(sub_predicate(Ref,Id),(addsubp(S,Ref))),
    format(S,'\n\n',[]),
    fail.
    %format(S,'~s\n',[Brief]),
process_group(S,Id,File,Line,Column,Text) :-
    %    forall(predicate(Ref,Id),(output_predicate(S,Ref))),
    load_all_text(Id,Text,AllText),
    format(S,'~s',[AllText]),
    nl(S),
    footer(S,File,Line,Column),
    fail.
process_group(S,Id,_File,_Line,_Column,_Text) :-
    \+ sub_predicate(_Pred,Id),
    once(sub_class(_,Id)),
    format(S,'## Classes\n\n', []), 
    format(S,'|Class~t|~20|Description~t|~40|\n', []), 
    format(S,'|:---|:---~|\n', []), 
    forall(sub_class(Ref,Id),(addsubc(S,Ref))),
    format(S,'\n\n',[]),
    fail.
    %format(S,'~s\n',[Brief]),
process_group(S,Id,_File,_Line,_Column,_Text) :-

    \+ sub_predicate(_Pred,Id),
    once(sub_class(_,Id)),
    format(S,'## Classes\n\n', []), 
    format(S,'|Class~t|~20|Description~t|~40|\n', []), 
    format(S,'|:---|:---~|\n', []), 
    forall(sub_class(Ref,Id),(addsubc(S,Ref))),
    format(S,'\n\n',[]),
    fail.
    %format(S,'~s\n',[Brief]),
process_group(S,Id,_File,_Line,_Column,_Text) :-
    forall(
(fmember(Type,TName),once(f(Type,Id,_))),


    (format(S,TName,[]),
     forall(f(Type,Id,Text),format(S,'~s',[Text])))).
    
    
process_predicate(S,Id,File,Line,Column,Text) :-
    %format(S,'~s\n',[Brief]),
    load_all_text(Id,Text,AllText),
    format(S,'~s',[AllText]),
    forall(sub_predicate(Ref,Id),(output_predicate(S,Ref))),
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
    sort(NTs,RTs),
    string_concat(RTs,AllText).

collect_txt(Id,_,Text) :-
    extra(Id,Text), Text \= ``.
collect_txt(Id,_,Text) :-
    v(Id,Text), Text \= ``.
collect_txt(Id,_,Text) :-
    f(_,Id,Text), Text \= ``.

load_brief(GId,Text,AllText) :-
    findall(T,(T=Text;extrabrief(GId,T)),Ts),
    string_concat(Ts,S1),
    string_chars(S1,Cs1),
    take_blanks(Cs1,Cs2),
    string_chars(S2,Cs2),
    (
      sub_string(S2,Prefix,1,_,`\n`)
      ->
      sub_string(S2, 0, Prefix,_,AllText)
      ;
      S2 = AllText
    ).

take_blanks([C|Cs],NCs) :-
    char_type_space(C),
    !,
    take_blanks(Cs,NCs).
take_blanks(Cs,Cs).


drop_dups([],[]).
drop_dups([X,X|L],NL) :-
    drop_dups([X|L],NL).
drop_dups([X|L],[X|NL]) :-
    drop_dups(L,NL).

preds(Id, S) :-
    (
      sub_predicate(_,Id)
      ->
      format(S,'## List of Predicates\n',[])
      ;
      true
    ).

addsubg(S,Id) :-
    group(Id,_Name,_File,_Line,_Column,Brief,_Text, Title),
    load_brief(Id,Brief,Brieffer),
    deref(Id,Link),
    format(S,'###### [**~s**](~s).          ~s\n',[Title,Link,Brieffer]).


addsubp(S,Id) :-
    predicate(Id,_Name,_File,_Line,_Column,Brief,_Text,Title),
    deref(Id,Link),
    load_brief(Id,Brief,Brieffer),
    format(S,'|**[~s](~s)**      |    **~s**|~n',[Title,Link,Brieffer]).

addsubc(S,Id) :-
    class(Id,_Name,_File,_Line,_Column,Brief,_Text,Title),
    load_brief(Id,Brief,Brieffer),
    deref(Id,Link),
    format(S,'|**[~s](~s)**      |    ~s|\n',[Title,Link,Brieffer]).

output_predicate(S,Id) :-
    predicate(Id,Name,_F,_L,_C,Brief,Text,_),
    load_briefs(Id,Brief,Brieffer),
    format(S,'### ~s          {#~s}\n~s\n',[Name,Link,Brieffer]),	
    format(S,'\n~s\n',[Text]),
    deref(Id,Link),
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


add_comments(U,Text) -->
    {
      member(briefdescription([[]|Brief]),Text),
      member(inbodydescription([[]|InBody]),Text),
      member(detaileddescription([[]|Detailed]),Text)
    },
    rpar(U,Brief),
    rpar(U,InBody),
    rpar(U,Detailed).

 

fmember(`define`,`### C-Preprocessor define::\n`).
fmember(`enum`,`### enum:\n`).
fmember(`friend`,`### friend:\n`).
fmember(`func`,`### function:\n`).
fmember(`package-attrib`,`### private attr:\n`).
fmember(`private-attrib`,`### private attr:\n`).
fmember(`private-static-func`,`### private method:\n`).
fmember(`private-func`,`### private method:\n`).
fmember(`private-type`,`### private type:\n`).
fmember(`public-attrib`,`### public attr:\n`).
fmember(`public-func`,`### public method:\n`).
fmember(`public-static-func`,`### public method:\n`).
fmember(`public-type`,`### public type:\n`).
fmember(`struct`,`### struct:\n`).
fmember(`var`,`### variable:\n`).
fmember(`variable`,`### variable:\n`).
fmember(`typedef`,`### type:\n`).

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
      format(string(St),`##### ~s~s  {#~s}\n`,[Def,As,Id])
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


mkunsafe(LF,UF):-
    string_chars(LF,U),
    foldl(char_to_safe,U,Unsafe,[]),
    string_chars(UF,Unsafe).

cunsafe(C,LF,L0) :-
    char_to_safe(C,LF,L0),
    !.
csafe(C,[C|L],L).

char_to_safe('=',['_',e,q|L],L).
char_to_safe('<',['_',l,t|L],L).
char_to_safe('>',['_',g,t|L],L).
char_to_safe('_',['_','_'|L],L).
char_to_safe('!',['_',c,t|L],L).
char_to_safe('-',['_',m,n|L],L).
char_to_safe('+',['_',p,l|L],L).
char_to_safe('*',['_',s,t|L],L).
char_to_safe('/',['_',s,l|L],L).
char_to_safe('$',['_',d,l|L],L).

