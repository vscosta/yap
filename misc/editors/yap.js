
// CodeMirror, copyright (c) by Marijn Haverbeke and others
// Distributed under an MIT license: http://codemirror.net/LICENSE

(function(mod) {
    if (typeof exports == "object" && typeof module == "object") // CommonJS
	mod(require("codemirror/lib/codemirror"));
    else if (typeof define == "function" && define.amd) // AMD
	define(["codemirror/lib/codemirror"], mod);
    else // Plain browser env
	mod(CodeMirror);
})(function(CodeMirror) {
    "use strict";

    CodeMirror.modeInfo = [
	{
	    name : "Prolog",
	    mime : "text/x-prolog",
	    mode : "prolog",
	    ext : [ "pl", "yap", "pro", "P", "prolog" ]
	},
	{name: "APL", mime: "text/apl", mode: "apl", ext: ["dyalog", "apl"]},
	{name: "PGP", mimes: ["application/pgp", "application/pgp-keys", "application/pgp-signature"], mode: "asciiarmor", ext: ["pgp"]},
	{name: "ASN.1", mime: "text/x-ttcn-asn", mode: "asn.1", ext: ["asn", "asn1"]},
	{name: "Asterisk", mime: "text/x-asterisk", mode: "asterisk", file: /^extensions\.conf$/i},
	{name: "Brainfuck", mime: "text/x-brainfuck", mode: "brainfuck", ext: ["b", "bf"]},
	{name: "C", mime: "text/x-csrc", mode: "clike", ext: ["c", "h"]},
	{name: "C++", mime: "text/x-c++src", mode: "clike", ext: ["cpp", "c++", "cc", "cxx", "hpp", "h++", "hh", "hxx"], alias: ["cpp"]},
	{name: "Cobol", mime: "text/x-cobol", mode: "cobol", ext: ["cob", "cpy"]},
	{name: "C#", mime: "text/x-csharp", mode: "clike", ext: ["cs"], alias: ["csharp"]},
	{name: "Clojure", mime: "text/x-clojure", mode: "clojure", ext: ["clj", "cljc", "cljx"]},
	{name: "ClojureScript", mime: "text/x-clojurescript", mode: "clojure", ext: ["cljs"]},
	{name: "Closure Stylesheets (GSS)", mime: "text/x-gss", mode: "css", ext: ["gss"]},
	{name: "CMake", mime: "text/x-cmake", mode: "cmake", ext: ["cmake", "cmake.in"], file: /^CMakeLists.txt$/},
	{name: "CoffeeScript", mime: "text/x-coffeescript", mode: "coffeescript", ext: ["coffee"], alias: ["coffee", "coffee-script"]},
	{name: "Common Lisp", mime: "text/x-common-lisp", mode: "commonlisp", ext: ["cl", "lisp", "el"], alias: ["lisp"]},
	{name: "Cypher", mime: "application/x-cypher-query", mode: "cypher", ext: ["cyp", "cypher"]},
	{name: "Cython", mime: "text/x-cython", mode: "python", ext: ["pyx", "pxd", "pxi"]},
	{name: "Crystal", mime: "text/x-crystal", mode: "crystal", ext: ["cr"]},
	{name: "CSS", mime: "text/css", mode: "css", ext: ["css"]},
	{name: "CQL", mime: "text/x-cassandra", mode: "sql", ext: ["cql"]},
	{name: "D", mime: "text/x-d", mode: "d", ext: ["d"]},
	{name: "Dart", mimes: ["application/dart", "text/x-dart"], mode: "dart", ext: ["dart"]},
	{name: "diff", mime: "text/x-diff", mode: "diff", ext: ["diff", "patch"]},
	{name: "Django", mime: "text/x-django", mode: "django"},
	{name: "Dockerfile", mime: "text/x-dockerfile", mode: "dockerfile", file: /^Dockerfile$/},
	{name: "DTD", mime: "application/xml-dtd", mode: "dtd", ext: ["dtd"]},
	{name: "Dylan", mime: "text/x-dylan", mode: "dylan", ext: ["dylan", "dyl", "intr"]},
	{name: "EBNF", mime: "text/x-ebnf", mode: "ebnf"},
	{name: "ECL", mime: "text/x-ecl", mode: "ecl", ext: ["ecl"]},
	{name: "edn", mime: "application/edn", mode: "clojure", ext: ["edn"]},
	{name: "Eiffel", mime: "text/x-eiffel", mode: "eiffel", ext: ["e"]},
	{name: "Elm", mime: "text/x-elm", mode: "elm", ext: ["elm"]},
	{name: "Embedded Javascript", mime: "application/x-ejs", mode: "htmlembedded", ext: ["ejs"]},
	{name: "Embedded Ruby", mime: "application/x-erb", mode: "htmlembedded", ext: ["erb"]},
	{name: "Erlang", mime: "text/x-erlang", mode: "erlang", ext: ["erl"]},
	{name: "Factor", mime: "text/x-factor", mode: "factor", ext: ["factor"]},
	{name: "FCL", mime: "text/x-fcl", mode: "fcl"},
	{name: "Forth", mime: "text/x-forth", mode: "forth", ext: ["forth", "fth", "4th"]},
	{name: "Fortran", mime: "text/x-fortran", mode: "fortran", ext: ["f", "for", "f77", "f90"]},
	{name: "F#", mime: "text/x-fsharp", mode: "mllike", ext: ["fs"], alias: ["fsharp"]},
	{name: "Gas", mime: "text/x-gas", mode: "gas", ext: ["s"]},
	{name: "Gherkin", mime: "text/x-feature", mode: "gherkin", ext: ["feature"]},
	{name: "GitHub Flavored Markdown", mime: "text/x-gfm", mode: "gfm", file: /^(readme|contributing|history).md$/i},
	{name: "Go", mime: "text/x-go", mode: "go", ext: ["go"]},
	{name: "Groovy", mime: "text/x-groovy", mode: "groovy", ext: ["groovy", "gradle"], file: /^Jenkinsfile$/},
	{name: "HAML", mime: "text/x-haml", mode: "haml", ext: ["haml"]},
	{name: "Haskell", mime: "text/x-haskell", mode: "haskell", ext: ["hs"]},
	{name: "Haskell (Literate)", mime: "text/x-literate-haskell", mode: "haskell-literate", ext: ["lhs"]},
	{name: "Haxe", mime: "text/x-haxe", mode: "haxe", ext: ["hx"]},
	{name: "HXML", mime: "text/x-hxml", mode: "haxe", ext: ["hxml"]},
	{name: "ASP.NET", mime: "application/x-aspx", mode: "htmlembedded", ext: ["aspx"], alias: ["asp", "aspx"]},
	{name: "HTML", mime: "text/html", mode: "htmlmixed", ext: ["html", "htm"], alias: ["xhtml"]},
	{name: "HTTP", mime: "message/http", mode: "http"},
	{name: "IDL", mime: "text/x-idl", mode: "idl", ext: ["pro"]},
	{name: "Pug", mime: "text/x-pug", mode: "pug", ext: ["jade", "pug"], alias: ["jade"]},
	{name: "Java", mime: "text/x-java", mode: "clike", ext: ["java"]},
	{name: "Java Server Pages", mime: "application/x-jsp", mode: "htmlembedded", ext: ["jsp"], alias: ["jsp"]},
	{name: "JavaScript", mimes: ["text/javascript", "text/ecmascript", "application/javascript", "application/x-javascript", "application/ecmascript"],
	 mode: "javascript", ext: ["js"], alias: ["ecmascript", "js", "node"]},
	{name: "JSON", mimes: ["application/json", "application/x-json"], mode: "javascript", ext: ["json", "map"], alias: ["json5"]},
	{name: "JSON-LD", mime: "application/ld+json", mode: "javascript", ext: ["jsonld"], alias: ["jsonld"]},
	{name: "JSX", mime: "text/jsx", mode: "jsx", ext: ["jsx"]},
	{name: "Jinja2", mime: "null", mode: "jinja2"},
	{name: "Julia", mime: "text/x-julia", mode: "julia", ext: ["jl"]},
	{name: "Kotlin", mime: "text/x-kotlin", mode: "clike", ext: ["kt"]},
	{name: "LESS", mime: "text/x-less", mode: "css", ext: ["less"]},
	{name: "LiveScript", mime: "text/x-livescript", mode: "livescript", ext: ["ls"], alias: ["ls"]},
	{name: "Lua", mime: "text/x-lua", mode: "lua", ext: ["lua"]},
	{name: "Markdown", mime: "text/x-markdown", mode: "markdown", ext: ["markdown", "md", "mkd"]},
	{name: "mIRC", mime: "text/mirc", mode: "mirc"},
	{name: "MariaDB SQL", mime: "text/x-mariadb", mode: "sql"},
	{name: "Mathematica", mime: "text/x-mathematica", mode: "mathematica", ext: ["m", "nb"]},
	{name: "Modelica", mime: "text/x-modelica", mode: "modelica", ext: ["mo"]},
	{name: "MUMPS", mime: "text/x-mumps", mode: "mumps", ext: ["mps"]},
	{name: "MS SQL", mime: "text/x-mssql", mode: "sql"},
	{name: "mbox", mime: "application/mbox", mode: "mbox", ext: ["mbox"]},
	{name: "MySQL", mime: "text/x-mysql", mode: "sql"},
	{name: "Nginx", mime: "text/x-nginx-conf", mode: "nginx", file: /nginx.*\.conf$/i},
	{name: "NSIS", mime: "text/x-nsis", mode: "nsis", ext: ["nsh", "nsi"]},
	{name: "NTriples", mime: "text/n-triples", mode: "ntriples", ext: ["nt"]},
	{name: "Objective C", mime: "text/x-objectivec", mode: "clike", ext: ["m", "mm"], alias: ["objective-c", "objc"]},
	{name: "OCaml", mime: "text/x-ocaml", mode: "mllike", ext: ["ml", "mli", "mll", "mly"]},
	{name: "Octave", mime: "text/x-octave", mode: "octave", ext: ["m"]},
	{name: "Oz", mime: "text/x-oz", mode: "oz", ext: ["oz"]},
	{name: "Pascal", mime: "text/x-pascal", mode: "pascal", ext: ["p", "pas"]},
	{name: "PEG.js", mime: "null", mode: "pegjs", ext: ["jsonld"]},
	{name: "Perl", mime: "text/x-perl", mode: "perl", ext: ["pl", "pm"]},
	{name: "PHP", mime: "application/x-httpd-php", mode: "php", ext: ["php", "php3", "php4", "php5", "phtml"]},
	{name: "Pig", mime: "text/x-pig", mode: "pig", ext: ["pig"]},
	{name: "Plain Text", mime: "text/plain", mode: "null", ext: ["txt", "text", "conf", "def", "list", "log"]},
	{name: "PLSQL", mime: "text/x-plsql", mode: "sql", ext: ["pls"]},
	{name: "PowerShell", mime: "application/x-powershell", mode: "powershell", ext: ["ps1", "psd1", "psm1"]},
	{name: "Properties files", mime: "text/x-properties", mode: "properties", ext: ["properties", "ini", "in"], alias: ["ini", "properties"]},
	{name: "ProtoBuf", mime: "text/x-protobuf", mode: "protobuf", ext: ["proto"]},
	{name: "Python", mime: "text/x-python", mode: "python", ext: ["BUILD", "bzl", "py", "pyw"], file: /^(BUCK|BUILD)$/},
	{name: "Puppet", mime: "text/x-puppet", mode: "puppet", ext: ["pp"]},
	{name: "Q", mime: "text/x-q", mode: "q", ext: ["q"]},
	{name: "R", mime: "text/x-rsrc", mode: "r", ext: ["r", "R"], alias: ["rscript"]},
	{name: "reStructuredText", mime: "text/x-rst", mode: "rst", ext: ["rst"], alias: ["rst"]},
	{name: "RPM Changes", mime: "text/x-rpm-changes", mode: "rpm"},
	{name: "RPM Spec", mime: "text/x-rpm-spec", mode: "rpm", ext: ["spec"]},
	{name: "Ruby", mime: "text/x-ruby", mode: "ruby", ext: ["rb"], alias: ["jruby", "macruby", "rake", "rb", "rbx"]},
	{name: "Rust", mime: "text/x-rustsrc", mode: "rust", ext: ["rs"]},
	{name: "SAS", mime: "text/x-sas", mode: "sas", ext: ["sas"]},
	{name: "Sass", mime: "text/x-sass", mode: "sass", ext: ["sass"]},
	{name: "Scala", mime: "text/x-scala", mode: "clike", ext: ["scala"]},
	{name: "Scheme", mime: "text/x-scheme", mode: "scheme", ext: ["scm", "ss"]},
	{name: "SCSS", mime: "text/x-scss", mode: "css", ext: ["scss"]},
	{name: "Shell", mime: "text/x-sh", mode: "shell", ext: ["sh", "ksh", "bash"], alias: ["bash", "sh", "zsh"], file: /^PKGBUILD$/},
	{name: "Sieve", mime: "application/sieve", mode: "sieve", ext: ["siv", "sieve"]},
	{name: "Slim", mimes: ["text/x-slim", "application/x-slim"], mode: "slim", ext: ["slim"]},
	{name: "Smalltalk", mime: "text/x-stsrc", mode: "smalltalk", ext: ["st"]},
	{name: "Smarty", mime: "text/x-smarty", mode: "smarty", ext: ["tpl"]},
	{name: "Solr", mime: "text/x-solr", mode: "solr"},
	{name: "Soy", mime: "text/x-soy", mode: "soy", ext: ["soy"], alias: ["closure template"]},
	{name: "SPARQL", mime: "application/sparql-query", mode: "sparql", ext: ["rq", "sparql"], alias: ["sparul"]},
	{name: "Spreadsheet", mime: "text/x-spreadsheet", mode: "spreadsheet", alias: ["excel", "formula"]},
	{name: "SQL", mime: "text/x-sql", mode: "sql", ext: ["sql"]},
	{name: "SQLite", mime: "text/x-sqlite", mode: "sql"},
	{name: "Squirrel", mime: "text/x-squirrel", mode: "clike", ext: ["nut"]},
	{name: "Stylus", mime: "text/x-styl", mode: "stylus", ext: ["styl"]},
	{name: "Swift", mime: "text/x-swift", mode: "swift", ext: ["swift"]},
	{name: "sTeX", mime: "text/x-stex", mode: "stex"},
	{name: "LaTeX", mime: "text/x-latex", mode: "stex", ext: ["text", "ltx"], alias: ["tex"]},
	{name: "SystemVerilog", mime: "text/x-systemverilog", mode: "verilog", ext: ["v"]},
	{name: "Tcl", mime: "text/x-tcl", mode: "tcl", ext: ["tcl"]},
	{name: "Textile", mime: "text/x-textile", mode: "textile", ext: ["textile"]},
	{name: "TiddlyWiki ", mime: "text/x-tiddlywiki", mode: "tiddlywiki"},
	{name: "Tiki wiki", mime: "text/tiki", mode: "tiki"},
	{name: "TOML", mime: "text/x-toml", mode: "toml", ext: ["toml"]},
	{name: "Tornado", mime: "text/x-tornado", mode: "tornado"},
	{name: "troff", mime: "text/troff", mode: "troff", ext: ["1", "2", "3", "4", "5", "6", "7", "8", "9"]},
	{name: "TTCN", mime: "text/x-ttcn", mode: "ttcn", ext: ["ttcn", "ttcn3", "ttcnpp"]},
	{name: "TTCN_CFG", mime: "text/x-ttcn-cfg", mode: "ttcn-cfg", ext: ["cfg"]},
	{name: "Turtle", mime: "text/turtle", mode: "turtle", ext: ["ttl"]},
	{name: "TypeScript", mime: "application/typescript", mode: "javascript", ext: ["ts"], alias: ["ts"]},
	{name: "TypeScript-JSX", mime: "text/typescript-jsx", mode: "jsx", ext: ["tsx"], alias: ["tsx"]},
	{name: "Twig", mime: "text/x-twig", mode: "twig"},
	{name: "Web IDL", mime: "text/x-webidl", mode: "webidl", ext: ["webidl"]},
	{name: "VB.NET", mime: "text/x-vb", mode: "vb", ext: ["vb"]},
	{name: "VBScript", mime: "text/vbscript", mode: "vbscript", ext: ["vbs"]},
	{name: "Velocity", mime: "text/velocity", mode: "velocity", ext: ["vtl"]},
	{name: "Verilog", mime: "text/x-verilog", mode: "verilog", ext: ["v"]},
	{name: "VHDL", mime: "text/x-vhdl", mode: "vhdl", ext: ["vhd", "vhdl"]},
	{name: "Vue.js Component", mimes: ["script/x-vue", "text/x-vue"], mode: "vue", ext: ["vue"]},
	{name: "XML", mimes: ["application/xml", "text/xml"], mode: "xml", ext: ["xml", "xsl", "xsd", "svg"], alias: ["rss", "wsdl", "xsd"]},
	{name: "XQuery", mime: "application/xquery", mode: "xquery", ext: ["xy", "xquery"]},
	{name: "Yacas", mime: "text/x-yacas", mode: "yacas", ext: ["ys"]},
	{name: "YAML", mimes: ["text/x-yaml", "text/yaml"], mode: "yaml", ext: ["yaml", "yml"], alias: ["yml"]},
	{name: "Z80", mime: "text/x-z80", mode: "z80", ext: ["z80"]},
	{name: "mscgen", mime: "text/x-mscgen", mode: "mscgen", ext: ["mscgen", "mscin", "msc"]},
	{name: "xu", mime: "text/x-xu", mode: "mscgen", ext: ["xu"]},
	{name: "msgenny", mime: "text/x-msgenny", mode: "mscgen", ext: ["msgenny"]}
    ];
    // Ensure all modes have a mime property for backwards compatibility
    for (var i = 0; i < CodeMirror.modeInfo.length; i++) {
	var info = CodeMirror.modeInfo[i];
	if (info.mimes) info.mime = info.mimes[0];
    }

    CodeMirror.findModeByMIME = function(mime) {
	mime = mime.toLowerCase();
	for (var i = 0; i < CodeMirror.modeInfo.length; i++) {
	    var info = CodeMirror.modeInfo[i];
	    if (info.mime == mime) return info;
	    if (info.mimes) for (var j = 0; j < info.mimes.length; j++)
		if (info.mimes[j] == mime) return info;
	}
	if (/\+xml$/.test(mime)) return CodeMirror.findModeByMIME("application/xml")
	if (/\+json$/.test(mime)) return CodeMirror.findModeByMIME("application/json")
    };

    CodeMirror.findModeByExtension = function(ext) {
	for (var i = 0; i < CodeMirror.modeInfo.length; i++) {
	    var info = CodeMirror.modeInfo[i];
	    if (info.ext) for (var j = 0; j < info.ext.length; j++)
		if (info.ext[j] == ext) return info;
	}
    };

    CodeMirror.findModeByFileName = function(filename) {
	for (var i = 0; i < CodeMirror.modeInfo.length; i++) {
	    var info = CodeMirror.modeInfo[i];
	    if (info.file && info.file.test(filename)) return info;
	}
	var dot = filename.lastIndexOf(".");
	var ext = dot > -1 && filename.substring(dot + 1, filename.length);
	if (ext) return CodeMirror.findModeByExtension(ext);
    };

    CodeMirror.findModeByName = function(name) {
	name = name.toLowerCase();
	for (var i = 0; i < CodeMirror.modeInfo.length; i++) {
	    var info = CodeMirror.modeInfo[i];
	    if (info.name.toLowerCase() == name) return info;
	    if (info.alias) for (var j = 0; j < info.alias.length; j++)
		if (info.alias[j].toLowerCase() == name) return info;
	}
    };

    CodeMirror.defineMode("prolog", function(cmConfig) {

	function chain(stream, state, f) {
            state.tokenize = f;
            return f(stream, state);
	}

	/*******************************
	 *       CONFIG DATA        *
	 *******************************/

	var config = { quasiQuotations: false,        /* {|Syntax||Quotation|} */
                       dicts: false,            /* tag{k:v, ...} */
		       unicodeEscape: true,        /* \uXXXX and \UXXXXXXXX */
		       multiLineQuoted: true,        /* "...\n..." */
		       groupedIntegers: false        /* 10 000 or 10_000 */
		     };

	var quoteType = { '"': "string",
			  "'": "qatom",
			  "`": "bqstring"
			};

	var isSingleEscChar = /[abref\\'"nrtsv]/;
	var isOctalDigit    = /[0-7]/;
	var isHexDigit      = /[0-9a-fA-F]/;

	var isSymbolChar = /[-#$&*+./:<=>?@\\^~]/;    /* Prolog glueing symbols chars */
	var isSoloChar   = /[[\]{}(),;|]/;        /* Prolog solo chars] */
	var isNeck       = /^(:-|-->)$/;
	var isControlOp  = /^(,|;|->|\*->|\\+|\|)$/;


        /*******************************
         *     CHARACTER ESCAPES    *
         *******************************/

	function readDigits(stream, re, count) {
	    if ( count > 0 ) {
		while( count-- > 0 ) {
		    if ( !re.test(stream.next()) )
			return false;
		}
	    } else {
		while ( re.test(stream.peek()) )
		    stream.next();
	    }
	    return true;
	}

	function readEsc(stream) {
	    var next = stream.next();
	    if ( isSingleEscChar.test(next) )
		return true;
	    switch( next )
	    { case "u":
	      if ( config.unicodeEscape )
		  return readDigits(stream, isHexDigit, 4); /* SWI */
              return false;
	      case "U":
	      if ( config.unicodeEscape )
		  return readDigits(stream, isHexDigit, 8); /* SWI */
              return false;
	      case null: return true;            /* end of line */
	      case "c": stream.eatSpace(); return true;
	      case "x": return readDigits(stream, isHexDigit, 2);
	    }
	    if ( isOctalDigit.test(next) ) {
		if ( !readDigits(stream, isOctalDigit, -1) )
		    return false;
		if ( stream.peek() == "\\" )        /* SWI: optional closing \ */
		    stream.next();
		return true;
	    }
	    return false;
	}

	function nextUntilUnescaped(stream, state, end) {
	    var next;
	    while ((next = stream.next()) != null) {
		if ( next == end && end != stream.peek() )
		{ state.nesting.pop();
		  return false;
		}
		if ( next == "\\" )
		{ if ( !readEsc(stream) )
		    return false;
		}
	    }
	    return config.multiLineQuoted;
	}

        /*******************************
         *    CONTEXT NESTING        *
         *******************************/

	function nesting(state) {
	    return state.nesting.slice(-1)[0];
	}

	/* Called on every non-comment token */
	function setArg1(state) {
	    var nest = nesting(state);
	    if ( nest ) {
		if ( nest.arg == 0 )        /* nested in a compound */
		    nest.arg = 1;
		else if ( nest.type == "control" )
		    state.goalStart = false;
	    } else
		state.goalStart = false;
	}

	function setArgAlignment(state) {
	    var nest = nesting(state);
	    if ( nest && !nest.alignment && nest.arg != undefined ) {
		if ( nest.arg == 0 )
		    nest.alignment = nest.leftCol ? nest.leftCol+4 : nest.column+4;
		else
		    nest.alignment = nest.column+1;
	    }
	}

	function nextArg(state) {
	    var nest = nesting(state);
	    if ( nest ) {
		if ( nest.arg )            /* nested in a compound */
		    nest.arg++;
		else if ( nest.type == "control" )
		    state.goalStart = true;        /* FIXME: also needed for ; and -> */
	    } else
		state.goalStart = true;
	}

	function isControl(state) {        /* our terms are goals */
	    var nest = nesting(state);
	    if ( nest ) {
		if ( nest.type == "control" ) {
		    return true;
		}
		return false;
	    } else
		return state.inBody;
	}

	// Used as scratch variables to communicate multiple values without
	// consing up tons of objects.
	var type, content;
	function ret(tp, style, cont) {
	    type = tp; content = cont;
	    return style;
	}

	function peekSpace(stream) {        /* TBD: handle block comment as space */
	    if ( stream.eol() ||
		 /[\s%]/.test(stream.peek()) )
		return true;
	    return false;
	}


        /*******************************
         *       SUB TOKENISERS    *
         *******************************/

	function plTokenBase(stream, state) {
	    var ch = stream.next();

	    if ( ch == "(" ) {
		if ( state.lastType == "functor" ) {
		    state.nesting.push({ functor: state.functorName,
					 column: stream.column(),
					 leftCol: state.functorColumn,
					 arg: 0
				       });
		    delete state.functorName;
		    delete state.functorColumn;
		} else {
		    state.nesting.push({ type: "control",
					 closeColumn: stream.column(),
					 alignment: stream.column()+4
				       });
		}
		return ret("solo", null, "(");
	    }

	    if ( ch == "{" && state.lastType == "tag" ) {
		state.nesting.push({ tag: state.tagName,
				     column: stream.column(),
				     leftCol: state.tagColumn,
				     arg: 0
				   });
		delete state.tagName;
		delete state.tagColumn;
		return ret("dict_open", null);
	    }

	    if ( ch == "/" && stream.eat("*") )
		return chain(stream, state, plTokenComment);

	    if ( ch == "%" ) {
		stream.skipToEnd();
		return ret("comment", "comment");
	    }

	    setArg1(state);

	    if ( isSoloChar.test(ch) ) {
		switch ( ch )
		{ case ")":
		  state.nesting.pop();
		  break;
		  case "]":
		  state.nesting.pop();
		  return ret("list_close", null);
		  case "}":
		  { var nest = nesting(state);
		    var type = (nest && nest.tag) ? "dict_close" : "brace_term_close";

		    state.nesting.pop();
		    return ret(type, null);
		  }
		  case ",":
		  if ( stream.eol() )
		      state.commaAtEOL = true;
		  nextArg(state);
		  /*FALLTHROUGH*/
		  case ";":
		  if ( isControl(state) )
		      state.goalStart = true;
		  break;
		  case "[":
		  state.nesting.push({ type: "list",
				       closeColumn: stream.column(),
				       alignment: stream.column()+2
				     });
		  return ret("list_open", null);
		  break;
		  case "{":
		  if ( config.quasiQuotations && stream.eat("|") ) {
		      state.nesting.push({ type: "quasi-quotation",
					   alignment: stream.column()+1
					 });
		      return ret("qq_open", "qq_open");
		  } else {
		      state.nesting.push({ type: "curly",
					   closeColumn: stream.column(),
					   alignment: stream.column()+2
					 });
		      return ret("brace_term_open", null);
		  }
		  break;
		  case "|":
		  if ( config.quasiQuotations ) {
		      if ( stream.eat("|") ) {
			  state.tokenize = plTokenQuasiQuotation;
			  return ret("qq_sep", "qq_sep");
		      } else if ( stream.eat("}") ) {
			  state.nesting.pop();
			  return ret("qq_close", "qq_close");
		      }
		  }
		  if ( isControl(state) )
		      state.goalStart = true;
		  break;
		}
		return ret("solo", null, ch);
	    }

	    if (ch == '"' || ch == "'" || ch == "`")
	    { state.nesting.push({ type: "quoted",
				   alignment: stream.column()+1
				 });
	      return chain(stream, state, plTokenString(ch));
	    }

	    if ( ch == "0" ) {
		if ( stream.eat(/x/i)) {
		    stream.eatWhile(/[\da-f]/i);
		    return ret("number", "number");
		}
		if ( stream.eat(/o/i)) {
		    stream.eatWhile(/[0-7]/i);
		    return ret("number", "number");
		}
		if ( stream.eat(/'/) ) {            /* 0' */
		    var next = stream.next();
		    if ( next == "\\" ) {
			if ( !readEsc(stream) )
			    return ret("error", "error");
		    }
		    return ret("code", "code");
		}
	    }

	    if ( /\d/.test(ch) || /[+-]/.test(ch) && stream.eat(/\d/)) {
		if ( config.groupedIntegers )
		    stream.match(/^\d*((_|\s+)\d+)*(?:\.\d+)?(?:[eE][+\-]?\d+)?/);
		else
		    stream.match(/^\d*(?:\.\d+)?(?:[eE][+\-]?\d+)?/);
		return ret(ch == "-" ? "neg-number" :
			   ch == "+" ? "pos-number" :
			   "number");
	    }

	    if ( isSymbolChar.test(ch) ) {
		stream.eatWhile(isSymbolChar);
		var atom = stream.current();
		if ( atom == "." && peekSpace(stream) ) {
		    if ( nesting(state) ) {
			return ret("fullstop", "error", atom);
		    } else {
		    } return ret("fullstop", "fullstop", atom);
		} else if ( isNeck.test(atom) ) {
		    return ret("neck", "neck", atom);
		} else if ( isControl(state) && isControlOp.test(atom) ) {
		    state.goalStart = true;
		    return ret("symbol", "operator", atom);
		} else
		    return ret("symbol", "operator", atom);
	    }

	    stream.eatWhile(/[\w_]/);
	    var word = stream.current();
	    if ( stream.peek() == "{" && config.dicts ) {
		state.tagName = word;            /* tmp state extension */
		state.tagColumn = stream.column();
		return ret("tag", "tag", word);
	    } else if ( ch == "_" ) {
		if ( word.length == 1 ) {
		    return ret("var", "anon", word);
		} else {
		    var sec = word.charAt(1);
		    if ( sec == sec.toUpperCase() )
			return ret("var", "var-2", word);
		}
		return ret("var", "var", word);
	    } else if ( ch == ch.toUpperCase() ) {
		return ret("var", "var", word);
	    } else if ( stream.peek() == "(" ) {
		state.functorName = word;            /* tmp state extension */
		state.functorColumn = stream.column();
		return ret("functor", "functor", word);
	    } else
		return ret("atom", "atom", word);
	}

	function plTokenString(quote) {
	    return function(stream, state) {
		if (!nextUntilUnescaped(stream, state, quote)) {
		    state.tokenize = plTokenBase;
		    if ( stream.peek() == "(" ) {        /* 'quoted functor'() */
			var word = stream.current();
			state.functorName = word;        /* tmp state extension */
			return ret("functor", "functor", word);
		    }
		    if ( stream.peek() == "{" && config.dicts ) { /* 'quoted tag'{} */
			var word = stream.current();
			state.tagName = word;            /* tmp state extension */
			return ret("tag", "tag", word);
		    }
		}
		return ret(quoteType[quote], quoteType[quote]);
	    };
	}

	function plTokenQuasiQuotation(stream, state) {
	    var maybeEnd = false, ch;
	    while (ch = stream.next()) {
		if (ch == "}" && maybeEnd) {
		    state.tokenize = plTokenBase;
		    stream.backUp(2);
		    break;
		}
		maybeEnd = (ch == "|");
	    }
	    return ret("qq_content", "qq_content");
	}

	function plTokenComment(stream, state) {
	    var maybeEnd = false, ch;
	    while (ch = stream.next()) {
		if (ch == "/" && maybeEnd) {
		    state.tokenize = plTokenBase;
		    break;
		}
		maybeEnd = (ch == "*");
	    }
	    return ret("comment", "comment");
	}


	/*******************************
	 *        ACTIVE KEYS        *
	 *******************************/

	/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	   Support if-then-else layout like this:

	   goal :-
	   (    Condition
	   ->  IfTrue
	   ;   IfFalse
	   ).
	   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


	CodeMirror.commands.prologStartIfThenElse = function(cm) {
	    var start = cm.getCursor("start");
	    var token = cm.getTokenAt(start, true);

	    if ( token.state.goalStart == true )
	    { cm.replaceSelection("(   ", "end");
              return;
	    }

	    return CodeMirror.Pass;
	}

	CodeMirror.commands.prologStartThen = function(cm) {
	    var start = cm.getCursor("start");
	    var token = cm.getTokenAt(start, true);

	    /* FIXME: These functions are copied from prolog.js.  How
               can we reuse these?
	    */
	    function nesting(state) {
		var len = state.nesting.length;
		if ( len > 0 )
		    return state.nesting[len-1];
		return null;
	    }

	    function isControl(state) {        /* our terms are goals */
		var nest = nesting(state);
		if ( nest ) {
		    if ( nest.type == "control" ) {
			return true;
		    }
		    return false;
		} else
		    return state.inBody;
	    }

	    if ( start.ch == token.end &&
		 token.type == "operator" &&
		 token.string == "-" &&
		 isControl(token.state) )
	    { cm.replaceSelection(">  ", "end");
              return;
	    }

	    return CodeMirror.Pass;
	}

	CodeMirror.commands.prologStartElse = function(cm) {
	    var start = cm.getCursor("start");
	    var token = cm.getTokenAt(start, true);

	    if ( token.start == 0 && start.ch == token.end &&
		 !/\S/.test(token.string) )
	    { cm.replaceSelection(";   ", "end");
              return;
	    }

	    return CodeMirror.Pass;
	}

	CodeMirror.defineOption("prologKeys", null, function(cm, val, prev) {
	    if (prev && prev != CodeMirror.Init)
		cm.removeKeyMap("prolog");
	    if ( val ) {
		var map = { name:     "prolog",
			    "'('":    "prologStartIfThenElse",
			    "'>'":    "prologStartThen",
			    "';'":    "prologStartElse",
			    "Ctrl-L": "refreshHighlight"
			  };
		cm.addKeyMap(map);
	    }
	});

    });
    //Default (SWI-)Prolog operator table.   To be used later to enhance the
    //offline experience.

    var ops = { "-->":   { p:1200, t:"xfx" },
		":-":    [ { p:1200, t:"xfx" },
			   { p:1200, t:"fx" }
			 ],
		"?-":    { p:1200, t:"fx" },

		"dynamic":            { p:1150, t:"fx" },
		"discontiguous":      { p:1150, t:"fx" },
		"initialization":     { p:1150, t:"fx" },
		"meta_predicate":     { p:1150, t:"fx" },
		"module_transparent": { p:1150, t:"fx" },
		"multifile":          { p:1150, t:"fx" },
		"thread_local":       { p:1150, t:"fx" },
		"volatile":           { p:1150, t:"fx" },

		";":    { p:1100, t:"xfy" },
		"|":    { p:1100, t:"xfy" },

		"->":   { p:1050, t:"xfy" },
		"*->":  { p:1050, t:"xfy" },

		",":    { p:1000, t:"xfy" },

		"\\+":  { p:900,  t:"fy" },

		"~":    { p:900,  t:"fx" },

		"<":    { p:700,  t:"xfx" },
		"=":    { p:700,  t:"xfx" },
		"=..":  { p:700,  t:"xfx" },
		"=@=":  { p:700,  t:"xfx" },
		"=:=":  { p:700,  t:"xfx" },
		"=<":   { p:700,  t:"xfx" },
		"==":   { p:700,  t:"xfx" },
		"=\\=": { p:700,  t:"xfx" },
		">":    { p:700,  t:"xfx" },
		">=":   { p:700,  t:"xfx" },
		"@<":   { p:700,  t:"xfx" },
		"@=<":  { p:700,  t:"xfx" },
		"@>":   { p:700,  t:"xfx" },
		"@>=":  { p:700,  t:"xfx" },
		"\\=":  { p:700,  t:"xfx" },
		"\\==": { p:700,  t:"xfx" },
		"is":   { p:700,  t:"xfx" },

		":":    { p:600,  t:"xfy" },

		"+":    [ { p:500,  t:"yfx" },
			  { p:200,  t:"fy" }
			],
		"-":    [ { p:500,  t:"yfx" },
			  { p:200,  t:"fy" }
			],
		"/\\":  { p:500,  t:"yfx" },
		"\\/":  { p:500,  t:"yfx" },
		"xor":  { p:500,  t:"yfx" },

		"?":    { p:500,  t:"fx" },

		"*":    { p:400,  t:"yfx" },
		"/":    { p:400,  t:"yfx" },
		"//":   { p:400,  t:"yfx" },
		"rdiv": { p:400,  t:"yfx" },
		"<<":   { p:400,  t:"yfx" },
		">>":   { p:400,  t:"yfx" },
		"mod":  { p:400,  t:"yfx" },
		"rem":  { p:400,  t:"yfx" },

		"**":   { p:200,  t:"xfx" },
		"^":    { p:200,  t:"xfy" },

		"\\":   { p:200,  t:"fy" }
              };


    /*******************************
     *       RETURN OBJECT    *
     *******************************/

    return {
	startState: function() {
	    return {
		tokenize: plTokenBase,
		inBody: false,
		goalStart: false,
		lastType: null,
		nesting: new Array(),        /* ([{}]) nesting FIXME: copy this */
		curTerm: null,            /* term index in metainfo */
		curToken: null            /* token in term */
	    };
	},

	token: function(stream, state) {
	    var nest;

	    if ( state.curTerm == null /* && parserConfig.metainfo */ ) {
		state.curTerm = 0;
		state.curToken = 0;
	    }

	    if ( stream.sol() )
		delete state.commaAtEOL;

	    if ( state.tokenize == plTokenBase && stream.eatSpace() ) {
		if ( stream.eol() )
		    setArgAlignment(state);
		return null;
	    }

	    var style = state.tokenize(stream, state);

	    if ( stream.eol() )
		setArgAlignment(state);

	    if ( type == "neck" ) {
		state.inBody = true;
		state.goalStart = true;
	    } else if ( type == "fullstop" ) {
		state.inBody = false;
		state.goalStart = false;
	    }

	    state.lastType = type;

	    //if ( typeof(parserConfig.enrich) == "function" )
	    //   style = parserConfig.enrich(stream, state, type, content, style);

	    return style;
	},

	indent: function(state, textAfter) {
	    if (state.tokenize == plTokenComment) return CodeMirror.Pass;

	    var nest;
	    if ( (nest=nesting(state)) ) {
		if ( nest.closeColumn && !state.commaAtEOL )
		    return nest.closeColumn;
		return nest.alignment;
	    }
	    if ( !state.inBody )
		return 0;

	    return 4;
	},

	theme: "prolog",

	blockCommentStart: "/*",        /* continuecomment.js support */
	blockCommentEnd: "*/",
	blockCommentContinue: " * ",
	lineComment: "%",
    });
  }

});
