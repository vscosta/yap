// CodeMirror, copyright (c) by Marijn Haverbeke and others
// Distributed under an MIT license: http://codemirror.net/LICENSE

(function(mod) {
if (typeof exports == "object" && typeof module == "object") // CommonJS
  mod(require("codemirror/lib/codemirror"));
else if (typeof define == "function" && define.amd) // AMD
  define([ "codemirror/lib/codemirror" ], mod);
else // Plain browser env
  mod(CodeMirror);
})(function(CodeMirror) {
"use strict";

CodeMirror.defineMode("prolog", function(conf, parserConfig) {
  function chain(stream, state, f) {
    state.tokenize = f;
    return f(stream, state);
  }

  var cm_;
  var curLine;

  /*******************************
   *       CONFIG DATA           *
   *******************************/

  var quasiQuotations =
      parserConfig.quasiQuotations || false;   /* {|Syntax||Quotation|} */
  var dialect = parserConfig.dialect || "yap"; /* {|Syntax||Quotation|} */
  var composeGoalWithDots =
      parserConfig.composeGoalWithDots || true; /* {|Syntax||Quotation|} */
  var dicts = parserConfig.dicts || false;      /* tag{k:v, ...} */
  var groupedIntegers =
      parserConfig.groupedIntegers || false; /* tag{k:v, ...} */
  var unicodeEscape =
      parserConfig.unicodeEscape || true; /* \uXXXX and \UXXXXXXXX */
  var multiLineQuoted = parserConfig.multiLineQuoted || true; /* "...\n..." */
  var quoteType = parserConfig.quoteType ||
                  {'"' : "string", "'" : "qatom", "`" : "bqstring"};

  var isSingleEscChar = /[abref\\'"nrtsv]/;
  var isOctalDigit = /[0-7]/;
  var isHexDigit = /[0-9a-fA-F]/;

  var isSymbolChar = /[-#$&*+./:<=>?@\\^~]/; /* Prolog glueing symbols chars */
  var isSoloChar = /[[\]{}(),;|!]/;          /* Prolog solo chars */
  var isNeck = /^(:-|-->)$/;
  var isControlOp = /^(,|;|->|\*->|\\+|\|)$/;

  /*******************************
   *        Linter Support        *
   *******************************/

  var errorFound = [];
  var exportedMsgs = [];

  function getLine(stream) {
    return stream.lineOracle.line;
    //  return cm_.getDoc().getCursor().line;
  }

  // var ed =
  // window.document.getElementsByClassName("CodeMirror")[0].CodeMirror.doc.getEditor();

  function rmError(stream) {
    if (cm_ == null)
    return;
    var doc = cm_.getDoc();
    var l = getLine(stream);
    // stream.lineOracle.line;
    for (var i = 0; i < errorFound.length; i++) {
      var elLine = doc.getLineNumber(errorFound[i].line);
      if (elLine == null || l === elLine) {
        errorFound.splice(i, 1);
        i -= 1;
        console.log(-elLine);
      }
    }
  }

  function mkError(stream, severity, msg) {
    if (stream.pos == 0)
      return;
    var l = cm_.getDoc().getLineHandle(getLine(stream));
    var found = errorFound.find(function(
        element) { return element.line === l && element.to == stream.pos; });
    if (!found) {
      console.log( getLine(stream) );
    errorFound.push({
        "line" : l,
        "from" : stream.start,
        "to" : stream.pos,
        severity : severity,
        message : msg
      });
    }
  }

  function exportErrors(text) {
    if (cm_ == null)
      return;
    var doc = cm_.getDoc();

    exportedMsgs.length = 0;
    for (var i = 0; i < errorFound.length; i += 1) {
      var e = errorFound[i];
      var l = doc.getLineNumber(e.line);
      if (l == null) {
        errorFound.splice(i, 1);
        i -= 1;
        continue;
      }
      exportedMsgs.push({
        "from" : CodeMirror.Pos(l, e.from),
        "to" : CodeMirror.Pos(l, e.to),
        "severity" : e.severity,
        "message" : e.message
      });
    }
    return exportedMsgs;
  }

  CodeMirror.registerHelper("lint", "prolog", exportErrors);

  /*******************************
   *     CHARACTER ESCAPES    *
   *******************************/

  function readDigits(stream, re, count) {
    if (count > 0) {
      while (count-- > 0) {
        if (!re.test(stream.next()))
          return false;
      }
    } else {
      while (re.test(stream.peek()))
        stream.next();
    }
    return true;
  }

  function readEsc(stream) {
    var next = stream.next();
    if (isSingleEscChar.test(next))
      return true;
    switch (next) {
    case "u":
      if (unicodeEscape)
        return readDigits(stream, isHexDigit, conf.indentUnit); /* SWI */
      return false;
    case "U":
      if (unicodeEscape)
        return readDigits(stream, isHexDigit, 8); /* SWI */
      return false;
    case null:
      return true; /* end of line */
    case "c":
      stream.eatSpace();
      return true;
    case "x":
      return readDigits(stream, isHexDigit, 2);
    }
    if (isOctalDigit.test(next)) {
      if (!readDigits(stream, isOctalDigit, -1))
        return false;
      if (stream.peek() == "\\") /* SWI: optional closing \ */
        stream.next();
      return true;
    }
    return false;
  }

  function nextUntilUnescaped(stream, state, end) {
    var next;
    while ((next = stream.next()) != null) {
      if (next == end && end != stream.peek()) {
        state.nesting.pop();
        return false;
      }
      if (next == "\\") {
        if (!readEsc(stream))
          return false;
      }
    }
    return multiLineQuoted;
  }

  /*******************************
   *    CONTEXT NESTING        *
   *******************************/

  function nesting(state) { return state.nesting.slice(-1)[0]; }

  /* Called on every non-comment token */
  function setArg1(state) {
    var nest = nesting(state);
    if (nest) {
      if (nest.arg == 0) /* nested in a compound */
        nest.arg = 1;
      else if (nest.type == "control")
        state.goalStart = false;
    } else
      state.goalStart = false;
  }

  function setArgAlignment(state) {
    var nest = nesting(state);
    if (nest && !nest.alignment && nest.arg != undefined) {
      if (nest.arg == 0)
        nest.alignment = nest.leftCol ? nest.leftCol + conf.indentUnit
                                      : nest.column + conf.indentUnit;
      else
        nest.alignment = nest.column + 1;
    }
  }

  function nextArg(state) {
    var nest = nesting(state);
    if (nest) {
      if (nest.arg) /* nested in a compound */
        nest.arg++;
      else if (nest.type == "control") {
        state.goalStart = true; /* FIXME: also needed for ; and -> */
      }
    } else {
      state.goalStart = true;
    }
  }

  function isControl(state) { /* our terms are goals */
    var nest = nesting(state);
    if (nest) {
      if (nest.type == "control") {
        return true;
      }
      return false;
    } else
      return state.inBody;
  }

  // Used as scratch variables to communicate multiple values without
  // consing up tons of objects.
  var type; //, content;
  function ret(tp, style, cont) {
    type = tp;
    //  content = cont;
    return style;
  }

  function peekSpace(stream) { /* TBD: handle block comment as space */
    if (stream.eol() || /[\s%]/.test(stream.peek()))
      return true;
    return false;
  }

  /*******************************
   *       SUB TOKENISERS    *
   *******************************/

  function plTokenBase(stream, state) {
    var ch = stream.next();

    if (ch == "(") {
      if (state.lastType == "functor") {
        state.nesting.push({
          functor : state.functorName,
          column : stream.column(),
          leftCol : state.functorColumn,
          arg : 0
        });
        delete state.functorName;
        delete state.functorColumn;
      } else {
        state.nesting.push({
          type : "control",
          closeColumn : stream.column(),
          alignment : stream.column() + 3
        });
      }
      return ret("solo", "tag", "(");
    }

    if (ch == "{" && state.lastType == "tag") {
      state.nesting.push({
        tag : state.tagName,
        column : stream.column(),
        leftCol : state.tagColumn,
        arg : 0
      });
      delete state.tagName;
      delete state.tagColumn;
      return ret("dict_open", "bracket");
    }

    if (ch == "/" && stream.eat("*"))
      return chain(stream, state, plTokenComment);

    if (ch == "%") {
      stream.skipToEnd();
      return ret("comment", "comment");
    }

    setArg1(state);

    if (isSoloChar.test(ch)) {
      switch (ch) {
      case ")": {
        state.nesting.pop();
      } break;
      case "]":

        state.nesting.pop();
        return ret("list_close", "bracket");
      case "}": {
        var nest = nesting(state);
        var type = (nest && nest.tag) ? "dict_close" : "brace_term_close";

        state.nesting.pop();
        return ret(type, null);
      } break;
      case ",": {
        if (stream.eol())
          state.commaAtEOL = true;
        nextArg(state);
        /*FALLTHROUGH*/
        if (isControl(state)) {
          if (!state.commaAtEOL)
            stream.eatSpace();
          if (!stream.peek("[")) {
            if (state.inBody) {
              state.goalStart = true;
            } else {
              return ret("solo", "error", ",");
            }
          }
        }
      } break;
      case ";":
        if (isControl(state)) {
          if (!state.inBody)
            return ret("solo", "error", ";");
          state.goalStart = true;
        }
        break;
      case "[":
        state.nesting.push({
          type : "list",
          closeColumn : stream.column(),
          alignment : stream.column() + 3
        });
        return ret("list_open", "bracket");
        break;
      case "{":
        if (quasiQuotations && stream.eat("|")) {
          state.nesting.push(
              {type : "quasi-quotation", alignment : stream.column() + 1});
          return ret("qq_open", "bracket");
        } else {
          state.nesting.push({
            type : "curly",
            closeColumn : stream.column(),
            alignment : stream.column() + 2
          });
          return ret("brace_term_open", "bracket");
        }
        break;
      case "|":
        if (quasiQuotations) {
          if (stream.eat("|")) {
            state.tokenize = plTokenQuasiQuotation;
            return ret("qq_sep", "bracket");
          } else if (stream.eat("}")) {
            state.nesting.pop();
            return ret("qq_close", "bracket");
          }
        }
        if (isControl(state)) {
          state.goalStart = true;
        }
        break;
        return ret("solo", "tag", ch);
      }
    }

    if (ch == '"' || ch == "'" || ch == "`") {
      state.nesting.push({type : "quoted", alignment : stream.column() + 1});
      return chain(stream, state, plTokenString(ch));
    }

    if (ch == "0") {
      if (stream.eat(/x/i)) {
        stream.eatWhile(/[\da-f]/i);
        return ret("number", "number");
      }
      if (stream.eat(/o/i)) {
        stream.eatWhile(/[0-7]/i);
        return ret("number", "number");
      }
      if (stream.eat(/'/)) { /* 0' */
        var next = stream.next();
        if (next == "\\") {
          if (!readEsc(stream))
            return ret("error", "error");
        }
        return ret("code", "number");
      }
    }

    if (/\d/.test(ch) || /[+-]/.test(ch) && stream.eat(/\d/)) {
      if (groupedIntegers)
        stream.match(/^\d*((_|\s+)\d+)*(?:\.\d+)?(?:[eE][+\-]?\d+)?/);
      else
        stream.match(/^\d*(?:\.\d+)?(?:[eE][+\-]?\d+)?/);
      return ret(ch == "-" ? "neg-number" : ch == "+" ? "-number" : "number",
                 "number");
    }

    if (isSymbolChar.test(ch)) {
      stream.eatWhile(isSymbolChar);
      var atom = stream.current();
      if (atom == "." && (stream.eol() || peekSpace(stream))) {
        if (nesting(state)) {
          mkError(stream, "error", "Clause over before closing all brackets");
          state.nesting = [];
        }
  //  var start = cm_.getCursor("end");
    //cm_.setBookmark(start, {"widget" : document.createTextNode("&bull;")});
        state.inBody = false;
        state.goalStart = true;
        stream.eat(ch);
        return ret("fullstop", "def", atom);

      } else {
        if (atom === ":-" && state.headStart) {

          return ret("directive", "attribute", atom);

        } else if (isNeck.test(atom)) {
          state.inBody = true;
          state.goalStart = true;
          return ret("neck", "property", atom);
        } else if (isControl(state) && isControlOp.test(atom)) {
          state.goalStart = true;
          return ret("symbol", "meta", atom);
        } else
          return ret("symbol", "meta", atom);
      }
    }
    stream.eatWhile(/[\w_]/);
    if (composeGoalWithDots) {
      while (stream.peek() == ".") {
        stream.eat('.');
        // a.b() and friends
        if ((ch = stream.peek()) == ' ' || stream.eol()) {
          stream.backUp(1);
          break;

        } else if (/[\w_]/.test(ch)) {
          stream.eatWhile(/[\w_]/);
        } else if (ch == "'") {

          stream.eat();

          stream.eatWhile(/[\w_]/);
          if (ch == ".") {
            return ret("atom", "error");
          }
        }
      }
    }
    var word = stream.current();
    var extra = "";
    if (stream.peek() == "{" && dicts) {
      state.tagName = word; /* tmp state extension */
      state.tagColumn = stream.column();
      return ret("tag", "tag", word);
    } else if ((ch = word[0]) == "_") {
      if (word.length == 1) {
        return ret("var", "variable-2", word);
      } else {
        var sec = word.charAt(1);
        if (sec == sec.toUpperCase())
          return ret("var", "variable-2", word);
      }
      return ret("var", "variable-2", word);
    } else if (ch == ch.toUpperCase()) {
      return ret("var", "variable-1", word);
    } else if (stream.peek() == "(") {
      state.functorName = word; /* tmp state extension */
      state.functorColumn = stream.column();
      if (state.headStart) {
        state.headStart = false;
        if (state.headFunctor != word) {
          state.headFunctor = word;
          return ret("functor", "def", word);
        }
      }
      if (builtins[word] && isControl(state))
        return ret("functor", "keyword", word);
      return ret("functor", "atom", word);
    } else if ((extra = stream.eatSpace())) {
      if (state.headStart && (stream.peek() == ":" || stream.peek() == "-")) {
        state.headStart = false;
        if (state.headFunctor != word) {
          state.headFunctor = word;
          return ret("functor", "def", word);
        }
      }
      state.functorName = word; /* tmp state extension */
      state.functorColumn = stream.column();
      var w = stream.current();
      if (builtins[word] && isControl(state))
        return ret("functor", "keyword", w);
      return ret("functor", "atom", w);
    } else if ((extra = stream.eat(/\/(\/)?\d\d?/) != "")) {
      state.functorName = word; /* tmp state extension */
      state.functorColumn = stream.column();
      var w = stream.current();
      if (builtins[word] && isControl(state))
        return ret("functor", "keyword", w);
      return ret("functor", "atom", w);
    } else if (builtins[word] && isControl(state))
      return ret("atom", "keyword", word);
    return ret("atom", "atom", word);
  }

  function plTokenString(quote) {
    return function(stream, state) {
      if (!nextUntilUnescaped(stream, state, quote)) {
        state.tokenize = plTokenBase;
        if (stream.peek() == "(") { /* 'quoted functor'() */
          var word = stream.current();
          state.functorName = word; /* tmp state extension */
          if (state.headStart) {
            state.headStart = false;
            if (state.headFunctor != word) {
              state.headFunctor = word;
              return ret("functor", "def", word);
            }
          }
          return ret("functor", "atom", word);
        }
        if (stream.peek() == "{" && dicts) { /* 'quoted tag'{} */
          var word = stream.current();
          state.tagName = word; /* tmp state extension */
          return ret("tag", "tag", word);
        }
      }
      return ret(quoteType[quote], "string");
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
    return ret("qq_content", "string");
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

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                                                                     - -
Support if-then-else layout like this:

goal :-
(
                                                             Condition
->
IfTrue
;
   IfFalse
).
    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
- - */

  CodeMirror.commands.prologStartIfThenElse = function(cm) {
    var start = cm.getCursor("start");
    var token = cm.getTokenAt(start, true);

    if (token.state.goalStart == true) {
      cm.replaceSelection("(\n", "end");
      return;
    }

    return CodeMirror.Pass;
  };

  CodeMirror.commands.prologStartThen =
      function(cm) {
    var start = cm.getCursor("start");
    var token = cm.getTokenAt(start, true);

    /* FIXME: These functions are copied from prolog.js.  How
                                                    can we reuse these?
             */
    function nesting(state) {
      var len = state.nesting.length;
      if (len > 0)
        return state.nesting[len - 1];
      return null;
    }

    function isControl(state) { /* our terms are goals */
      var nest = nesting(state);
      if (nest) {
        if (nest.type == "control") {
          return true;
        }
        return false;
      } else
        return state.inBody;
    }

    if (start.ch == token.end && token.type == "operator" &&
        token.string == "-" && isControl(token.state)) {
      cm.replaceSelection("\n->\n", "end");
      return;
    }

    return CodeMirror.Pass;
  }

      CodeMirror.commands.prologStartElse =
          function(cm) {
    var start = cm.getCursor("start");
    var token = cm.getTokenAt(start, true);

    if (token.start == 0 && start.ch == token.end && !/\S/.test(token.string)) {
      cm.replaceSelection("\n;\n", "end");
      return;
    }

    return CodeMirror.Pass;
  }

          CodeMirror.commands.prologEndClause =
              function(cm) {
    if (!cm.state.nesting() && !isControl(cm.state)) {
      var start = cm.getCursor("end");
      cm.setBookmark(start, {widget : document.createTextNode("&para;")});
      return;
    }
    return CodeMirror.Pass;
  }

              CodeMirror.defineOption(
                  "prologKeys", true, function(cm, editor, prev) {
                    cm_ = cm;
                    if (prev && prev != CodeMirror.Init)
                      cm.removeKeyMap("prolog");
                    if (true) {
                      var map = {
                        name : "prolog",
                        "Enter '('" : "prologStartIfThenElse",
                        "Enter '-' '>'" : "prologStartThen",
                        "Enter ';'" : "prologStartElse",
                        "Ctrl-L" : "refreshHighlight",
                        "Tab" : "indent"
                      };
                      cm.addKeyMap(map);
                    }
                  });

  // Default (SWI-)Prolog operator table.   To be used later to enhance
  // the offline experience.

  var ops = {
    "-->" : {p : 1200, t : "xfx"},
    ":-" : [ {p : 1200, t : "xfx"}, {p : 1200, t : "fx"} ],
    "?-" : {p : 1200, t : "fx"},

    "dynamic" : {p : 1150, t : "fx"},
    "discontiguous" : {p : 1150, t : "fx"},
    "initialization" : {p : 1150, t : "fx"},
    "meta_predicate" : {p : 1150, t : "fx"},
    "module_transparent" : {p : 1150, t : "fx"},
    "multifile" : {p : 1150, t : "fx"},
    "thread_local" : {p : 1150, t : "fx"},
    "volatile" : {p : 1150, t : "fx"},

    ";" : {p : 1100, t : "xfy"},
    "|" : {p : 1100, t : "xfy"},

    "->" : {p : 1050, t : "xfy"},
    "*->" : {p : 1050, t : "xfy"},

    "," : {p : 1000, t : "xfy"},

    "\\+" : {p : 900, t : "fy"},

    "~" : {p : 900, t : "fx"},

    "<" : {p : 700, t : "xfx"},
    "=" : {p : 700, t : "xfx"},
    "=.." : {p : 700, t : "xfx"},
    "=@=" : {p : 700, t : "xfx"},
    "=:=" : {p : 700, t : "xfx"},
    "=<" : {p : 700, t : "xfx"},
    "==" : {p : 700, t : "xfx"},
    "=\\=" : {p : 700, t : "xfx"},
    ">" : {p : 700, t : "xfx"},
    ">=" : {p : 700, t : "xfx"},
    "@<" : {p : 700, t : "xfx"},
    "@=<" : {p : 700, t : "xfx"},
    "@>" : {p : 700, t : "xfx"},
    "@>=" : {p : 700, t : "xfx"},
    "\\=" : {p : 700, t : "xfx"},
    "\\==" : {p : 700, t : "xfx"},
    "is" : {p : 700, t : "xfx"},

    ":" : {p : 600, t : "xfy"},

    "+" : [ {p : 500, t : "yfx"}, {p : 200, t : "fy"} ],
    "-" : [ {p : 500, t : "yfx"}, {p : 200, t : "fy"} ],
    "/\\" : {p : 500, t : "yfx"},
    "\\/" : {p : 500, t : "yfx"},
    "xor" : {p : 500, t : "yfx"},

    "?" : {p : 500, t : "fx"},

    "*" : {p : 400, t : "yfx"},
    "/" : {p : 400, t : "yfx"},
    "//" : {p : 400, t : "yfx"},
    "rdiv" : {p : 400, t : "yfx"},
    "<<" : {p : 400, t : "yfx"},
    ">>" : {p : 400, t : "yfx"},
    "mod" : {p : 400, t : "yfx"},
    "rem" : {p : 400, t : "yfx"},

    "**" : {p : 200, t : "xfx"},
    "^" : {p : 200, t : "xfy"},

    "\\" : {p : 200, t : "fy"}
  };

  var builtins = {
    "C" : "prolog",
    "abolish" : "prolog",
    "abolish_all_tables" : "prolog",
    "abolish_frozen_choice_points" : "prolog",
    "abolish_module" : "prolog",
    "abolish_table" : "prolog",
    "abort" : "prolog",
    "absolute_file_name" : "prolog",
    "absolute_file_system_path" : "prolog",
    "access" : "prolog",
    "access_file" : "prolog",
    "acyclic_term" : "prolog",
    "add_import_module" : "prolog",
    "add_to_array_element" : "prolog",
    "add_to_path" : "prolog",
    "alarm" : "prolog",
    "all" : "prolog",
    "always_prompt_user" : "prolog",
    "arena_size" : "prolog",
    "arg" : "prolog",
    "array" : "prolog",
    "array_element" : "prolog",
    "assert" : "prolog",
    "assert_static" : "prolog",
    "asserta" : "prolog",
    "asserta_static" : "prolog",
    "assertz" : "prolog",
    "assertz_static" : "prolog",
    "at_end_of_line" : "prolog",
    "at_end_of_stream" : "prolog",
    "at_end_of_stream_0" : "prolog",
    "at_halt" : "prolog",
    "atom" : "prolog",
    "atom_chars" : "prolog",
    "atom_codes" : "prolog",
    "atom_concat" : "prolog",
    "atom_length" : "prolog",
    "atom_number" : "prolog",
    "atom_string" : "prolog",
    "atom_to_term" : "prolog",
    "atomic_concat" : "prolog",
    "atomic_length" : "prolog",
    "atomic_list_concat" : "prolog",
    "atomics_to_string" : "prolog",
    "attvar" : "prolog",
    "b_getval" : "prolog",
    "b_setval" : "prolog",
    "bagof" : "prolog",
    "bb_delete" : "prolog",
    "bb_get" : "prolog",
    "bb_put" : "prolog",
    "bb_update" : "prolog",
    "between" : "prolog",
    "bootstrap" : "prolog",
    "break" : "prolog",
    "call" : "prolog",
    "call_cleanup" : "prolog",
    "call_count" : "prolog",
    "call_count_data" : "prolog",
    "call_count_reset" : "prolog",
    "call_residue" : "prolog",
    "call_residue_vars" : "prolog",
    "call_shared_object_function" : "prolog",
    "call_with_args" : "prolog",
    "callable" : "prolog",
    "catch" : "prolog",
    "catch_ball" : "prolog",
    "cd" : "prolog",
    "cfile_search_path" : "prolog",
    "char_code" : "prolog",
    "char_conversion" : "prolog",
    "char_type" : "prolog",
    "clause" : "prolog",
    "clause_property" : "prolog",
    "close" : "prolog",
    "close_shared_object" : "prolog",
    "close_static_array" : "prolog",
    "code_type" : "prolog",
    "commons_directory" : "prolog",
    "commons_library" : "prolog",
    "compare" : "prolog",
    "compile" : "prolog",
    "compile_expressions" : "prolog",
    "compile_predicates" : "prolog",
    "compound" : "prolog",
    "consult" : "prolog",
    "consult_depth" : "prolog",
    "context_module" : "prolog",
    "copy_term" : "prolog",
    "copy_term_nat" : "prolog",
    "create_mutable" : "prolog",
    "create_prolog_flag" : "prolog",
    "creep_allowed" : "prolog",
    "current_atom" : "prolog",
    "current_char_conversion" : "prolog",
    "current_host" : "prolog",
    "current_input" : "prolog",
    "current_key" : "prolog",
    "current_line_number" : "prolog",
    "current_module" : "prolog",
    "current_mutex" : "prolog",
    "current_op" : "prolog",
    "current_predicate" : "prolog",
    "current_prolog_flag" : "prolog",
    "current_reference_count" : "prolog",
    "current_stream" : "prolog",
    "current_thread" : "prolog",
    "db_files" : "prolog",
    "db_reference" : "prolog",
    "debug" : "prolog",
    "debugging" : "prolog",
    "decrease_reference_count" : "prolog",
    "del_attr" : "prolog",
    "del_attrs" : "prolog",
    "delete_import_module" : "prolog",
    "depth_bound_call" : "prolog",
    "dif" : "prolog",
    "discontiguous" : "prolog",
    "display" : "prolog",
    "do_c_built_in" : "prolog",
    "do_c_built_metacall" : "prolog",
    "do_not_compile_expressions" : "prolog",
    "dule" : "prolog",
    "dum" : "prolog",
    "dump_active_goals" : "prolog",
    "duplicate_term" : "prolog",
    "dynamic" : "prolog",
    "dynamic_predicate" : "prolog",
    "dynamic_update_array" : "prolog",
    "eamconsult" : "prolog",
    "eamtrans" : "prolog",
    "end_of_file" : "prolog",
    "ensure_loaded" : "prolog",
    "erase" : "prolog",
    "eraseall" : "prolog",
    "erased" : "prolog",
    "exists" : "prolog",
    "exists_directory" : "prolog",
    "exists_file" : "prolog",
    "exists_source" : "prolog",
    "exo_files" : "prolog",
    "expand_expr" : "prolog",
    "expand_exprs" : "prolog",
    "expand_file_name" : "prolog",
    "expand_goal" : "prolog",
    "expand_term" : "prolog",
    "expects_dialect" : "prolog",
    "export" : "prolog",
    "export_list" : "prolog",
    "export_resource" : "prolog",
    "extend" : "prolog",
    "fail" : "prolog",
    "false" : "prolog",
    "file_base_name" : "prolog",
    "file_directory_name" : "prolog",
    "file_exists" : "prolog",
    "file_name_extension" : "prolog",
    "file_search_path" : "prolog",
    "file_size" : "prolog",
    "fileerrors" : "prolog",
    "findall" : "prolog",
    "float" : "prolog",
    "flush_output" : "prolog",
    "forall" : "prolog",
    "foreign_directory" : "prolog",
    "format" : "prolog",
    "freeze" : "prolog",
    "freeze_choice_point" : "prolog",
    "frozen" : "prolog",
    "functor" : "prolog",
    "garbage_collect" : "prolog",
    "garbage_collect_atoms" : "prolog",
    "gc" : "prolog",
    "get" : "prolog",
    "get0" : "prolog",
    "get_attr" : "prolog",
    "get_attrs" : "prolog",
    "get_byte" : "prolog",
    "get_char" : "prolog",
    "get_code" : "prolog",
    "get_depth_limit" : "prolog",
    "get_mutable" : "prolog",
    "get_string_code" : "prolog",
    "get_value" : "prolog",
    "getcwd" : "prolog",
    "getenv" : "prolog",
    "global_trie_statistics" : "prolog",
    "ground" : "prolog",
    "grow_heap" : "prolog",
    "grow_stack" : "prolog",
    "halt" : "prolog",
    "heap_space_info" : "prolog",
    "hide_atom" : "prolog",
    "hide_predicate" : "prolog",
    "hostname_address" : "prolog",
    "hread_get_message" : "prolog",
    "hread_signal" : "prolog",
    "if" : "prolog",
    "ignore" : "prolog",
    "import_module" : "prolog",
    "incore" : "prolog",
    "increase_reference_count" : "prolog",
    "init_random_state" : "prolog",
    "initialization" : "prolog",
    "instance" : "prolog",
    "instance_property" : "prolog",
    "int_message" : "prolog",
    "integer" : "prolog",
    "is" : "prolog",
    "is_absolute_file_name" : "prolog",
    "is_list" : "prolog",
    "is_mutable" : "prolog",
    "is_tabled" : "prolog",
    "isinf" : "prolog",
    "isnan" : "prolog",
    "key_erased_statistics" : "prolog",
    "key_statistics" : "prolog",
    "keysort" : "prolog",
    "leash" : "prolog",
    "length" : "prolog",
    "libraries_directories" : "prolog",
    "line_count" : "prolog",
    "listing" : "prolog",
    "load_absolute_foreign_files" : "prolog",
    "load_db" : "prolog",
    "load_files" : "prolog",
    "load_foreign_files" : "prolog",
    "log_event" : "prolog",
    "logsum" : "prolog",
    "ls" : "prolog",
    "ls_imports" : "prolog",
    "make" : "prolog",
    "make_directory" : "prolog",
    "make_library_index" : "prolog",
    "message_queue_create" : "prolog",
    "message_queue_destroy" : "prolog",
    "message_queue_property" : "prolog",
    "message_to_string" : "prolog",
    "mmapped_array" : "prolog",
    "module" : "prolog",
    "module_property" : "prolog",
    "module_state" : "prolog",
    "msort" : "prolog",
    "multifile" : "prolog",
    "must_be_of_type" : "prolog",
    "mutex_create" : "prolog",
    "mutex_property" : "prolog",
    "mutex_unlock_all" : "prolog",
    "name" : "prolog",
    "nb_create" : "prolog",
    "nb_current" : "prolog",
    "nb_delete" : "prolog",
    "nb_getval" : "prolog",
    "nb_linkarg" : "prolog",
    "nb_linkval" : "prolog",
    "nb_set_bit" : "prolog",
    "nb_set_shared_arg" : "prolog",
    "nb_set_shared_val" : "prolog",
    "nb_setarg" : "prolog",
    "nb_setval" : "prolog",
    "new_system_module" : "prolog",
    "nl" : "prolog",
    "no_source" : "prolog",
    "no_style_check" : "prolog",
    "nodebug" : "prolog",
    "nofileeleerrors" : "prolog",
    "nogc" : "prolog",
    "nonvar" : "prolog",
    "nospy" : "prolog",
    "nospyall" : "prolog",
    "not" : "prolog",
    "notrace" : "prolog",
    "nth_clause" : "prolog",
    "nth_instance" : "prolog",
    "number" : "prolog",
    "number_atom" : "prolog",
    "number_chars" : "prolog",
    "number_codes" : "prolog",
    "on_signal" : "prolog",
    "once" : "prolog",
    "op" : "prolog",
    "opaque" : "prolog",
    "open" : "prolog",
    "open_pipe_stream" : "prolog",
    "open_shared_object" : "prolog",
    "opt_statistics" : "prolog",
    "or_statistics" : "prolog",
    "ortray_clause" : "prolog",
    "otherwise" : "prolog",
    "parallel" : "prolog",
    "parallel_findall" : "prolog",
    "parallel_findfirst" : "prolog",
    "parallel_once" : "prolog",
    "path" : "prolog",
    "peek" : "prolog",
    "peek_byte" : "prolog",
    "peek_char" : "prolog",
    "peek_code" : "prolog",
    "phrase" : "prolog",
    "plus" : "prolog",
    "portray_clause" : "prolog",
    "predicate_erased_statistics" : "prolog",
    "predicate_property" : "prolog",
    "predicate_statistics" : "prolog",
    "predmerge" : "prolog",
    "predsort" : "prolog",
    "primitive" : "prolog",
    "print" : "prolog",
    "print_message" : "prolog",
    "print_message_lines" : "prolog",
    "private" : "prolog",
    "profalt" : "prolog",
    "profend" : "prolog",
    "profile_data" : "prolog",
    "profile_reset" : "prolog",
    "profinit" : "prolog",
    "profoff" : "prolog",
    "profon" : "prolog",
    "prolog" : "prolog",
    "prolog_current_frame" : "prolog",
    "prolog_file_name" : "prolog",
    "prolog_file_type" : "prolog",
    "prolog_flag" : "prolog",
    "prolog_flag_property" : "prolog",
    "prolog_initialization" : "prolog",
    "prolog_load_context" : "prolog",
    "prolog_to_os_filename" : "prolog",
    "prompt" : "prolog",
    "prompt1" : "prolog",
    "put" : "prolog",
    "put_attr" : "prolog",
    "put_attrs" : "prolog",
    "put_byte" : "prolog",
    "put_char" : "prolog",
    "put_char1" : "prolog",
    "put_code" : "prolog",
    "putenv" : "prolog",
    "pwd" : "prolog",
    "qend_program" : "prolog",
    "qload_file" : "prolog",
    "qload_module" : "prolog",
    "qpack_clean_up_to_disjunction" : "prolog",
    "qsave_file" : "prolog",
    "qsave_module" : "prolog",
    "qsave_program" : "prolog",
    "raise_exception" : "prolog",
    "rational" : "prolog",
    "rational_term_to_tree" : "prolog",
    "read" : "prolog",
    "read_clause" : "prolog",
    "read_sig" : "prolog",
    "read_term" : "prolog",
    "read_term_from_atom" : "prolog",
    "read_term_from_atomic" : "prolog",
    "read_term_from_string" : "prolog",
    "real_path" : "prolog",
    "reconsult" : "prolog",
    "recorda" : "prolog",
    "recorda_at" : "prolog",
    "recordaifnot" : "prolog",
    "recorded" : "prolog",
    "recordz" : "prolog",
    "recordz_at" : "prolog",
    "recordzifnot" : "prolog",
    "release_random_state" : "prolog",
    "remove_from_path" : "prolog",
    "rename" : "prolog",
    "repeat" : "prolog",
    "reset_static_array" : "prolog",
    "reset_total_choicepoints" : "prolog",
    "resize_static_array" : "prolog",
    "restore" : "prolog",
    "retract" : "prolog",
    "retractall" : "prolog",
    "rmdir" : "prolog",
    "same_file" : "prolog",
    "save_program" : "prolog",
    "see" : "prolog",
    "seeing" : "prolog",
    "seen" : "prolog",
    "set_base_module" : "prolog",
    "set_input" : "prolog",
    "set_output" : "prolog",
    "set_prolog_flag" : "prolog",
    "set_random_state" : "prolog",
    "set_stream" : "prolog",
    "set_stream_position" : "prolog",
    "set_value" : "prolog",
    "setarg" : "prolog",
    "setenv" : "prolog",
    "setof" : "prolog",
    "setup_call_catcher_cleanup" : "prolog",
    "setup_call_cleanup" : "prolog",
    "sformat" : "prolog",
    "sh" : "prolog",
    "show_all_local_tables" : "prolog",
    "show_all_tables" : "prolog",
    "show_global_trie" : "prolog",
    "show_global_trieshow_tabled_predicates" : "prolog",
    "show_low_level_trace" : "prolog",
    "show_table" : "prolog",
    "show_tabled_predicates" : "prolog",
    "showprofres" : "prolog",
    "simple" : "prolog",
    "skip" : "prolog",
    "skip1" : "prolog",
    "socket" : "prolog",
    "socket_accept" : "prolog",
    "socket_bind" : "prolog",
    "socket_close" : "prolog",
    "socket_connect" : "prolog",
    "socket_listen" : "prolog",
    "sort" : "prolog",
    "sort2" : "prolog",
    "source" : "prolog",
    "source_file" : "prolog",
    "source_file_property" : "prolog",
    "source_location" : "prolog",
    "source_mode" : "prolog",
    "source_module" : "prolog",
    "split_path_file" : "prolog",
    "spy" : "prolog",
    "srandom" : "prolog",
    "start_low_level_trace" : "prolog",
    "stash_predicate" : "prolog",
    "static_array" : "prolog",
    "static_array_location" : "prolog",
    "static_array_properties" : "prolog",
    "static_array_to_term" : "prolog",
    "statistics" : "prolog",
    "stop_low_level_trace" : "prolog",
    "stream_position" : "prolog",
    "stream_position_data" : "prolog",
    "stream_property" : "prolog",
    "stream_select" : "prolog",
    "string" : "prolog",
    "string_chars" : "prolog",
    "string_code" : "prolog",
    "string_codes" : "prolog",
    "string_concat" : "prolog",
    "string_length" : "prolog",
    "string_number" : "prolog",
    "string_to_atom" : "prolog",
    "string_to_atomic" : "prolog",
    "string_to_list" : "prolog",
    "strip_module" : "prolog",
    "style_check" : "prolog",
    "sub_atom" : "prolog",
    "sub_string" : "prolog",
    "subsumes_term" : "prolog",
    "succ" : "prolog",
    "sys_debug" : "prolog",
    "system" : "prolog",
    "system_error" : "prolog",
    "system_library" : "prolog",
    "system_module" : "prolog",
    "system_predicate" : "prolog",
    "t_body" : "prolog",
    "t_head" : "prolog",
    "t_hgoal" : "prolog",
    "t_hlist" : "prolog",
    "t_tidy" : "prolog",
    "tab" : "prolog",
    "tab1" : "prolog",
    "table" : "prolog",
    "table_statistics" : "prolog",
    "tabling_mode" : "prolog",
    "tabling_statistics" : "prolog",
    "tell" : "prolog",
    "telling" : "prolog",
    "term_attvars" : "prolog",
    "term_factorized" : "prolog",
    "term_to_atom" : "prolog",
    "term_to_string" : "prolog",
    "term_variables" : "prolog",
    "thread_at_exit" : "prolog",
    "thread_cancel" : "prolog",
    "thread_create" : "prolog",
    "thread_default" : "prolog",
    "thread_defaults" : "prolog",
    "thread_detach" : "prolog",
    "thread_exit" : "prolog",
    "thread_get_message" : "prolog",
    "thread_join" : "prolog",
    "thread_local" : "prolog",
    "thread_peek_message" : "prolog",
    "thread_property" : "prolog",
    "thread_self" : "prolog",
    "thread_send_message" : "prolog",
    "thread_set_default" : "prolog",
    "thread_set_defaults" : "prolog",
    "thread_signal" : "prolog",
    "thread_sleep" : "prolog",
    "thread_statistics" : "prolog",
    "threads" : "prolog",
    "throw" : "prolog",
    "time" : "prolog",
    "time_file" : "prolog",
    "time_file64" : "prolog",
    "told" : "prolog",
    "tolower" : "prolog",
    "total_choicepoints" : "prolog",
    "total_erased" : "prolog",
    "toupper" : "prolog",
    "trace" : "prolog",
    "true" : "prolog",
    "true_file_name" : "prolog",
    "tthread_peek_message" : "prolog",
    "ttyget" : "prolog",
    "ttyget0" : "prolog",
    "ttynl" : "prolog",
    "ttyput" : "prolog",
    "ttyskip" : "prolog",
    "udi" : "prolog",
    "unhide_atom" : "prolog",
    "unify_with_occurs_check" : "prolog",
    "unix" : "prolog",
    "unknown" : "prolog",
    "unload_file" : "prolog",
    "unload_module" : "prolog",
    "unnumbervars" : "prolog",
    "update_array" : "prolog",
    "update_mutable" : "prolog",
    "use_module" : "prolog",
    "use_system_module" : "prolog",
    "user_defined_directive" : "prolog",
    "var" : "prolog",
    "version" : "prolog",
    "volatile" : "prolog",
    "wake_choice_point" : "prolog",
    "when" : "prolog",
    "with_mutex" : "prolog",
    "with_output_to" : "prolog",
    "working_directory" : "prolog",
    "write" : "prolog",
    "write_canonical" : "prolog",
    "write_depth" : "prolog",
    "write_term" : "prolog",
    "writeln" : "prolog",
    "writeq" : "prolog",
    "yap_flag" : "prolog"
  };

  /*******************************
   *       RETURN OBJECT         *
   *******************************/

  var external = {
    startState : function() {
      return {
        tokenize : plTokenBase,
        inBody : false,
        goalStart : false,
        headStart : true,
        headFunctor : "",
        lastType : null,
        nesting : new Array(), /* ([{}]) nesting FIXME: copy this */
        curTerm : null,        /* term index in metainfo */
        curToken : null        /* token in term */
      };
    },
    token : function(stream, state) {
      // var nest;

      if (state.curTerm == null) {
        if (parserConfig.metainfo) {
          state.curTerm = 0;
          state.curToken = 0;
        }
      }

      if (stream.eol()) {
        curLine++;
        delete state.commaAtEOL;
      }

      if (state.tokenize == plTokenBase && stream.eatSpace()) {
        if (stream.eol())
          setArgAlignment(state);
        return null;
      }
      if (state.curLine == null || state.pos == 0)
        rmError(stream);

      var style = state.tokenize(stream, state);

      if (stream.eol()) {
        if (stream.pos > 0)
          setArgAlignment(state);
      }

      state.lastType = type;

      if (builtins[state.curToken] == "prolog")
        return "builtin";
      if (ops[state.curToken])
        return "keyword";

      // if (typeof(parserConfig.enrich) == "function")
      //  style = parserConfig.enrich(stream, state, type, content,
      //  style);

      return style;
    },

    indent : function(state, textAfter) {
      if (state.tokenize == plTokenComment)
        return CodeMirror.Pass;

      var nest;
      if (!state.inBody)
        return 0;
      var ctl = isControl(state);
      if ((nest = nesting(state))) {
        if (!ctl && nest.closeColumn && !state.commaAtEOL)
          return nest.closeColumn;
        if ((textAfter === ']' || textAfter === ')' || textAfter === ').' ||
             textAfter === '->' || textAfter === ';' || textAfter === '*->') &&
            ctl)
          return nest.alignment - 3;
        return nest.alignment;
      }

      return conf.indentUnit;
    },

    electricInput : /^\s*([\}\]\)\;]|\-\>}\*\-\>|\)\.)$/,
    blockCommentStart : "/*", /* continuecomment.js support */
    blockCommentEnd : "*/",
    blockCommentContinue : " * ",
    comment : "%",
    fold : "indent"
  };
  return external;
});

CodeMirror.defineMIME("text/x-prolog", "prolog");
});
