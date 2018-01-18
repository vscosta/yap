
;(function(window_CodeMirror){
  "use strict";

    // the `require` namespace for codemirror
    CM_PATH = "components/codemirror/";

  define(
    [
      "underscore",
      "jquery",
      CM_PATH + "lib/codemirror",
      "base/js/namespace",
      // silent upgrades
      "./jshint.js",
      CM_PATH + "addon/lint/javascript-lint",
      CM_PATH + "addon/hint/javascript-hint",
      CM_PATH + "addon/lint/lint",
      CM_PATH + "addon/hint/show-hint",
      "./prolog.js"
    ],
    function(_, $, CodeMirror, Jupyter){

                // the main function
                cm_tweak_js = function(cell){
                  var editor = cell.code_mirror,
                    opts = {},
                    meta = ensure_ns(cell),
                    keys = editor.getOption("extraKeys") || {},
                    mode = editor.getMode();

                    // only update editors we care about, reset ones we might have messed
                    if(!editor){
                      return;
                  } else {
                      editor.setOption("mode", "x-text/prolog");
                  }
          }
        }
    } // the `define` callback
  ); // the `define`
    ).call(this, window.CodeMirror);
