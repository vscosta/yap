"use strict";
// Copyright (c) Jupyter Development Team.
// Distributed under the terms of the Modified BSD License.
Object.defineProperty(exports, "__esModule", { value: true });
var codeeditor_1 = require("@jupyterlab/codeeditor");
var CodeMirror = require("codemirror");
require("codemirror/mode/meta");
require("codemirror/addon/runmode/runmode");
require("./codemirror-ipython");
require("./codemirror-ipythongfm");
// Bundle other common modes
require("codemirror/mode/javascript/javascript");
require("codemirror/mode/css/css");
require("codemirror/mode/prolog/prolog");
require("codemirror/mode/julia/julia");
require("codemirror/mode/r/r");
require("codemirror/mode/markdown/markdown");
require("codemirror/mode/clike/clike");
require("codemirror/mode/shell/shell");
require("codemirror/mode/sql/sql");
var coreutils_1 = require("@jupyterlab/coreutils");
/**
 * The namespace for CodeMirror Mode functionality.
 */
var Mode;
(function (Mode) {
    /**
     * Get the raw list of available modes specs.
     */
    function getModeInfo() {
        return CodeMirror.modeInfo;
    }
    Mode.getModeInfo = getModeInfo;
    /**
     * Running a CodeMirror mode outside of an editor.
     */
    function run(code, mode, el) {
        CodeMirror.runMode(code, mode, el);
    }
    Mode.run = run;
    /**
     * Ensure a codemirror mode is available by name or Codemirror spec.
     *
     * @param mode - The mode to ensure.  If it is a string, uses [findBest]
     *   to get the appropriate spec.
     *
     * @returns A promise that resolves when the mode is available.
     */
    function ensure(mode) {
        var spec = findBest(mode);
        // Simplest, cheapest check by mode name.
        if (CodeMirror.modes.hasOwnProperty(spec.mode)) {
            return Promise.resolve(spec);
        }
        // Fetch the mode asynchronously.
        return new Promise(function (resolve, reject) {
            require(["codemirror/mode/" + spec.mode + "/" + spec.mode + ".js"], function () {
                resolve(spec);
            });
        });
    }
    Mode.ensure = ensure;
    /**
     * Find a codemirror mode by name or CodeMirror spec.
     */
    function findBest(mode) {
        var modename = (typeof mode === 'string') ? mode :
            mode.mode || mode.name;
        var mimetype = (typeof mode !== 'string') ? mode.mime : modename;
        var ext = (typeof mode !== 'string') ? mode.ext : [];
        return (CodeMirror.findModeByName(modename || '') ||
            CodeMirror.findModeByMIME(mimetype || '') ||
            findByExtension(ext) ||
            CodeMirror.findModeByMIME(codeeditor_1.IEditorMimeTypeService.defaultMimeType) ||
            CodeMirror.findModeByMIME('text/plain'));
    }
    Mode.findBest = findBest;
    /**
     * Find a codemirror mode by MIME.
     */
    function findByMIME(mime) {
        return CodeMirror.findModeByMIME(mime);
    }
    Mode.findByMIME = findByMIME;
    /**
     * Find a codemirror mode by name.
     */
    function findByName(name) {
        return CodeMirror.findModeByName(name);
    }
    Mode.findByName = findByName;
    /**
     * Find a codemirror mode by filename.
     */
    function findByFileName(name) {
        var basename = coreutils_1.PathExt.basename(name);
        return CodeMirror.findModeByFileName(basename);
    }
    Mode.findByFileName = findByFileName;
    /**
     * Find a codemirror mode by extension.
     */
    function findByExtension(ext) {
        if (typeof ext === 'string') {
            return CodeMirror.findModeByExtension(name);
        }
        for (var i = 0; i < ext.length; i++) {
            var mode = CodeMirror.findModeByExtension(ext[i]);
            if (mode) {
                return mode;
            }
        }
    }
    Mode.findByExtension = findByExtension;
})(Mode = exports.Mode || (exports.Mode = {}));
