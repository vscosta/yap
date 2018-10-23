requirejs.config({
    map: {
      '*' : {
        'codemirror/mode/prolog/prolog':'/kernelspecs/yap_kernel/prolog.js'

      }
    }
});    

define([
    'codemirror/lib/codemirror',
 'base/js/namespace',
   'base/js/events',
],function(Jupyter,events,CodeMirror){
    var onload = function(){
      var mode = {
            mime: "text/x-prolog",
            name: "Prolog",
            mode: "prolog",
            ext: ["pl", "yap", "yss", "P"]
        };
      //console.log("I am being loaded");
        CodeMirror.requireMode("prolog", mode);
      CodeMirror.setOption("mode",mode);

    return {onload:onload};
}});
