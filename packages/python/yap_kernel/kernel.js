
define([
    "/kernelspecs/yap_kernel/prolog.js"],function(CodeMirror){

	var onload = function(){
            console.log("I am being loaded");
	    var cell = Jupyter.notebook.get_selected_cell();
	    var cm = cell.config;
	    var patch = {
		CodeCell:{
		    cm_config:{mode: "prolog"} // only change here.
		}
	    };
	    config.update(patch);
	    return {onload:onload};
	}	
    }
      );
