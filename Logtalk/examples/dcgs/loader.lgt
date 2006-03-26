
:- initialization(
	logtalk_load([
		parsep,
		calculator,
		enigma,
		parsetree,
		sentences,
		tokenizer,
		url,
		xml,
		shell,
		walker,
		bom,
		faa,
		bypass,
		dcgtest])). 

/*
If you intend to use the FOP XSL:FO processor for generating PDF documenting
files, comment the directive above and uncomment the directive below

:- initialization(
	logtalk_load(
		[parsep,
		calculator,
		enigma,
		parsetree,
		sentences,
		tokenizer,
		url,
		xml,
		shell,
		walker,
		bom,
		faa,
		bypass,
		dcgtest], [xmlsref(standalone)])).
*/
