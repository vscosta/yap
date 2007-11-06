
:- encoding(utf8).		% this directive, when present, must be the first term in a source file


:- object(babel).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2005/04/06,
		comment is 'Simple test of the encoding/1 directive.']).

	:- public(hello_world/2).
	:- mode(hello_world(?atom, ?atom), zero_or_more).
	:- info(hello_world/2, [
		comment is 'Table of "hello world" messages in several languages (using ISO 639-2 two letter language codes for indexing).',
		argnames is ['Language', 'Text']]).

	hello_world(el, 'Γειάσου κόσμος!').
	hello_world(en, 'Hello world!').
	hello_world(es, '¡Hola mundo!').
	hello_world(ja, 'こんにちは世界!').
	hello_world(ko, '여보세요 세계!').
	hello_world(nl, 'Hallo wereld!').
	hello_world(pt, 'Olá mundo!').
	hello_world(ru, 'Здравствулте! мир!').
	hello_world(zh, '你好世界!').

:- end_object.
