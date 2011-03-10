/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(cgi,
	  [ cgi_get_form/1		% -ListOf Name(Value)
	  ]).
:- use_module(library(shlib)).

:- use_foreign_library(foreign(cgi), install_cgi).

/** <module> Read CGI parameters

Below is a very simple CGI script  that prints the passed parameters. To
test it, compile this program using the   command below, copy it to your
cgi-bin directory (or make it otherwise known  as a CGI-script) and try
the query =|http://myhost.mydomain/cgi-bin/cgidemo?hello=world|=

    ==
    % swipl -o cgidemo --goal=main --toplevel=halt -c cgidemo.pl
    ==

    ==
    :- use_module(library(cgi)).

    main :-
        set_stream(current_output, encoding(utf8)),
        cgi_get_form(Arguments),
        format('Content-type: text/html; charset=UTF-8~n~n', []),
        format('<html>~n', []),
        format('<head>~n', []),
        format('<title>Simple SWI-Prolog CGI script</title>~n', []),
        format('</head>~n~n', []),
        format('<body>~n', []),
        format('<p>', []),
        print_args(Arguments),
        format('</body>~n</html>~n', []).

    print_args([]).
    print_args([A0|T]) :-
        A0 =.. [Name, Value],
        format('<b>~w</b>=<em>~w</em><br>~n', [Name, Value]),
        print_args(T).
    ==
*/

%%	cgi_get_form(-Form)
%
%	Decodes standard input and the environment variables to obtain a
%	list of arguments passed to the  CGI script. This predicate both
%	deals with the CGI *GET* method as well as the *POST* method. If
%	the data cannot be  obtained,   an  existence_error exception is
%	raised.
%
%	@param Form is a list of Name(Value) terms.
