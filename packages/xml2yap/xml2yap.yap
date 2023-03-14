
:-  

:- module( xml4yap, [load_xml/2,
		     op(50, yf, []),
		     op(50, yf, '()'),
     op(50, yf, '{}'),
     op(100, xfy, '.'),
     op(100, fy, '.')
] ).

:- use_module(library(matrix)).

:- load_foreign_files(['libPUGIXML'],[],libxml_yap_init).
