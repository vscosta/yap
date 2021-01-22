
:-   op(50, yf, []),
     op(50, yf, '()'),
     op(50, yf, '{}'),
     op(100, xfy, '.'),
     op(100, fy, '.').


:- module( xml4yap, [load_xml/2] ).

:- use_module(library(matrix)).

:- load_foreign_files(['libxml4yap'],[],libxml_yap_init).
