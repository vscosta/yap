
:- object(root).

    :- public(p/1).
    :- dynamic(p/1).

    p(root).

:- end_object.


:- object(descendant,
    extends(root)).

:- end_object.
