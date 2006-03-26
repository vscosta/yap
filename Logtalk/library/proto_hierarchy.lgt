
:- category(proto_hierarchy,
	implements(proto_hierarchyp)).


	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2006/2/20,
		comment is 'Prototype hierarchy predicates.']).


	parent(Parent) :-
		self(Self),
		extends_object(Self, Parent).


	parents(Parents) :-
		self(Self),
		findall(Parent, extends_object(Self, Parent), Parents).


	ancestor(Ancestor) :-
		self(Self),
		ancestor(Self, Ancestor).


	ancestor(Self, Ancestor) :-
		extends_object(Self, Ancestor).

	ancestor(Self, Ancestor) :-
		extends_object(Self, Parent),
		ancestor(Parent, Ancestor).


	ancestors(Ancestors) :-
		self(Self),
		findall(Ancestor, ancestor(Self, Ancestor), Ancestors).


	extension(Prototype) :-
		self(Self),
		extends_object(Prototype, Self).


	extensions(Prototypes) :-
		self(Self),
		findall(Prototype, extends_object(Prototype, Self), Prototypes).


	leaf(Leaf) :-
		self(Self),
		leaf(Self, Leaf).


	leaf(Self, Leaf) :-
		extends_object(Leaf, Self),
		\+ extends_object(_, Leaf).

	leaf(Self, Leaf) :-
		extends_object(Object, Self),
		leaf(Object, Leaf).


	leaves(Leaves) :-
		self(Self),
		(	setof(Leaf, leaf(Self, Leaf), Leaves) ->
			true
		;	Leaves = []
		).


	descendant(Descendant) :-
		self(Self),
		descendant(Self, Descendant).


	descendant(Self, Descendant) :-
		extends_object(Descendant, Self).

	descendant(Self, Descendant) :-
		extends_object(Descendant, Self),
		\+ extends_object(Descendant, Self).

	descendant(Self, Descendant) :-
		extends_object(Subclass, Self),
		descendant(Subclass, Descendant).


	descendants(Descendants) :-
		self(Self),
		(	setof(Descendant, descendant(Self, Descendant), Descendants) ->
			true
		;	Descendants = []
		).


:- end_category.
