
:- category(proto_hierarchy,
	implements(proto_hierarchyp)).


	:- info([
		version is 1.0,
		authors is 'Paulo Moura',
		date is 2000/7/24,
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
		findall(Leaf, leaf(Self, Leaf), Leaves).


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
		findall(Descendant, descendant(Self, Descendant), Descendants).


:- end_category.
