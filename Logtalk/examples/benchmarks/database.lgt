

:- object(database).

	:- public(db_test_this/0, db_test_self/0, db_test_obj/0).

	:- private(pred_this/0, pred_self/0, pred_obj/0).
	:- dynamic(pred_this/0, pred_self/0, pred_obj/0).

	db_test_this :-
		{repeat(100)},
			assertz(pred_this),
		fail.
	db_test_this :-
		retract(pred_this),
		fail.
	db_test_this.

	db_test_self :-
		{repeat(100)},
			::assertz(pred_self),
		fail.
	db_test_self :-
		::retract(pred_self),
		fail.
	db_test_self.


	db_test_obj :-
		this(This),
		{repeat(100)},
			This::assertz(pred_obj),
		fail.
	db_test_obj :-
		this(This),
		This::retract(pred_obj),
		fail.
	db_test_obj.

:- end_object.
