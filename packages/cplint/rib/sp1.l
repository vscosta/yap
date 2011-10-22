output(sameperson/2).

input(gender/2).

input(genre/2).

input(actor/1).

input(director/1).

input(movie/2).

input(workedunder/2).


unseen(sameperson_pos/2).
unseen(sameperson_neg/2).

modeb(1,sameperson(-person, -person)).

modeb(1,sameperson_pos(-person, -person)).

modeb(1,sameperson_neg(-person, -person)).

modeb(1,workedunder(+person,-person)).

modeb(1,movie(-movie,+person)).

modeb(1,gender(+person,-gender)).

modeb(1,genre(+person,-genre)).

modeb(1,actor(+person)).

modeb(1,director(+person)).

:- set(minimal_step,0.005).
:- set(maximal_step,0.1).

