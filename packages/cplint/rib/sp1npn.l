output(sameperson/2).

input(gender/2).

input(genre/2).

input(actor/1).

input(director/1).

input(movie/2).

input(workedunder/2).

modeb(1,sameperson(-person, -person)).

modeb(1,workedunder(+person,-person)).

modeb(1,movie(-movie,+person)).

modeb(1,gender(+person,-gender)).

modeb(1,genre(+person,-genre)).

modeb(1,actor(+person)).

modeb(1,director(+person)).

:- set(maximal_step,0.1). 
:- set(minimal_step,0.005).
