output(advisedby/2).

input(student/1).
input(professor/1).
input(inphase/2).
input(hasposition/2).
input(publication/2).
input(yearsinprogram/2).
input(taughtby/3).
input(ta/3).
input(courselevel/2).
input(tempadvisedby/2).


determination(professor/1,student/1).
determination(professor/1,hasposition/2).
determination(professor/1,publication/2).
determination(professor/1,courselevel/2).
determination(professor/1,inphase/2).
determination(professor/1,advisedby/2).
determination(professor/1,tempadvisedby/2).
determination(professor/1,yearsinprogram/2).
determination(professor/1,taughtby/3).
determination(professor/1,ta/3).

determination(student/1,professor/1).
determination(student/1,hasposition/2).
determination(student/1,publication/2).
determination(student/1,courselevel/2).
determination(student/1,inphase/2).
determination(student/1,advisedby/2).
determination(student/1,tempadvisedby/2).
determination(student/1,yearsinprogram/2).
determination(student/1,taughtby/3).
determination(student/1,ta/3).

determination(hasposition/2,professor/1).
determination(hasposition/2,student/1).
determination(hasposition/2,publication/2).
determination(hasposition/2,courselevel/2).
determination(hasposition/2,inphase/2).
determination(hasposition/2,advisedby/2).
determination(hasposition/2,tempadvisedby/2).
determination(hasposition/2,yearsinprogram/2).
determination(hasposition/2,taughtby/3).
determination(hasposition/2,ta/3).

determination(inphase/2,professor/1).
determination(inphase/2,student/1).
determination(inphase/2,hasposition/2).
determination(inphase/2,publication/2).
determination(inphase/2,courselevel/2).
determination(inphase/2,advisedby/2).
determination(inphase/2,tempadvisedby/2).
determination(inphase/2,yearsinprogram/2).
determination(inphase/2,taughtby/3).
determination(inphase/2,ta/3).

determination(taughtby/3,professor/1).
determination(taughtby/3,student/1).
determination(taughtby/3,hasposition/2).
determination(taughtby/3,publication/2).
determination(taughtby/3,courselevel/2).
determination(taughtby/3,inphase/2).
determination(taughtby/3,advisedby/2).
determination(taughtby/3,tempadvisedby/2).
determination(taughtby/3,yearsinprogram/2).
determination(taughtby/3,ta/3).

determination(advisedby/2,professor/1).
determination(advisedby/2,student/1).
determination(advisedby/2,hasposition/2).
determination(advisedby/2,publication/2).
determination(advisedby/2,courselevel/2).
determination(advisedby/2,inphase/2).
determination(advisedby/2,yearsinprogram/2).
determination(advisedby/2,taughtby/3).
determination(advisedby/2,ta/3).

determination(tempadvisedby/2,professor/1).
determination(tempadvisedby/2,student/1).
determination(tempadvisedby/2,hasposition/2).
determination(tempadvisedby/2,publication/2).
determination(tempadvisedby/2,courselevel/2).
determination(tempadvisedby/2,inphase/2).
determination(tempadvisedby/2,yearsinprogram/2).
determination(tempadvisedby/2,taughtby/3).
determination(tempadvisedby/2,ta/3).

determination(ta/3,professor/1).
determination(ta/3,student/1).
determination(ta/3,hasposition/2).
determination(ta/3,publication/2).
determination(ta/3,courselevel/2).
determination(ta/3,inphase/2).
determination(ta/3,advisedby/2).
determination(ta/3,tempadvisedby/2).
determination(ta/3,yearsinprogram/2).
determination(ta/3,taughtby/3).

determination(yearsinprogram/2,professor/1).
determination(yearsinprogram/2,student/1).
determination(yearsinprogram/2,hasposition/2).
determination(yearsinprogram/2,publication/2).
determination(yearsinprogram/2,courselevel/2).
determination(yearsinprogram/2,inphase/2).
determination(yearsinprogram/2,advisedby/2).
determination(yearsinprogram/2,tempadvisedby/2).
determination(yearsinprogram/2,taughtby/3).
determination(yearsinprogram/2,ta/3).

determination(courselevel/2,professor/1).
determination(courselevel/2,student/1).
determination(courselevel/2,hasposition/2).
determination(courselevel/2,publication/2).
determination(courselevel/2,inphase/2).
determination(courselevel/2,advisedby/2).
determination(courselevel/2,tempadvisedby/2).
determination(courselevel/2,yearsinprogram/2).
determination(courselevel/2,taughtby/3).
determination(courselevel/2,ta/3).

determination(publication/2,professor/1).
determination(publication/2,student/1).
determination(publication/2,hasposition/2).
determination(publication/2,courselevel/2).
determination(publication/2,inphase/2).
determination(publication/2,advisedby/2).
determination(publication/2,tempadvisedby/2).
determination(publication/2,yearsinprogram/2).
determination(publication/2,taughtby/3).
determination(publication/2,ta/3).


modeh(*,advisedby(+stu,+pro)). 
%modeh(*,professor(+pro)).
%modeh(*,student(+stu)).
%modeh(*,hasposition(+person, +position)).
%modeh(*,hasposition(+pro, faculty)).
%modeh(*,hasposition(+pro, faculty_affiliate)).
%modeh(*,hasposition(+pro, faculty_adjunct)).
%modeh(*,hasposition(+pro, faculty_visiting)).
%modeh(*,hasposition(+pro, faculty_emeritus)).

%modeh(*,inphase(+person, +phase)).
%modeh(*,inphase(+person, post_quals)).
%modeh(*,inphase(+person, post_generals)).
%modeh(*,inphase(+person, pre_quals)).

%modeh(*,taughtby(+course, +person, +quarter)). 
%modeh(*,tempadvisedby(+person,+person)). 
%modeh(*,ta(+course,+person,+quarter)).
%modeh(*,courselevel(+course,+level)).
%modeh(*,yearsinprogram(+person,+year)).
%modeh(*,publication(+pub,+person)).


modeb(*,publication(-title, +stu)).
modeb(*,publication(+title, +stu)).
modeb(*,publication(-title, +pro)).
modeb(*,publication(+title, +pro)).
modeb(*,student(+stu)).
modeb(*,professor(+pro)).
modeb(*,taughtby(-course, +pro, -quarter)).
%modeb(*,taughtby(+course, -person, -quarter)).
%modeb(*,taughtby(+course, +person, -quarter)).
modeb(*,taughtby(+course, +pro, +quarter)).
modeb(*,ta(-course, +stu, -quarter)).
%modeb(*,ta(+course, -person, -quarter)).
%modeb(*,ta(+course, +person, -quarter)).
modeb(*,ta(+course, +stu, +quarter)).

%modeb(*,ta(+course, +person, +quarter)).
%modeb(*,ta(-course, +person, -quarter)).
%modeb(*,hasposition(+person, -position)).
%modeb(*,hasposition(+pro, faculty)).
%modeb(*,hasposition(+pro, faculty_affiliate)).
%modeb(*,hasposition(+pro, faculty_adjunct)).
%modeb(*,hasposition(+pro, faculty_visiting)).
%modeb(*,hasposition(+pro, faculty_emeritus)).

%modeb(*,inphase(+person, -phase)).
%modeb(*,inphase(+stu, post_quals)).
%modeb(*,inphase(+stu, post_generals)).
%modeb(*,inphase(+stu, pre_quals)).

%modeb(*,tempadvisedby(+stu, -pro)).
%modeb(*,tempadvisedby(-stu, +pro)).

%modeb(*,yearsinprogram(+person, -year)).
%modeb(*,yearsinprogram(+stu, year_1)).
%modeb(*,yearsinprogram(+stu, year_2)).
%modeb(*,yearsinprogram(+stu, year_3)).
%modeb(*,yearsinprogram(+stu, year_4)).
%modeb(*,yearsinprogram(+stu, year_5)).
%modeb(*,yearsinprogram(+stu, year_6)).
%modeb(*,yearsinprogram(+stu, year_7)).
%modeb(*,yearsinprogram(+stu, year_8)).
%modeb(*,yearsinprogram(+stu, year_9)).
%modeb(*,yearsinprogram(+stu, year_10)).
%modeb(*,yearsinprogram(+stu, year_12)).

%modeb(*,courselevel(+course, -level)).

banned([advisedby(A,B)],[publication(C,B),publication(D,B)]).
banned([advisedby(A,B)],[publication(C,A),publication(D,A)]).
banned([advisedby(A,B)],[publication(C,A),publication(D,B), C \== D]).

%lookahead(ta(_A,_B,_C),[taughtby(_A,_D,_C)]).
%lookahead(publication(_A,_B),[publication(_A,_C), professor(_B),student(_C)]). 
