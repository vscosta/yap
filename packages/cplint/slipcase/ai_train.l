
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

input(projectmember/2).

input(sameperson/2).
input(samecourse/2).
input(sameproject/2).


modeh(*,advisedby(+person,+person)). 

modeb(*,professor(+person)).
modeb(*,student(+person)).

modeb(*,sameperson(+person, -person)). 
modeb(*,sameperson(-person, +person)). 
modeb(*,samecourse(+course, -course)). 
modeb(*,samecourse(-course, +course)). 
modeb(*,sameproject(-project, +project)). 
modeb(*,sameproject(+project, -project)).

modeb(*,publication(+title, -person)).
modeb(*,courselevel(+course, -level)).
modeb(*,hasposition(+person, -position)).
%modeb(*,projectmember(+project, -person)).
modeb(*,projectmember(project119, +person)).
modeb(*,projectmember(project130, +person)).
modeb(*,projectmember(project152, +person)).
modeb(*,projectmember(project62, +person)).
modeb(*,projectmember(project94, +person)).
modeb(*,inphase(+person, -phase)).
modeb(*,tempadvisedby(+person, -person)).
modeb(*,yearsinprogram(+person, -integer)).

modeb(*,publication(-title, +person)).
%modeb(*,courselevel(-course, +level)).
modeb(*,courselevel(+course, level_500)).
modeb(*,courselevel(+course, level_400)).
modeb(*,courselevel(+course, level_300)).
%modeb(*,hasposition(-person, +position)).
modeb(*,hasposition(+person, faculty)).
modeb(*,hasposition(+person, faculty_affiliate)).
modeb(*,hasposition(+person, faculty_adjunct)).
modeb(*,hasposition(+person, faculty_visiting)).
modeb(*,hasposition(+person, faculty_emeritus)).
modeb(*,projectmember(-project, +person)).
%modeb(*,inphase(-person, +phase)).
modeb(*,inphase(+person, post_quals)).
modeb(*,inphase(+person, post_generals)).
modeb(*,inphase(+person, pre_quals)).
modeb(*,tempadvisedby(-person, +person)).
%modeb(*,yearsinprogram(-person, +integer)).
modeb(*,yearsinprogram(+person, year_1)).
modeb(*,yearsinprogram(+person, year_2)).
modeb(*,yearsinprogram(+person, year_3)).
modeb(*,yearsinprogram(+person, year_4)).
modeb(*,yearsinprogram(+person, year_5)).
modeb(*,yearsinprogram(+person, year_6)).
modeb(*,yearsinprogram(+person, year_7)).
modeb(*,yearsinprogram(+person, year_8)).
modeb(*,yearsinprogram(+person, year_9)).
modeb(*,yearsinprogram(+person, year_10)).
modeb(*,yearsinprogram(+person, year_12)).

modeb(*,taughtby(+course, -person, -quarter)).
modeb(*,ta(+course, -person, -quarter)).
modeb(*,taughtby(-course, +person, -quarter)).
modeb(*,ta(-course, +person, -quarter)).

%modeb(*,taughtby(-course, -person, +quarter)).
%modeb(*,ta(-course, -person, +quarter)).

modeb(*,taughtby(+course, +person, -quarter)).
modeb(*,ta(+course, +person, -quarter)).

%modeb(*,taughtby(+course, -person, +quarter)).
%modeb(*,ta(+course, -person, +quarter)).
modeb(*,taughtby(+course, -person, autumn_0001)).
modeb(*,ta(+course, -person, autumn_0001)).
modeb(*,taughtby(+course, -person, autumn_0102)).
modeb(*,ta(+course, -person, autumn_0102)).
modeb(*,taughtby(+course, -person, autumn_0203)).
modeb(*,ta(+course, -person, autumn_0203)).
modeb(*,taughtby(+course, -person, autumn_0304)).
modeb(*,ta(+course, -person, autumn_0304)).
modeb(*,taughtby(+course, -person, spring_0001)).
modeb(*,ta(+course, -person, spring_0001)).
modeb(*,taughtby(+course, -person, spring_0102)).
modeb(*,ta(+course, -person, spring_0102)).
modeb(*,taughtby(+course, -person, spring_0203)).
modeb(*,ta(+course, -person, spring_0203)).
modeb(*,taughtby(+course, -person, spring_0304)).
modeb(*,ta(+course, -person, spring_0304)).
modeb(*,taughtby(+course, -person, summer_0102)).
modeb(*,ta(+course, -person, summer_0102)).
modeb(*,taughtby(+course, -person, summer_0203)).
modeb(*,ta(+course, -person, summer_0203)).
modeb(*,taughtby(+course, -person, winter_0001)).
modeb(*,ta(+course, -person, winter_0001)).
modeb(*,taughtby(+course, -person, winter_0102)).
modeb(*,ta(+course, -person, winter_0102)).
modeb(*,taughtby(+course, -person, winter_0203)).
modeb(*,ta(+course, -person, winter_0203)).
modeb(*,taughtby(+course, -person, winter_0304)).
modeb(*,ta(+course, -person, winter_0304)).

%modeb(*,taughtby(-course, +person, +quarter)).
%modeb(*,ta(-course, +person, +quarter)).
modeb(*,taughtby(-course, +person, autumn_0001)).
modeb(*,ta(-course, +person, autumn_0001)).
modeb(*,taughtby(-course, +person, autumn_0102)).
modeb(*,ta(-course, +person, autumn_0102)).
modeb(*,taughtby(-course, +person, autumn_0203)).
modeb(*,ta(-course, +person, autumn_0203)).
modeb(*,taughtby(-course, +person, autumn_0304)).
modeb(*,ta(-course, +person, autumn_0304)).
modeb(*,taughtby(-course, +person, spring_0001)).
modeb(*,ta(-course, +person, spring_0001)).
modeb(*,taughtby(-course, +person, spring_0102)).
modeb(*,ta(-course, +person, spring_0102)).
modeb(*,taughtby(-course, +person, spring_0203)).
modeb(*,ta(-course, +person, spring_0203)).
modeb(*,taughtby(-course, +person, spring_0304)).
modeb(*,ta(-course, +person, spring_0304)).
modeb(*,taughtby(-course, +person, summer_0102)).
modeb(*,ta(-course, +person, summer_0102)).
modeb(*,taughtby(-course, +person, summer_0203)).
modeb(*,ta(-course, +person, summer_0203)).
modeb(*,taughtby(-course, +person, winter_0001)).
modeb(*,ta(-course, +person, winter_0001)).
modeb(*,taughtby(-course, +person, winter_0102)).
modeb(*,ta(-course, +person, winter_0102)).
modeb(*,taughtby(-course, +person, winter_0203)).
modeb(*,ta(-course, +person, winter_0203)).
modeb(*,taughtby(-course, +person, winter_0304)).
modeb(*,ta(-course, +person, winter_0304)).
