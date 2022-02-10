izar% naiverun
YAP=../../bins/java/yap: Command not found.
TESTF=test.f: Command not found.
TESTN=test.n: Command not found.
BF=advisedby: Command not found.
for: Command not found.
do: Command not found.
i: Undefined variable.
izar% pwd
/u/vitor/Yap/ILP/uw-cse
izar% bash
bash-2.05b$ naiverun
bash-2.05b$ ./naiverun
bash-2.05b$ ./naiverun
bash-2.05b$ ./naiverun
bash-2.05b$ ./naiverun
bash-2.05b$ printenv
bash: printnv: command not found
bash-2.05b$ printenv
MANPATH=/s/man:/usr/man:/usr/X11R6/man:/usr/share/man:/usr/share/man
SSH_AGENT_PID=23503
SUPPORTED=pt_PT.UTF-8:pt_PT:pt:es_ES.UTF-8:es_ES:es:en_US.UTF-8:en_US:en
HOSTNAME=izar.biostat.wisc.edu
HOST=izar.biostat.wisc.edu
SHELL=/bin/csh
TERM=dumb
YAPLIBDIR=/u/ml-group/Yap_Condor/linux-4.5Jan04/vanilla-2GB
GTK_RC_FILES=/etc/gtk/gtkrc:/u/jdavis/.gtkrc-1.2-gnome2
WINDOWID=27693984
LANGVAR=en_US.UTF-8
LOGTALKDIR=/u/vitor/logtalk
GROUP=medinfo
USER=vitor
EMACS=t
LD_LIBRARY_PATH=.:/u/shavlikgroup/Advice_Taking/linux/lib:/u/shavlikgroup/linux/lib:
LS_COLORS=
SSH_AUTH_SOCK=/tmp/ssh-PRk23502/agent.23502
HOSTTYPE=i386-linux
TERMCAP=
USERNAME=jdavis
SESSION_MANAGER=local/izar.biostat.wisc.edu:/tmp/.ICE-unix/23502
LOGTALKHOME=/u/vitor/Yap/work/Logtalk
COLUMNS=80
PATH=/u/vitor/linux/bin:/u/vitor/bin/noarch:/usr/bin:/bin:/sbin:/usr/sbin:/u/vitor/linux/bin:/usr/X11R6/bin:.:/s/bin:/usr/openwin/bin:/usr/local/bin:/u/vitor/bin:/usr/bin:/bin:/usr/X11R6/bin
MAIL=/var/mail/vitor
PWD=/u/vitor/Yap/ILP/uw-cse
YAPBINDIR=/u/ml-group/Yap_Condor/linux-4.5Jan04/vanilla-2GB
XMODIFIERS=@im=none
LANG=en_US.UTF-8
LAMHELPFILE=/etc/lam/lam-helpfile
GDMSESSION=GNOME
SSH_ASKPASS=/usr/libexec/openssh/gnome-ssh-askpass
SHLVL=5
HOME=/u/vitor
OSTYPE=linux
GNOME_DESKTOP_SESSION_ID=Default
VENDOR=intel
MACHTYPE=i386
LOGNAME=vitor
LOGTALKUSER=/u/vitor/Yap/work/Logtalk
LESSOPEN=|/usr/bin/lesspipe.sh %s
DISPLAY=:0.0
G_BROKEN_FILENAMES=1
COLORTERM=gnome-terminal
XAUTHORITY=/u/vitor/.xauthsqVi1j
_=/usr/bin/printenv
bash-2.05b$ export YAPLIBDIR=/u/vitor/Yap/bins/java/lib
bash-2.05b$ ./naiverun
bash-2.05b$ export YAPLIBDIR=/u/vitor/Yap/bins/java
bash-2.05b$ ./naiverun
bash-2.05b$ ls /u/vitor/Yap/bins/java/lib
libYap.a  libYap.so  Yap
bash-2.05b$ ls /u/vitor/Yap/bins/java/lib/Yap
jpl.so	   regcomp.so	regexec.so  regfree.so	sys.so	    yap_tries.so
random.so  regerror.so	regexp.so   startup	yap2swi.so
bash-2.05b$ export YAPLIBDIR=/u/vitor/Yap/bins/java/lib/Yap
bash-2.05b$ ./naiverun
You have new mail in /var/mail/vitor
bash-2.05b$ grep "Exception" LOGNAIVE.4

bash-2.05b$ bash: rep: command not found
bash-2.05b$ grep "Exception" LOGNAIVE.4
java.lang.ArrayIndexOutOfBoundsException: 628
java.lang.ArrayIndexOutOfBoundsException: 628
java.lang.ArrayIndexOutOfBoundsException: 628
java.lang.ArrayIndexOutOfBoundsException: 628
java.lang.ArrayIndexOutOfBoundsException: 628
java.lang.ArrayIndexOutOfBoundsException: 628
java.lang.ArrayIndexOutOfBoundsException: 628
java.lang.ArrayIndexOutOfBoundsException: 628
java.lang.ArrayIndexOutOfBoundsException: 628
java.lang.ArrayIndexOutOfBoundsException: 628
java.lang.ArrayIndexOutOfBoundsException: 628
java.lang.ArrayIndexOutOfBoundsException: 628
bash-2.05b$ ls 
advisedby0.trace	  LOGNAIVE.4		prun2.~1~
advisedby1.trace	  LOGNOPATH.0		prun2.~2~
advisedby2.trace	  LOGNOPATH.1		prun.~3~
advisedby3.trace	  LOGNOPATH.2		prun.~4~
advisedby4.trace	  LOGNOPATH.3		Ptest.f
advisedby.b		  LOGNOPATH.4		Ptest.n
advisedby.b.~1~		  LOGPATH.0		Ptrain.f
advisedby.b.~10~	  LOG_PATH.3		Ptrain.n
advisedby.b.~11~	  LOG_PATH.4		Ptune.f
advisedby.b.~12~	  LOGTAN.0		Ptune.n
advisedby.b.~13~	  LOGTAN.1		readme.txt
advisedby.b.~14~	  LOGTAN.2		Rs
advisedby.b.~2~		  LOGTAN.3		run
advisedby.b.~3~		  LOGTAN.4		run.~1~
advisedby.b.~4~		  misc.preds		run.~10~
advisedby.b.~5~		  misc.yap		run.~11~
advisedby.f		  misc.yap.~1~		run.~12~
advisedby.n		  misc.yap.~2~		run.~13~
ai.b			  misc.yap.~3~		run.~14~
ai.f			  nadvisedby0.trace	run.~2~
AI.log			  nadvisedby1.trace	run.~3~
ai.n			  nadvisedby2.trace	run.~4~
ai.preds		  nadvisedby3.trace	run.~5~
ai.yap			  nadvisedby4.trace	sets
ai.yap.~1~		  naiverun		sets0
ai.yap.~2~		  #naiverun#		#sets0#
classtypes.txt		  naiverun.~1~		sets0.~1~
core.25033		  naiverun.~2~		sets.~1~
core.27809		  naiverun.~3~		sets.~2~
core.30172		  nb-advisedby.f	Ss
core.32678		  nb-advisedby.n	systems.preds
fs			  nb-test.f		systems.yap
gen_exs.yap		  nb-test.n		systems.yap.~1~
gen_exs.yap.~1~		  nb-train.f		systems.yap.~2~
gen_exs.yap.~2~		  nb-train.n		systems.yap.~3~
graphics.preds		  nb-tune.f		tadvisedby0.trace
graphics.yap		  nb-tune.n		tadvisedby1.trace
graphics.yap.~1~	  netscript		tadvisedby2.trace
graphics.yap.~2~	  netscript.~1~		tadvisedby3.trace
graphics.yap.~3~	  netscript.~2~		tadvisedby4.trace
graphics.yap.~4~	  nohup.out		tanrun
interesting_clauses	  padvisedby0.trace	tanrun.~1~
interesting_clauses0.yap  padvisedby1.trace	test_ai.f
interesting_clauses1.yap  padvisedby2.trace	test_ai.n
interesting_clauses2.yap  Padvisedby.b		test.f
interesting_clauses3.yap  Padvisedby.f		test.n
interesting_clauses4.yap  Padvisedby.n		test.out
kb.yap			  Padvisedby.trace	theory.preds
kb.yap.~1~		  PR_1			theory.yap
language.preds		  PR_CK			theory.yap.~1~
language.yap		  probabilities-n0.yap	theory.yap.~2~
language.yap.~1~	  probabilities-n1.yap	theory.yap.~3~
language.yap.~2~	  probabilities-n2.yap	times.yap
language.yap.~3~	  probabilities-n3.yap	times.yap.~1~
learning_curve		  probabilities-n4.yap	times.yap.~2~
LOG.0			  probabilities-t0.yap	train_ai.f
LOG.1			  probabilities-t1.yap	train_ai.n
LOG.2			  probabilities-t2.yap	train.f
LOG.3			  probabilities-t3.yap	train.n
LOG.4			  probabilities-t4.yap	tune.f
LOGNAIVE.0		  prun			tune.n
LOGNAIVE.1		  prun.~1~		unknown.preds
LOGNAIVE.2		  prun2			unknown.yap
LOGNAIVE.3		  prun.~2~		unknown.yap.~1~
bash-2.05b$ more probabilities-n0.yap 
neg advisedby(person191_1,person378_1) 0.0127126560317995
neg advisedby(person397_1,person378_1) 0.0546787930963581
neg advisedby(person138_1,person378_1) 0.000123070944853218
neg advisedby(person303_1,person378_1) 0.036510708177034
neg advisedby(person77_1,person378_1) 0.000123070944853218
neg advisedby(person141_1,person378_1) 0.189970427112161
neg advisedby(person383_1,person378_1) 0.0127126560317995
neg advisedby(person422_1,person378_1) 0.0127126560317995
neg advisedby(person390_1,person378_1) 0.036510708177034
neg advisedby(person288_1,person378_1) 0.189970427112161
neg advisedby(person159_1,person378_1) 0.0204352935568096
neg advisedby(person172_1,person378_1) 0.0127126560317995
neg advisedby(person226_1,person378_1) 0.0127126560317995
neg advisedby(person242_1,person378_1) 0.275342357461599
neg advisedby(person416_1,person378_1) 0.0127126560317995
neg advisedby(person348_1,person378_1) 0.0204352935568096
neg advisedby(person278_1,person378_1) 0.0127126560317995
neg advisedby(person6_1,person378_1) 0.0127126560317995
neg advisedby(person75_1,person378_1) 0.189970427112161
neg advisedby(person249_1,person378_1) 0.189970427112161
neg advisedby(person68_1,person378_1) 0.275342357461599
neg advisedby(person205_1,person378_1) 0.0127126560317995

neg advisedby(person182_1,person378_1) 0.0127126560317995

bash-2.05b$ ls
advisedby0.trace	  LOGNAIVE.4		prun2.~1~
advisedby1.trace	  LOGNOPATH.0		prun2.~2~
advisedby2.trace	  LOGNOPATH.1		prun.~3~
advisedby3.trace	  LOGNOPATH.2		prun.~4~
advisedby4.trace	  LOGNOPATH.3		Ptest.f
advisedby.b		  LOGNOPATH.4		Ptest.n
advisedby.b.~1~		  LOGPATH.0		Ptrain.f
advisedby.b.~10~	  LOG_PATH.3		Ptrain.n
advisedby.b.~11~	  LOG_PATH.4		Ptune.f
advisedby.b.~12~	  LOGTAN.0		Ptune.n
advisedby.b.~13~	  LOGTAN.1		readme.txt
advisedby.b.~14~	  LOGTAN.2		Rs
advisedby.b.~2~		  LOGTAN.3		run
advisedby.b.~3~		  LOGTAN.4		run.~1~
advisedby.b.~4~		  misc.preds		run.~10~
advisedby.b.~5~		  misc.yap		run.~11~
advisedby.f		  misc.yap.~1~		run.~12~
advisedby.n		  misc.yap.~2~		run.~13~
ai.b			  misc.yap.~3~		run.~14~
ai.f			  nadvisedby0.trace	run.~2~
AI.log			  nadvisedby1.trace	run.~3~
ai.n			  nadvisedby2.trace	run.~4~
ai.preds		  nadvisedby3.trace	run.~5~
ai.yap			  nadvisedby4.trace	sets
ai.yap.~1~		  naiverun		sets0
ai.yap.~2~		  #naiverun#		#sets0#
classtypes.txt		  naiverun.~1~		sets0.~1~
core.25033		  naiverun.~2~		sets.~1~
core.27809		  naiverun.~3~		sets.~2~
core.30172		  nb-advisedby.f	Ss
core.32678		  nb-advisedby.n	systems.preds
fs			  nb-test.f		systems.yap
gen_exs.yap		  nb-test.n		systems.yap.~1~
gen_exs.yap.~1~		  nb-train.f		systems.yap.~2~
gen_exs.yap.~2~		  nb-train.n		systems.yap.~3~
graphics.preds		  nb-tune.f		tadvisedby0.trace
graphics.yap		  nb-tune.n		tadvisedby1.trace
graphics.yap.~1~	  netscript		tadvisedby2.trace
graphics.yap.~2~	  netscript.~1~		tadvisedby3.trace
graphics.yap.~3~	  netscript.~2~		tadvisedby4.trace
graphics.yap.~4~	  nohup.out		tanrun
interesting_clauses	  padvisedby0.trace	tanrun.~1~
interesting_clauses0.yap  padvisedby1.trace	test_ai.f
interesting_clauses1.yap  padvisedby2.trace	test_ai.n
interesting_clauses2.yap  Padvisedby.b		test.f
interesting_clauses3.yap  Padvisedby.f		test.n
interesting_clauses4.yap  Padvisedby.n		test.out
kb.yap			  Padvisedby.trace	theory.preds
kb.yap.~1~		  PR_1			theory.yap
language.preds		  PR_CK			theory.yap.~1~
language.yap		  probabilities-n0.yap	theory.yap.~2~
language.yap.~1~	  probabilities-n1.yap	theory.yap.~3~
language.yap.~2~	  probabilities-n2.yap	times.yap
language.yap.~3~	  probabilities-n3.yap	times.yap.~1~
learning_curve		  probabilities-n4.yap	times.yap.~2~
LOG.0			  probabilities-t0.yap	train_ai.f
LOG.1			  probabilities-t1.yap	train_ai.n
LOG.2			  probabilities-t2.yap	train.f
LOG.3			  probabilities-t3.yap	train.n
LOG.4			  probabilities-t4.yap	tune.f
LOGNAIVE.0		  prun			tune.n
LOGNAIVE.1		  prun.~1~		unknown.preds
LOGNAIVE.2		  prun2			unknown.yap
LOGNAIVE.3		  prun.~2~		unknown.yap.~1~
bash-2.05b$ ./naiverun
./naiverun: line 42: 25537 Segmentation fault      (core dumped) $YAP  >LOGNAIVE.$i 2>&1 <<EOF
['../AlephUW/aleph.yap'].
%assert(fold($i)).
[times].
read_all('$BF').
set(recordfile,'nadvisedby$i.trace').
set(nodes,10000).
set(verbosity,0).
set(test_pos,'$TESTF').
set(test_neg,'$TESTN').
vsc_set(bnet_dump_file('probabilities-n$i.yap')).
%vsc_set(bnet_min_recall_in_area(0.5)).
%vsc_set(bnet_scoring_method(0)).
%vsc_set(bnet_start_seeds(200)).
vsc_set(bnet_min_recall_in_area(0.0)).
statistics(runtime,[T,_]), time($i,X), Z is X // 1000,induce_bnet_timed(Z), statistics(runtime,[Tf,_]), D is Tf-T, format('ILP run took ~d msec.~n',[D]).
EOF

You have new mail in /var/mail/vitor
bash-2.05b$ ./naiverun
bash-2.05b$ ./naiverun
bash-2.05b$ ./naiverun
bash-2.05b$ ./naiverun
bash-2.05b$ ./naiverun
You have new mail in /var/mail/vitor
bash-2.05b$pwd
/u/vitor/Yap/ILP/uw-cse
bash-2.05b$ 
bash-2.05b$ ls
0.0			  learning_curve	prune_bitmap.yap.~2~
0.2			  LOG.0			Ptest.f
advisedby0.trace	  LOG.1			Ptest.n
advisedby1.trace	  LOG.2			Ptrain.f
advisedby2.trace	  LOG.3			Ptrain.n
advisedby3.trace	  LOG.4			Ptune.f
advisedby4.trace	  LOGALEPH.0		Ptune.n
advisedby.b		  LOGALEPH.1		readme.txt
advisedby.b.~1~		  LOGALEPH.2		Rs
advisedby.b.~10~	  LOGALEPH.3		run
advisedby.b.~11~	  LOGALEPH.4		run.~1~
advisedby.b.~12~	  #LOGNAIVE.0#		run.~11~
advisedby.b.~13~	  LOGNOPATH.0		run.~12~
advisedby.b.~14~	  LOGNOPATH.1		run.~13~
advisedby.b.~2~		  LOGNOPATH.2		run.~14~
advisedby.b.~3~		  LOGNOPATH.3		run.~15~
advisedby.b.~4~		  LOGNOPATH.4		run.~2~
advisedby.b.~5~		  LOGPATH.0		run.~3~
advisedby.f		  LOG_PATH.3		run.~4~
advisedby.n		  LOG_PATH.4		run.~5~
ai.b			  LOGTAN.0		script.~1~
ai.f			  LOGTAN.1		script.~2~
AI.log			  LOGTAN.2		script.~3~
ai.n			  LOGTAN.3		script.~4~
ai.preds		  LOGTAN.4		script.~5~
ai.yap			  misc.preds		sets
ai.yap.~1~		  misc.yap		sets0
ai.yap.~2~		  misc.yap.~1~		#sets0#
aleph_advisedby0.trace	  misc.yap.~2~		sets0.~1~
aleph_advisedby1.trace	  misc.yap.~3~		sets.~1~
aleph_advisedby2.trace	  naiverun		sets.~2~
aleph_advisedby3.trace	  naiverun.~1~		Ss
aleph_advisedby4.trace	  naiverun.~2~		systems.preds
bmaps			  naiverun.~3~		systems.yap
classtypes.txt		  naiverun.~4~		systems.yap.~1~
cls0.yap		  naiverun.~5~		systems.yap.~2~
cls1.yap		  nb-advisedby.f	systems.yap.~3~
cls2.yap		  nb-advisedby.n	tadvisedby0.trace
cls3.yap		  nb-test.f		tadvisedby1.trace
cls4.yap		  nb-test.n		tadvisedby2.trace
fetch_clauses		  nb-train.f		tadvisedby3.trace
finish			  nb-train.n		tadvisedby4.trace
finish.~1~		  nb-tune.f		tanrun
finish.~2~		  nb-tune.n		tanrun.~1~
finish.~3~		  netscript		tanrun.~2~
fs			  netscript.~1~		tanrun.~3~
gen_bitmap.yap		  netscript.~2~		tanrun.~4~
gen_bitmap.yap.~1~	  netscript.~3~		test_ai.f
gen_bitmap.yap.~2~	  netscript.~4~		test_ai.n
gen_exs.yap		  netscript.~5~		test.f
gen_exs.yap.~1~		  nohup.out		test.n
gen_exs.yap.~2~		  padvisedby0.trace	test.out
graphics.preds		  padvisedby1.trace	theory.preds
graphics.yap		  padvisedby2.trace	theory.yap
graphics.yap.~1~	  Padvisedby.b		theory.yap.~1~
graphics.yap.~2~	  Padvisedby.f		theory.yap.~2~
graphics.yap.~3~	  Padvisedby.n		theory.yap.~3~
graphics.yap.~4~	  Padvisedby.trace	times.yap
i0.yap			  PR_1			times.yap.~1~
i1.yap			  PR_CK			times.yap.~2~
i2.yap			  probabilities-t0.yap	times.yap.~3~
i3.yap			  probabilities-t1.yap	times.yap.~4~
i4.yap			  probabilities-t2.yap	times.yap.~5~
interesting_clauses	  probabilities-t3.yap	train_ai.f
interesting_clauses0.yap  probabilities-t4.yap	train_ai.n
interesting_clauses1.yap  prun			train.f
interesting_clauses2.yap  #prun#		train.n
interesting_clauses3.yap  prun.~1~		tune.f
interesting_clauses4.yap  prun2			tune.n
kb.yap			  prun.~2~		unknown.preds
kb.yap.~1~		  prun2.~1~		unknown.yap
language.preds		  prun2.~2~		unknown.yap.~1~
language.yap		  prun.~3~		use_background.yap
language.yap.~1~	  prun.~4~		use_background.yap.~1~
language.yap.~2~	  prune_bitmap.yap	use_background.yap.~2~
language.yap.~3~	  prune_bitmap.yap.~1~
bash-2.05b$ exit
exit
izar% exit
izar% exit

Process shell finished
