:- op(600,xfy,::).
:- op(600,fy,::).
:- op(600,fx,^^).
:- dynamic log1__ddcl/4.
:- dynamic log1__ddef/5.
log1_(log1__dcl,log1__def,log1__super,log1__sdcl,log1__sdef,log1__ddcl,log1__ddef).
log1__dcl(_9931,_9932,_9933,_9934):-fail.
log1__dcl(_9931,_9932,_9933,_9934,log(_9938),log(_9938)):-log1__dcl(_9931,_9932,_9933,_9934).
log1__dcl(_9931,_9932,_9933,_9934,log(_9938),log(_9938)):-log1__ddcl(_9931,_9932,_9933,_9934).
log1__dcl(_9931,_9932,_9933,_9934,log(_9938),_9936):-symdiffp0__dcl(_9931,_9932,_9933,_9934,_9936).
log1__def(diff(_9934),_9929,_9930,_9931,log1_diff1(_9934,_9929,_9930,_9931)).
log1__def(diff(_9934,_9935),_9929,_9930,_9931,log1_diff2(_9934,_9935,_9929,_9930,_9931)).
log1__def(simplify(_9934),_9929,_9930,_9931,log1_simplify1(_9934,_9929,_9930,_9931)).
log1__def(simplify(_9934,_9935),_9929,_9930,_9931,log1_simplify2(_9934,_9935,_9929,_9930,_9931)).
log1__def(_9931,_9932,_9933,_9934,_9935,log(_9938)):-log1__def(_9931,_9932,_9933,_9934,_9935).
log1__def(_9931,_9932,_9933,_9934,_9935,log(_9938)):-log1__ddef(_9931,_9932,_9933,_9934,_9935).
log1__super(_9931,_9932,_9933,_9934,_9935,_9936):-fail.
log1_diff1(_9931,_9932,log(_9936),_9934):-once(log1_diff2(_9936,_9931,_9932,log(_9936),_9934)).
log1_diff2(_9931,0,_9933,_9934,_9935):-integer(_9931).
log1_diff2(_9931,_9937*_9931** -1,_9933,_9934,_9935):-lgt_send_to_object(_9931,diff(_9937),_9934).
log1_simplify1(_9931,_9932,log(_9936),_9934):-once(log1_simplify2(_9936,_9931,_9932,log(_9936),_9934)).
log1_simplify2(1,0,_9930,_9931,_9932).
log1_simplify2(_9931,_9932,_9933,_9934,_9935):-integer(_9931),_9932 is log(_9931).
log1_simplify2(_9928,_9928,_9930,_9931,_9932).
:- initialization((lgt_assert_relation_clauses([lgt_current_object_(log(_9945),log1_,log1__dcl,log1__def,log1__super),lgt_implements_protocol_(log(_9953),symdiffp,public)]))).
