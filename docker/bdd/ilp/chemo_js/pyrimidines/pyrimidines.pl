%%% Learning Bias %%%

:- set(i, 2).
%:- set(language,3).
%:- set(language_init,3).
:- set(star_default_recall, 5).
:- set(clause_length,5).
%:- set(heuristic, acc).
%:- set(heuristic, l).
%:- set(samplesize,50).
%:- set(noise,0.25).
%:- set(mincover,75).
:- set(maximum_singletons_in_clause, 4).
%:- set(sample, 0.4).
:- set(nodes, 3000).
:- set(cross_validation_folds,1).
%:- set(auto_settings,lazy).
%:- set(negseval,lazy).
%:- set(alleval,lazy).

:- [examples_train].
%------------------------------------------------------------------

/*
:- determination(great/2,flex/2).
:- determination(great/2,flex0/1).
:- determination(great/2,flex0x/1).
:- determination(great/2,flex1/1).
:- determination(great/2,flex1x/1).
:- determination(great/2,flex2/1).
:- determination(great/2,flex2x/1).
:- determination(great/2,flex3/1).
:- determination(great/2,flex3x/1).
:- determination(great/2,flex4/1).
:- determination(great/2,flex4x/1).
:- determination(great/2,flex6/1).
:- determination(great/2,flex6x/1).
:- determination(great/2,flex7/1).
:- determination(great/2,flex7x/1).
:- determination(great/2,flex8/1).
:- determination(great/2,flex8x/1).
:- determination(great/2,great0_flex/1).
:- determination(great/2,great0_h_acc/1).
:- determination(great/2,great0_h_don/1).
:- determination(great/2,great0_pi_acc/1).
:- determination(great/2,great0_pi_don/1).
:- determination(great/2,great0_polar/1).
:- determination(great/2,great0_polari/1).
:- determination(great/2,great0_sigma/1).
:- determination(great/2,great0_size/1).
:- determination(great/2,great1_flex/1).
:- determination(great/2,great1_h_acc/1).
:- determination(great/2,great1_h_don/1).
:- determination(great/2,great1_pi_acc/1).
:- determination(great/2,great1_pi_don/1).
:- determination(great/2,great1_polar/1).
:- determination(great/2,great1_polari/1).
:- determination(great/2,great1_sigma/1).
:- determination(great/2,great1_size/1).
:- determination(great/2,great2_flex/1).
:- determination(great/2,great2_h_acc/1).
:- determination(great/2,great2_h_don/1).
:- determination(great/2,great2_pi_acc/1).
:- determination(great/2,great2_pi_don/1).
:- determination(great/2,great2_polar/1).
:- determination(great/2,great2_polari/1).
:- determination(great/2,great2_sigma/1).
:- determination(great/2,great2_size/1).
:- determination(great/2,great3_flex/1).
:- determination(great/2,great3_h_acc/1).
:- determination(great/2,great3_h_don/1).
:- determination(great/2,great3_pi_acc/1).
:- determination(great/2,great3_pi_don/1).
:- determination(great/2,great3_polar/1).
:- determination(great/2,great3_polari/1).
:- determination(great/2,great3_sigma/1).
:- determination(great/2,great3_size/1).
:- determination(great/2,great4_flex/1).
:- determination(great/2,great4_h_acc/1).
:- determination(great/2,great4_h_don/1).
:- determination(great/2,great4_pi_acc/1).
:- determination(great/2,great4_pi_don/1).
:- determination(great/2,great4_polar/1).
:- determination(great/2,great4_polari/1).
:- determination(great/2,great4_sigma/1).
:- determination(great/2,great4_size/1).
:- determination(great/2,great5_flex/1).
:- determination(great/2,great5_h_acc/1).
:- determination(great/2,great5_h_don/1).
:- determination(great/2,great5_pi_acc/1).
:- determination(great/2,great5_pi_don/1).
:- determination(great/2,great5_polar/1).
:- determination(great/2,great5_polari/1).
:- determination(great/2,great5_sigma/1).
:- determination(great/2,great5_size/1).
:- determination(great/2,great6_flex/1).
:- determination(great/2,great6_h_acc/1).
:- determination(great/2,great6_h_don/1).
:- determination(great/2,great6_pi_acc/1).
:- determination(great/2,great6_pi_don/1).
:- determination(great/2,great6_polar/1).
:- determination(great/2,great6_polari/1).
:- determination(great/2,great6_sigma/1).
:- determination(great/2,great6_size/1).
:- determination(great/2,great7_flex/1).
:- determination(great/2,great7_h_acc/1).
:- determination(great/2,great7_h_don/1).
:- determination(great/2,great7_pi_acc/1).
:- determination(great/2,great7_pi_don/1).
:- determination(great/2,great7_polar/1).
:- determination(great/2,great7_polari/1).
:- determination(great/2,great7_sigma/1).
:- determination(great/2,great7_size/1).
:- determination(great/2,great_flex/2).
:- determination(great/2,great_h_acc/2).
:- determination(great/2,great_h_don/2).
:- determination(great/2,great_pi_acc/2).
:- determination(great/2,great_pi_don/2).
:- determination(great/2,great_polar/2).
:- determination(great/2,great_polari/2).
:- determination(great/2,great_sigma/2).
:- determination(great/2,great_size/2).
:- determination(great/2,h_acceptor/2).
:- determination(great/2,h_acceptor0/1).
:- determination(great/2,h_acceptor0x/1).
:- determination(great/2,h_acceptor1/1).
:- determination(great/2,h_acceptor1x/1).
:- determination(great/2,h_acceptor2/1).
:- determination(great/2,h_acceptor2x/1).
:- determination(great/2,h_doner/2).
:- determination(great/2,h_doner0/1).
:- determination(great/2,h_doner0x/1).
:- determination(great/2,h_doner1/1).
:- determination(great/2,h_doner1x/1).
:- determination(great/2,h_doner2/1).
:- determination(great/2,h_doner2x/1).
:- determination(great/2,less2_flex/1).
:- determination(great/2,less2_h_acc/1).
:- determination(great/2,less2_h_don/1).
:- determination(great/2,less2_pi_acc/1).
:- determination(great/2,less2_pi_don/1).
:- determination(great/2,less2_polar/1).
:- determination(great/2,less2_polari/1).
:- determination(great/2,less2_sigma/1).
:- determination(great/2,less2_size/1).
:- determination(great/2,less3_flex/1).
:- determination(great/2,less3_h_acc/1).
:- determination(great/2,less3_h_don/1).
:- determination(great/2,less3_pi_acc/1).
:- determination(great/2,less3_pi_don/1).
:- determination(great/2,less3_polar/1).
:- determination(great/2,less3_polari/1).
:- determination(great/2,less3_sigma/1).
:- determination(great/2,less3_size/1).
:- determination(great/2,less4_flex/1).
:- determination(great/2,less4_h_acc/1).
:- determination(great/2,less4_h_don/1).
:- determination(great/2,less4_pi_acc/1).
:- determination(great/2,less4_pi_don/1).
:- determination(great/2,less4_polar/1).
:- determination(great/2,less4_polari/1).
:- determination(great/2,less4_sigma/1).
:- determination(great/2,less4_size/1).
:- determination(great/2,less5_flex/1).
:- determination(great/2,less5_h_acc/1).
:- determination(great/2,less5_h_don/1).
:- determination(great/2,less5_pi_acc/1).
:- determination(great/2,less5_pi_don/1).
:- determination(great/2,less5_polar/1).
:- determination(great/2,less5_polari/1).
:- determination(great/2,less5_sigma/1).
:- determination(great/2,less5_size/1).
:- determination(great/2,less6_flex/1).
:- determination(great/2,less6_h_acc/1).
:- determination(great/2,less6_h_don/1).
:- determination(great/2,less6_pi_acc/1).
:- determination(great/2,less6_pi_don/1).
:- determination(great/2,less6_polar/1).
:- determination(great/2,less6_polari/1).
:- determination(great/2,less6_sigma/1).
:- determination(great/2,less6_size/1).
:- determination(great/2,less7_flex/1).
:- determination(great/2,less7_h_acc/1).
:- determination(great/2,less7_h_don/1).
:- determination(great/2,less7_pi_acc/1).
:- determination(great/2,less7_pi_don/1).
:- determination(great/2,less7_polar/1).
:- determination(great/2,less7_polari/1).
:- determination(great/2,less7_sigma/1).
:- determination(great/2,less7_size/1).
:- determination(great/2,less8_flex/1).
:- determination(great/2,less8_h_acc/1).
:- determination(great/2,less8_h_don/1).
:- determination(great/2,less8_pi_acc/1).
:- determination(great/2,less8_pi_don/1).
:- determination(great/2,less8_polar/1).
:- determination(great/2,less8_polari/1).
:- determination(great/2,less8_sigma/1).
:- determination(great/2,less8_size/1).
:- determination(great/2,less9_flex/1).
:- determination(great/2,less9_h_acc/1).
:- determination(great/2,less9_h_don/1).
:- determination(great/2,less9_pi_acc/1).
:- determination(great/2,less9_pi_don/1).
:- determination(great/2,less9_polar/1).
:- determination(great/2,less9_polari/1).
:- determination(great/2,less9_sigma/1).
:- determination(great/2,less9_size/1).
:- determination(great/2,pi_acceptor/2).
:- determination(great/2,pi_acceptor0/1).
:- determination(great/2,pi_acceptor0x/1).
:- determination(great/2,pi_acceptor1/1).
:- determination(great/2,pi_acceptor1x/1).
:- determination(great/2,pi_acceptor2/1).
:- determination(great/2,pi_acceptor2x/1).
:- determination(great/2,pi_doner/2).
:- determination(great/2,pi_doner0/1).
:- determination(great/2,pi_doner0x/1).
:- determination(great/2,pi_doner1/1).
:- determination(great/2,pi_doner1x/1).
:- determination(great/2,pi_doner2/1).
:- determination(great/2,pi_doner2x/1).
:- determination(great/2,polar/2).
:- determination(great/2,polar0/1).
:- determination(great/2,polar0x/1).
:- determination(great/2,polar1/1).
:- determination(great/2,polar1x/1).
:- determination(great/2,polar2/1).
:- determination(great/2,polar2x/1).
:- determination(great/2,polar3/1).
:- determination(great/2,polar3x/1).
:- determination(great/2,polar4/1).
:- determination(great/2,polar4x/1).
:- determination(great/2,polar5/1).
:- determination(great/2,polar5x/1).
:- determination(great/2,polarisable/2).
:- determination(great/2,polarisable0/1).
:- determination(great/2,polarisable0x/1).
:- determination(great/2,polarisable1/1).
:- determination(great/2,polarisable1x/1).
:- determination(great/2,polarisable2/1).
:- determination(great/2,polarisable2x/1).
:- determination(great/2,polarisable3/1).
:- determination(great/2,polarisable3x/1).
:- determination(great/2,sigma/2).
:- determination(great/2,sigma0/1).
:- determination(great/2,sigma0x/1).
:- determination(great/2,sigma1/1).
:- determination(great/2,sigma1x/1).
:- determination(great/2,sigma2/1).
:- determination(great/2,sigma2x/1).
:- determination(great/2,sigma3/1).
:- determination(great/2,sigma3x/1).
:- determination(great/2,sigma5/1).
:- determination(great/2,sigma5x/1).
:- determination(great/2,size/2).
:- determination(great/2,size1/1).
:- determination(great/2,size1x/1).
:- determination(great/2,size2/1).
:- determination(great/2,size2x/1).
:- determination(great/2,size3/1).
:- determination(great/2,size3x/1).
:- determination(great/2,size4/1).
:- determination(great/2,size4x/1).
:- determination(great/2,size5/1).
:- determination(great/2,size5x/1).
:- determination(great/2,struc/4).
:- determination(great/2,gt/2).
*/

%%%%%%%%%%% MODES

:- modeh(1, great(+drug, +drug)).
:- modeb(*,struc(+drug,-component,-component,-component)).
:- modeb(*,polar(+component,-polarval)).
:- modeb(*,polar(+component,#polarval)).
:- modeb(*,size(+component,-sizeval)).
:- modeb(*,size(+component,#sizeval)).
:- modeb(*,flex(+component,-flexval)).
:- modeb(*,flex(+component,#flexval)).
:- modeb(*,h_doner(+component,-donerval)).
:- modeb(*,h_doner(+component,#donerval)).
:- modeb(*,h_acceptor(+component,-acceptorval)).
:- modeb(*,h_acceptor(+component,#acceptorval)).
:- modeb(*,pi_doner(+component,-pidonnerval)).
:- modeb(*,pi_doner(+component,#pidonnerval)).
:- modeb(*,pi_acceptor(+component,-piacceptorval)).
:- modeb(*,pi_acceptor(+component,#piacceptorval)).
:- modeb(*,polarisable(+component,-polarival)).
:- modeb(*,polarisable(+component,#polarival)).
:- modeb(*,sigma(+component,-sigmaval)).
:- modeb(*,sigma(+component,#sigmaval)).

:- modeb(1,gt(+polarval,+polarval)).
:- modeb(1,gt(+sizeval,+sizeval)).
:- modeb(1,gt(+flexval,+flexval)).
:- modeb(1,gt(+donerval,+donerval)).
:- modeb(1,gt(+acceptorval,+acceptorval)).
:- modeb(1,gt(+pidonnerval,+pidonnerval)).
:- modeb(1,gt(+piacceptorval,+piacceptorval)).
:- modeb(1,gt(+polarival,+polarival)).
:- modeb(1,gt(+sigmaval,+sigmaval)).


%--------------------------------------------------------------------------------------


gt(X, Y) :-
	atom(X),
	atom(Y),
	name(X, XList),
	name(Y, YList),
	last(XList, XChar),
	last(YList, YChar),
	XVal is XChar - 48,
	YVal is YChar - 48,
	XVal > YVal.

struc(d01,oh,xh,oh).
struc(d02,h,xobch2b6ch3,h).
struc(d03,h,xobch2b5ch3,h).
struc(d04,h,xh,h).
struc(d05,h,xno2,h).
struc(d06,f,xh,h).
struc(d07,obch2b7ch3,xh,h).
struc(d08,ch2oh,xh,h).
struc(d09,h,xnh2,h).
struc(d10,ch2oh,xh,ch2oh).
struc(d11,h,xf,h).
struc(d12,obch2b6ch3,xh,h).
struc(d13,h,xoch2ch2och3,h).
struc(d14,h,xcl,h).
struc(d15,oh,xoh,h).
struc(d16,oh,xh,h).
struc(d17,h,xch3,h).
struc(d18,och2ch2och3,xh,h).
struc(d19,ch2obch2b3ch3,xh,h).
struc(d20,och2conh2,xh,h).
struc(d21,h,xocf3,h).
struc(d22,ch2och3,xh,h).
struc(d23,cl,xh,h).
struc(d24,ch3,xh,h).
struc(d25,h,xnbch3b2,h).
struc(d26,h,xbr,h).
struc(d27,h,xoch3,h).
struc(d28,obch2b3ch3,xh,h).
struc(d29,obch2b5ch3,xh,h).
struc(d30,h,xobch2b3ch3,h).
struc(d31,h,xnhcoch3,h).
struc(d32,oso2ch3,xh,h).
struc(d33,och3,xh,h).
struc(d34,br,xh,h).
struc(d35,no2,xnhcoch3,h).
struc(d36,och2c6h5,xh,h).
struc(d37,cf3,xh,h).
struc(d38,och2ch2och3,xoch2ch2och3,h).
struc(d39,i,xh,h).
struc(d40,cf3,xoch3,h).
struc(d41,och3,xoch3,h).
struc(d42,och3,xoch2ch2och3,och3).
struc(d43,och3,xh,och3).
struc(d44,och3,xoch3,och3).
struc(d45,ch3,xoh,ch3).
struc(d46,ch3,xoch3,ch3).
struc(d47,och3,xobch2b5ch3,och3).
struc(d48,och3,xobch2b7ch3,och3).
struc(d49,och3,xoch2c6h5,och3).
struc(d50,och3,xch3,och3).
struc(d51,i,xoch3,i).
struc(d52,i,xoh,i).
struc(d53,br,xnh2,br).
struc(d54,cl,xnh2,cl).
struc(d55,cl,xnh2,ch3).
polar(ch3,polar0).
polar(ch2och3,polar0).
polar(ch2obch2b3ch3,polar0).
polar(nbch3b2,polar1).
polar(ch2oh,polar2).
polar(och3,polar2).
polar(och2ch2och3,polar2).
polar(obch2b3ch3,polar2).
polar(obch2b5ch3,polar2).
polar(obch2b6ch3,polar2).
polar(obch2b7ch3,polar2).
polar(och2c6h5,polar2).
polar(nhcoch3,polar3).
polar(och2conh2,polar3).
polar(nh2,polar3).
polar(i,polar3).
polar(br,polar3).
polar(cl,polar3).
polar(oh,polar3).
polar(cf3,polar3).
polar(ocf3,polar4).
polar(oso2ch3,polar4).
polar(f,polar5).
polar(no2,polar5).


polar(xch3,polar0).
polar(xch2och3,polar0).
polar(xch2obch2b3ch3,polar0).
polar(xnbch3b2,polar1).
polar(xch2oh,polar2).
polar(xoch3,polar2).
polar(xoch2ch2och3,polar2).
polar(xobch2b3ch3,polar2).
polar(xobch2b5ch3,polar2).
polar(xobch2b6ch3,polar2).
polar(xobch2b7ch3,polar2).
polar(xoch2c6h5,polar2).
polar(xnhcoch3,polar3).
polar(xoch2conh2,polar3).
polar(xnh2,polar3).
polar(xi,polar3).
polar(xbr,polar3).
polar(xcl,polar3).
polar(xoh,polar3).
polar(xcf3,polar3).
polar(xocf3,polar4).
polar(xoso2ch3,polar4).
polar(xf,polar5).
polar(xno2,polar5).


size(ch3,size1).
size(nh2,size1).
size(i,size1).
size(br,size1).
size(cl,size1).
size(oh,size1).
size(cf3,size1).
size(f,size1).
size(no2,size2).
size(nbch3b2,size2).
size(ch2och3,size2).
size(ch2oh,size2).
size(och3,size2).
size(nhcoch3,size2).
size(oso2ch3,size2).
size(ocf3,size3).
size(och2conh2,size3).
size(och2ch2och3,size3).
size(obch2b3ch3,size3).
size(ch2obch2b3ch3,size4).
size(och2c6h5,size4).
size(obch2b5ch3,size5).
size(obch2b6ch3,size5).
size(obch2b7ch3,size5).

size(xch3,size1).
size(xnh2,size1).
size(xi,size1).
size(xbr,size1).
size(xcl,size1).
size(xoh,size1).
size(xcf3,size1).
size(xf,size1).
size(xno2,size2).
size(xnbch3b2,size2).
size(xch2och3,size2).
size(xch2oh,size2).
size(xoch3,size2).
size(xnhcoch3,size2).
size(xoso2ch3,size2).
size(xocf3,size3).
size(xoch2conh2,size3).
size(xoch2ch2och3,size3).
size(xobch2b3ch3,size3).
size(xch2obch2b3ch3,size4).
size(xoch2c6h5,size4).
size(xobch2b5ch3,size5).
size(xobch2b6ch3,size5).
size(xobch2b7ch3,size5).


flex(f,flex0).
flex(oh,flex0).
flex(nh2,flex0).
flex(ch3,flex0).
flex(cl,flex0).
flex(no2,flex0).
flex(br,flex0).
flex(i,flex0).
flex(cf3,flex0).
flex(nbch3b2,flex0).
flex(nhcoch3,flex0).
flex(och3,flex1).
flex(ocf3,flex1).
flex(oso2ch3,flex1).
flex(ch2oh,flex2).
flex(och2c6h5,flex2).
flex(och2conh2,flex2).
flex(ch2och3,flex3).
flex(och2ch2och3,flex4).
flex(obch2b3ch3,flex4).
flex(ch2obch2b3ch3,flex6).
flex(obch2b5ch3,flex6).
flex(obch2b6ch3,flex7).
flex(obch2b7ch3,flex8).

flex(xf,flex0).
flex(xoh,flex0).
flex(xnh2,flex0).
flex(xch3,flex0).
flex(xcl,flex0).
flex(xno2,flex0).
flex(xbr,flex0).
flex(xi,flex0).
flex(xcf3,flex0).
flex(xnbch3b2,flex0).
flex(xnhcoch3,flex0).
flex(xoch3,flex1).
flex(xocf3,flex1).
flex(xoso2ch3,flex1).
flex(xch2oh,flex2).
flex(xoch2c6h5,flex2).
flex(xoch2conh2,flex2).
flex(xch2och3,flex3).
flex(xoch2ch2och3,flex4).
flex(xobch2b3ch3,flex4).
flex(xch2obch2b3ch3,flex6).
flex(xobch2b5ch3,flex6).
flex(xobch2b6ch3,flex7).
flex(xobch2b7ch3,flex8).

h_doner(f,h_don0).
h_doner(ch3,h_don0).
h_doner(cl,h_don0).
h_doner(no2,h_don0).
h_doner(br,h_don0).
h_doner(i,h_don0).
h_doner(cf3,h_don0).
h_doner(nbch3b2,h_don0).
h_doner(och3,h_don0).
h_doner(ocf3,h_don0).
h_doner(oso2ch3,h_don0).
h_doner(och2c6h5,h_don0).
h_doner(ch2och3,h_don0).
h_doner(och2ch2och3,h_don0).
h_doner(obch2b3ch3,h_don0).
h_doner(ch2obch2b3ch3,h_don0).
h_doner(obch2b5ch3,h_don0).
h_doner(obch2b6ch3,h_don0).
h_doner(obch2b7ch3,h_don0).
h_doner(nhcoch3,h_don1).
h_doner(och2conh2,h_don1).
h_doner(oh,h_don2).
h_doner(nh2,h_don2).
h_doner(ch2oh,h_don2).

h_doner(xf,h_don0).
h_doner(xch3,h_don0).
h_doner(xcl,h_don0).
h_doner(xno2,h_don0).
h_doner(xbr,h_don0).
h_doner(xi,h_don0).
h_doner(xcf3,h_don0).
h_doner(xnbch3b2,h_don0).
h_doner(xoch3,h_don0).
h_doner(xocf3,h_don0).
h_doner(xoso2ch3,h_don0).
h_doner(xoch2c6h5,h_don0).
h_doner(xch2och3,h_don0).
h_doner(xoch2ch2och3,h_don0).
h_doner(xobch2b3ch3,h_don0).
h_doner(xch2obch2b3ch3,h_don0).
h_doner(xobch2b5ch3,h_don0).
h_doner(xobch2b6ch3,h_don0).
h_doner(xobch2b7ch3,h_don0).
h_doner(xnhcoch3,h_don1).
h_doner(xoch2conh2,h_don1).
h_doner(xoh,h_don2).
h_doner(xnh2,h_don2).
h_doner(xch2oh,h_don2).

h_acceptor(ch3,h_acc0).
h_acceptor(cl,h_acc0).
h_acceptor(no2,h_acc0).
h_acceptor(br,h_acc0).
h_acceptor(i,h_acc0).
h_acceptor(cf3,h_acc0).
h_acceptor(ocf3,h_acc0).
h_acceptor(oso2ch3,h_acc0).
h_acceptor(ch2obch2b3ch3,h_acc0).
h_acceptor(nh2,h_acc0).
h_acceptor(f,h_acc1).
h_acceptor(nbch3b2,h_acc1).
h_acceptor(och3,h_acc1).
h_acceptor(nhcoch3,h_acc1).
h_acceptor(och2c6h5,h_acc1).
h_acceptor(ch2och3,h_acc1).
h_acceptor(och2ch2och3,h_acc1).
h_acceptor(obch2b3ch3,h_acc1).
h_acceptor(obch2b5ch3,h_acc1).
h_acceptor(obch2b6ch3,h_acc1).
h_acceptor(obch2b7ch3,h_acc1).
h_acceptor(och2conh2,h_acc1).
h_acceptor(oh,h_acc2).
h_acceptor(ch2oh,h_acc2).

h_acceptor(xch3,h_acc0).
h_acceptor(xcl,h_acc0).
h_acceptor(xno2,h_acc0).
h_acceptor(xbr,h_acc0).
h_acceptor(xi,h_acc0).
h_acceptor(xcf3,h_acc0).
h_acceptor(xocf3,h_acc0).
h_acceptor(xoso2ch3,h_acc0).
h_acceptor(xch2obch2b3ch3,h_acc0).
h_acceptor(xnh2,h_acc0).
h_acceptor(xf,h_acc1).
h_acceptor(xnbch3b2,h_acc1).
h_acceptor(xoch3,h_acc1).
h_acceptor(xnhcoch3,h_acc1).
h_acceptor(xoch2c6h5,h_acc1).
h_acceptor(xch2och3,h_acc1).
h_acceptor(xoch2ch2och3,h_acc1).
h_acceptor(xobch2b3ch3,h_acc1).
h_acceptor(xobch2b5ch3,h_acc1).
h_acceptor(xobch2b6ch3,h_acc1).
h_acceptor(xobch2b7ch3,h_acc1).
h_acceptor(xoch2conh2,h_acc1).
h_acceptor(xoh,h_acc2).
h_acceptor(xch2oh,h_acc2).

pi_doner(ch3,pi_don0).
pi_doner(no2,pi_don0).
pi_doner(cf3,pi_don0).
pi_doner(ocf3,pi_don0).
pi_doner(oso2ch3,pi_don0).
pi_doner(f,pi_don0).
pi_doner(ch2och3,pi_don0).
pi_doner(ch2obch2b3ch3,pi_don0).
pi_doner(ch2oh,pi_don0).
pi_doner(cl,pi_don0).
pi_doner(br,pi_don1).
pi_doner(i,pi_don1).
pi_doner(nhcoch3,pi_don1).
pi_doner(och2c6h5,pi_don1).
pi_doner(och2ch2och3,pi_don1).
pi_doner(obch2b3ch3,pi_don1).
pi_doner(obch2b5ch3,pi_don1).
pi_doner(obch2b6ch3,pi_don1).
pi_doner(obch2b7ch3,pi_don1).
pi_doner(och2conh2,pi_don1).
pi_doner(och3,pi_don1).
pi_doner(nh2,pi_don2).
pi_doner(nbch3b2,pi_don2).
pi_doner(oh,pi_don2).

pi_doner(xch3,pi_don0).
pi_doner(xno2,pi_don0).
pi_doner(xcf3,pi_don0).
pi_doner(xocf3,pi_don0).
pi_doner(xoso2ch3,pi_don0).
pi_doner(xf,pi_don0).
pi_doner(xch2och3,pi_don0).
pi_doner(xch2obch2b3ch3,pi_don0).
pi_doner(xch2oh,pi_don0).
pi_doner(xcl,pi_don0).
pi_doner(xbr,pi_don1).
pi_doner(xi,pi_don1).
pi_doner(xnhcoch3,pi_don1).
pi_doner(xoch2c6h5,pi_don1).
pi_doner(xoch2ch2och3,pi_don1).
pi_doner(xobch2b3ch3,pi_don1).
pi_doner(xobch2b5ch3,pi_don1).
pi_doner(xobch2b6ch3,pi_don1).
pi_doner(xobch2b7ch3,pi_don1).
pi_doner(xoch2conh2,pi_don1).
pi_doner(xoch3,pi_don1).
pi_doner(xnh2,pi_don2).
pi_doner(xnbch3b2,pi_don2).
pi_doner(xoh,pi_don2).

pi_acceptor(f,pi_acc0).
pi_acceptor(ch3,pi_acc0).
pi_acceptor(ch2och3,pi_acc0).
pi_acceptor(ch2obch2b3ch3,pi_acc0).
pi_acceptor(ch2oh,pi_acc0).
pi_acceptor(cl,pi_acc0).
pi_acceptor(br,pi_acc0).
pi_acceptor(nhcoch3,pi_acc0).
pi_acceptor(och2c6h5,pi_acc0).
pi_acceptor(och2ch2och3,pi_acc0).
pi_acceptor(obch2b3ch3,pi_acc0).
pi_acceptor(obch2b5ch3,pi_acc0).
pi_acceptor(obch2b6ch3,pi_acc0).
pi_acceptor(obch2b7ch3,pi_acc0).
pi_acceptor(och2conh2,pi_acc0).
pi_acceptor(nh2,pi_acc0).
pi_acceptor(nbch3b2,pi_acc0).
pi_acceptor(och3,pi_acc0).
pi_acceptor(oh,pi_acc0).
pi_acceptor(i,pi_acc0).
pi_acceptor(cf3,pi_acc0).
pi_acceptor(oso2ch3,pi_acc1).
pi_acceptor(no2,pi_acc2).
pi_acceptor(ocf3,pi_acc2).

pi_acceptor(xf,pi_acc0).
pi_acceptor(xch3,pi_acc0).
pi_acceptor(xch2och3,pi_acc0).
pi_acceptor(xch2obch2b3ch3,pi_acc0).
pi_acceptor(xch2oh,pi_acc0).
pi_acceptor(xcl,pi_acc0).
pi_acceptor(xbr,pi_acc0).
pi_acceptor(xnhcoch3,pi_acc0).
pi_acceptor(xoch2c6h5,pi_acc0).
pi_acceptor(xoch2ch2och3,pi_acc0).
pi_acceptor(xobch2b3ch3,pi_acc0).
pi_acceptor(xobch2b5ch3,pi_acc0).
pi_acceptor(xobch2b6ch3,pi_acc0).
pi_acceptor(xobch2b7ch3,pi_acc0).
pi_acceptor(xoch2conh2,pi_acc0).
pi_acceptor(xnh2,pi_acc0).
pi_acceptor(xnbch3b2,pi_acc0).
pi_acceptor(xoch3,pi_acc0).
pi_acceptor(xoh,pi_acc0).
pi_acceptor(xi,pi_acc0).
pi_acceptor(xcf3,pi_acc0).
pi_acceptor(xoso2ch3,pi_acc1).
pi_acceptor(xno2,pi_acc2).
pi_acceptor(xocf3,pi_acc2).

polarisable(och2conh2,polari0).
polarisable(nh2,polari0).
polarisable(cf3,polari0).
polarisable(no2,polari0).
polarisable(ocf3,polari0).
polarisable(f,polari0).
polarisable(ch3,polari1).
polarisable(ch2och3,polari1).
polarisable(ch2obch2b3ch3,polari1).
polarisable(ch2oh,polari1).
polarisable(cl,polari1).
polarisable(nhcoch3,polari1).
polarisable(och2ch2och3,polari1).
polarisable(obch2b3ch3,polari1).
polarisable(obch2b5ch3,polari1).
polarisable(obch2b6ch3,polari1).
polarisable(obch2b7ch3,polari1).
polarisable(nbch3b2,polari1).
polarisable(och3,polari1).
polarisable(oh,polari1).
polarisable(och2c6h5,polari1).
polarisable(br,polari2).
polarisable(oso2ch3,polari2).
polarisable(i,polari3).

polarisable(xoch2conh2,polari0).
polarisable(xnh2,polari0).
polarisable(xcf3,polari0).
polarisable(xno2,polari0).
polarisable(xocf3,polari0).
polarisable(xf,polari0).
polarisable(xch3,polari1).
polarisable(xch2och3,polari1).
polarisable(xch2obch2b3ch3,polari1).
polarisable(xch2oh,polari1).
polarisable(xcl,polari1).
polarisable(xnhcoch3,polari1).
polarisable(xoch2ch2och3,polari1).
polarisable(xobch2b3ch3,polari1).
polarisable(xobch2b5ch3,polari1).
polarisable(xobch2b6ch3,polari1).
polarisable(xobch2b7ch3,polari1).
polarisable(xnbch3b2,polari1).
polarisable(xoch3,polari1).
polarisable(xoh,polari1).
polarisable(xoch2c6h5,polari1).
polarisable(xbr,polari2).
polarisable(xoso2ch3,polari2).
polarisable(xi,polari3).

sigma(ch3,sigma0).
sigma(ch2och3,sigma0).
sigma(ch2obch2b3ch3,sigma0).
sigma(ch2oh,sigma0).
sigma(obch2b3ch3,sigma1).
sigma(nbch3b2,sigma1).
sigma(nh2,sigma1).
sigma(nhcoch3,sigma1).
sigma(och2ch2och3,sigma1).
sigma(obch2b5ch3,sigma1).
sigma(obch2b6ch3,sigma1).
sigma(obch2b7ch3,sigma1).
sigma(och3,sigma1).
sigma(och2c6h5,sigma1).
sigma(och2conh2,sigma1).
sigma(oh,sigma2).
sigma(i,sigma3).
sigma(br,sigma3).
sigma(oso2ch3,sigma3).
sigma(cl,sigma3).
sigma(cf3,sigma3).
sigma(no2,sigma3).
sigma(ocf3,sigma3).
sigma(f,sigma5).

sigma(xch3,sigma0).
sigma(xch2och3,sigma0).
sigma(xch2obch2b3ch3,sigma0).
sigma(xch2oh,sigma0).
sigma(xobch2b3ch3,sigma1).
sigma(xnbch3b2,sigma1).
sigma(xnh2,sigma1).
sigma(xnhcoch3,sigma1).
sigma(xoch2ch2och3,sigma1).
sigma(xobch2b5ch3,sigma1).
sigma(xobch2b6ch3,sigma1).
sigma(xobch2b7ch3,sigma1).
sigma(xoch3,sigma1).
sigma(xoch2c6h5,sigma1).
sigma(xoch2conh2,sigma1).
sigma(xoh,sigma2).
sigma(xi,sigma3).
sigma(xbr,sigma3).
sigma(xoso2ch3,sigma3).
sigma(xcl,sigma3).
sigma(xcf3,sigma3).
sigma(xno2,sigma3).
sigma(xocf3,sigma3).
sigma(xf,sigma5).

polar0(ch3).
polar0(ch2och3).
polar0(ch2obch2b3ch3).
polar1(nbch3b2).
polar2(ch2oh).
polar2(och3).
polar2(och2ch2och3).
polar2(obch2b3ch3).
polar2(obch2b5ch3).
polar2(obch2b6ch3).
polar2(obch2b7ch3).
polar2(och2c6h5).
polar3(nhcoch3).
polar3(och2conh2).
polar3(nh2).
polar3(i).
polar3(br).
polar3(cl).
polar3(oh).
polar3(cf3).
polar4(ocf3).
polar4(oso2ch3).
polar5(f).
polar5(no2).

size1(ch3).
size1(nh2).
size1(i).
size1(br).
size1(cl).
size1(oh).
size1(cf3).
size1(f).
size2(no2).
size2(nbch3b2).
size2(ch2och3).
size2(ch2oh).
size2(och3).
size2(nhcoch3).
size2(oso2ch3).
size3(ocf3).
size3(och2conh2).
size3(och2ch2och3).
size3(obch2b3ch3).
size4(ch2obch2b3ch3).
size4(och2c6h5).
size5(obch2b5ch3).
size5(obch2b6ch3).
size5(obch2b7ch3).

flex0(f).
flex0(oh).
flex0(nh2).
flex0(ch3).
flex0(cl).
flex0(no2).
flex0(br).
flex0(i).
flex0(cf3).
flex0(nbch3b2).
flex0(nhcoch3).
flex1(och3).
flex1(ocf3).
flex1(oso2ch3).
flex2(ch2oh).
flex2(och2c6h5).
flex2(och2conh2).
flex3(ch2och3).
flex4(och2ch2och3).
flex4(obch2b3ch3).
flex6(ch2obch2b3ch3).
flex6(obch2b5ch3).
flex7(obch2b6ch3).
flex8(obch2b7ch3).

h_doner0(f).
h_doner0(ch3).
h_doner0(cl).
h_doner0(no2).
h_doner0(br).
h_doner0(i).
h_doner0(cf3).
h_doner0(nbch3b2).
h_doner0(och3).
h_doner0(ocf3).
h_doner0(oso2ch3).
h_doner0(och2c6h5).
h_doner0(ch2och3).
h_doner0(och2ch2och3).
h_doner0(obch2b3ch3).
h_doner0(ch2obch2b3ch3).
h_doner0(obch2b5ch3).
h_doner0(obch2b6ch3).
h_doner0(obch2b7ch3).
h_doner1(nhcoch3).
h_doner1(och2conh2).
h_doner2(oh).
h_doner2(nh2).
h_doner2(ch2oh).

h_acceptor0(ch3).
h_acceptor0(cl).
h_acceptor0(no2).
h_acceptor0(br).
h_acceptor0(i).
h_acceptor0(cf3).
h_acceptor0(ocf3).
h_acceptor0(oso2ch3).
h_acceptor0(ch2obch2b3ch3).
h_acceptor0(nh2).
h_acceptor1(f).
h_acceptor1(nbch3b2).
h_acceptor1(och3).
h_acceptor1(nhcoch3).
h_acceptor1(och2c6h5).
h_acceptor1(ch2och3).
h_acceptor1(och2ch2och3).
h_acceptor1(obch2b3ch3).
h_acceptor1(obch2b5ch3).
h_acceptor1(obch2b6ch3).
h_acceptor1(obch2b7ch3).
h_acceptor1(och2conh2).
h_acceptor2(oh).
h_acceptor2(ch2oh).

pi_doner0(ch3).
pi_doner0(no2).
pi_doner0(cf3).
pi_doner0(ocf3).
pi_doner0(oso2ch3).
pi_doner0(f).
pi_doner0(ch2och3).
pi_doner0(ch2obch2b3ch3).
pi_doner0(ch2oh).
pi_doner0(cl).
pi_doner1(br).
pi_doner1(i).
pi_doner1(nhcoch3).
pi_doner1(och2c6h5).
pi_doner1(och2ch2och3).
pi_doner1(obch2b3ch3).
pi_doner1(obch2b5ch3).
pi_doner1(obch2b6ch3).
pi_doner1(obch2b7ch3).
pi_doner1(och2conh2).
pi_doner1(och3).
pi_doner2(nh2).
pi_doner2(nbch3b2).
pi_doner2(oh).

pi_acceptor0(f).
pi_acceptor0(ch3).
pi_acceptor0(ch2och3).
pi_acceptor0(ch2obch2b3ch3).
pi_acceptor0(ch2oh).
pi_acceptor0(cl).
pi_acceptor0(br).
pi_acceptor0(nhcoch3).
pi_acceptor0(och2c6h5).
pi_acceptor0(och2ch2och3).
pi_acceptor0(obch2b3ch3).
pi_acceptor0(obch2b5ch3).
pi_acceptor0(obch2b6ch3).
pi_acceptor0(obch2b7ch3).
pi_acceptor0(och2conh2).
pi_acceptor0(nh2).
pi_acceptor0(nbch3b2).
pi_acceptor0(och3).
pi_acceptor0(oh).
pi_acceptor0(i).
pi_acceptor0(cf3).
pi_acceptor1(oso2ch3).
pi_acceptor2(no2).
pi_acceptor2(ocf3).

polarisable0(och2conh2).
polarisable0(nh2).
polarisable0(cf3).
polarisable0(no2).
polarisable0(ocf3).
polarisable0(f).
polarisable1(ch3).
polarisable1(ch2och3).
polarisable1(ch2obch2b3ch3).
polarisable1(ch2oh).
polarisable1(cl).
polarisable1(nhcoch3).
polarisable1(och2ch2och3).
polarisable1(obch2b3ch3).
polarisable1(obch2b5ch3).
polarisable1(obch2b6ch3).
polarisable1(obch2b7ch3).
polarisable1(nbch3b2).
polarisable1(och3).
polarisable1(oh).
polarisable1(och2c6h5).
polarisable2(br).
polarisable2(oso2ch3).
polarisable3(i).

sigma0(ch3).
sigma0(ch2och3).
sigma0(ch2obch2b3ch3).
sigma0(ch2oh).
sigma1(obch2b3ch3).
sigma1(nbch3b2).
sigma1(nh2).
sigma1(nhcoch3).
sigma1(och2ch2och3).
sigma1(obch2b5ch3).
sigma1(obch2b6ch3).
sigma1(obch2b7ch3).
sigma1(och3).
sigma1(och2c6h5).
sigma1(och2conh2).
sigma2(oh).
sigma3(i).
sigma3(br).
sigma3(oso2ch3).
sigma3(cl).
sigma3(cf3).
sigma3(no2).
sigma3(ocf3).
sigma5(f).


polar0x(xch3).
polar0x(xch2och3).
polar0x(xch2obch2b3ch3).
polar1x(xnbch3b2).
polar2x(xch2oh).
polar2x(xoch3).
polar2x(xoch2ch2och3).
polar2x(xobch2b3ch3).
polar2x(xobch2b5ch3).
polar2x(xobch2b6ch3).
polar2x(xobch2b7ch3).
polar2x(xoch2c6h5).
polar3x(xnhcoch3).
polar3x(xoch2conh2).
polar3x(xnh2).
polar3x(xi).
polar3x(xbr).
polar3x(xcl).
polar3x(xoh).
polar3x(xcf3).
polar4x(xocf3).
polar4x(xoso2ch3).
polar5x(xf).
polar5x(xno2).

size1x(xch3).
size1x(xnh2).
size1x(xi).
size1x(xbr).
size1x(xcl).
size1x(xoh).
size1x(xcf3).
size1x(xf).
size2x(xno2).
size2x(xnbch3b2).
size2x(xch2och3).
size2x(xch2oh).
size2x(xoch3).
size2x(xnhcoch3).
size2x(xoso2ch3).
size3x(xocf3).
size3x(xoch2conh2).
size3x(xoch2ch2och3).
size3x(xobch2b3ch3).
size4x(xch2obch2b3ch3).
size4x(xoch2c6h5).
size5x(xobch2b5ch3).
size5x(xobch2b6ch3).
size5x(xobch2b7ch3).

flex0x(xf).
flex0x(xoh).
flex0x(xnh2).
flex0x(xch3).
flex0x(xcl).
flex0x(xno2).
flex0x(xbr).
flex0x(xi).
flex0x(xcf3).
flex0x(xnbch3b2).
flex0x(xnhcoch3).
flex1x(xoch3).
flex1x(xocf3).
flex1x(xoso2ch3).
flex2x(xch2oh).
flex2x(xoch2c6h5).
flex2x(xoch2conh2).
flex3x(xch2och3).
flex4x(xoch2ch2och3).
flex4x(xobch2b3ch3).
flex6x(xch2obch2b3ch3).
flex6x(xobch2b5ch3).
flex7x(xobch2b6ch3).
flex8x(xobch2b7ch3).

h_doner0x(xf).
h_doner0x(xch3).
h_doner0x(xcl).
h_doner0x(xno2).
h_doner0x(xbr).
h_doner0x(xi).
h_doner0x(xcf3).
h_doner0x(xnbch3b2).
h_doner0x(xoch3).
h_doner0x(xocf3).
h_doner0x(xoso2ch3).
h_doner0x(xoch2c6h5).
h_doner0x(xch2och3).
h_doner0x(xoch2ch2och3).
h_doner0x(xobch2b3ch3).
h_doner0x(xch2obch2b3ch3).
h_doner0x(xobch2b5ch3).
h_doner0x(xobch2b6ch3).
h_doner0x(xobch2b7ch3).
h_doner1x(xnhcoch3).
h_doner1x(xoch2conh2).
h_doner2x(xoh).
h_doner2x(xnh2).
h_doner2x(xch2oh).

h_acceptor0x(xch3).
h_acceptor0x(xcl).
h_acceptor0x(xno2).
h_acceptor0x(xbr).
h_acceptor0x(xi).
h_acceptor0x(xcf3).
h_acceptor0x(xocf3).
h_acceptor0x(xoso2ch3).
h_acceptor0x(xch2obch2b3ch3).
h_acceptor0x(xnh2).
h_acceptor1x(xf).
h_acceptor1x(xnbch3b2).
h_acceptor1x(xoch3).
h_acceptor1x(xnhcoch3).
h_acceptor1x(xoch2c6h5).
h_acceptor1x(xch2och3).
h_acceptor1x(xoch2ch2och3).
h_acceptor1x(xobch2b3ch3).
h_acceptor1x(xobch2b5ch3).
h_acceptor1x(xobch2b6ch3).
h_acceptor1x(xobch2b7ch3).
h_acceptor1x(xoch2conh2).
h_acceptor2x(xoh).
h_acceptor2x(xch2oh).

pi_doner0x(xch3).
pi_doner0x(xno2).
pi_doner0x(xcf3).
pi_doner0x(xocf3).
pi_doner0x(xoso2ch3).
pi_doner0x(xf).
pi_doner0x(xch2och3).
pi_doner0x(xch2obch2b3ch3).
pi_doner0x(xch2oh).
pi_doner0x(xcl).
pi_doner1x(xbr).
pi_doner1x(xi).
pi_doner1x(xnhcoch3).
pi_doner1x(xoch2c6h5).
pi_doner1x(xoch2ch2och3).
pi_doner1x(xobch2b3ch3).
pi_doner1x(xobch2b5ch3).
pi_doner1x(xobch2b6ch3).
pi_doner1x(xobch2b7ch3).
pi_doner1x(xoch2conh2).
pi_doner1x(xoch3).
pi_doner2x(xnh2).
pi_doner2x(xnbch3b2).
pi_doner2x(xoh).

pi_acceptor0x(xf).
pi_acceptor0x(xch3).
pi_acceptor0x(xch2och3).
pi_acceptor0x(xch2obch2b3ch3).
pi_acceptor0x(xch2oh).
pi_acceptor0x(xcl).
pi_acceptor0x(xbr).
pi_acceptor0x(xnhcoch3).
pi_acceptor0x(xoch2c6h5).
pi_acceptor0x(xoch2ch2och3).
pi_acceptor0x(xobch2b3ch3).
pi_acceptor0x(xobch2b5ch3).
pi_acceptor0x(xobch2b6ch3).
pi_acceptor0x(xobch2b7ch3).
pi_acceptor0x(xoch2conh2).
pi_acceptor0x(xnh2).
pi_acceptor0x(xnbch3b2).
pi_acceptor0x(xoch3).
pi_acceptor0x(xoh).
pi_acceptor0x(xi).
pi_acceptor0x(xcf3).
pi_acceptor1x(xoso2ch3).
pi_acceptor2x(xno2).
pi_acceptor2x(xocf3).

polarisable0x(xoch2conh2).
polarisable0x(xnh2).
polarisable0x(xcf3).
polarisable0x(xno2).
polarisable0x(xocf3).
polarisable0x(xf).
polarisable1x(xch3).
polarisable1x(xch2och3).
polarisable1x(xch2obch2b3ch3).
polarisable1x(xch2oh).
polarisable1x(xcl).
polarisable1x(xnhcoch3).
polarisable1x(xoch2ch2och3).
polarisable1x(xobch2b3ch3).
polarisable1x(xobch2b5ch3).
polarisable1x(xobch2b6ch3).
polarisable1x(xobch2b7ch3).
polarisable1x(xnbch3b2).
polarisable1x(xoch3).
polarisable1x(xoh).
polarisable1x(xoch2c6h5).
polarisable2x(xbr).
polarisable2x(xoso2ch3).
polarisable3x(xi).

sigma0x(xch3).
sigma0x(xch2och3).
sigma0x(xch2obch2b3ch3).
sigma0x(xch2oh).
sigma1x(xobch2b3ch3).
sigma1x(xnbch3b2).
sigma1x(xnh2).
sigma1x(xnhcoch3).
sigma1x(xoch2ch2och3).
sigma1x(xobch2b5ch3).
sigma1x(xobch2b6ch3).
sigma1x(xobch2b7ch3).
sigma1x(xoch3).
sigma1x(xoch2c6h5).
sigma1x(xoch2conh2).
sigma2x(xoh).
sigma3x(xi).
sigma3x(xbr).
sigma3x(xoso2ch3).
sigma3x(xcl).
sigma3x(xcf3).
sigma3x(xno2).
sigma3x(xocf3).
sigma5x(xf).


great_polar(polar1,polar0).
great_polar(polar2,polar0).
great_polar(polar3,polar0).
great_polar(polar4,polar0).
great_polar(polar5,polar0).
great_polar(polar6,polar0).
great_polar(polar7,polar0).
great_polar(polar8,polar0).
great_polar(polar9,polar0).
great_polar(polar2,polar1).
great_polar(polar3,polar1).
great_polar(polar4,polar1).
great_polar(polar5,polar1).
great_polar(polar6,polar1).
great_polar(polar7,polar1).
great_polar(polar8,polar1).
great_polar(polar9,polar1).
great_polar(polar3,polar2).
great_polar(polar4,polar2).
great_polar(polar5,polar2).
great_polar(polar6,polar2).
great_polar(polar7,polar2).
great_polar(polar8,polar2).
great_polar(polar9,polar2).
great_polar(polar4,polar3).
great_polar(polar5,polar3).
great_polar(polar6,polar3).
great_polar(polar7,polar3).
great_polar(polar8,polar3).
great_polar(polar9,polar3).
great_polar(polar5,polar4).
great_polar(polar6,polar4).
great_polar(polar7,polar4).
great_polar(polar8,polar4).
great_polar(polar9,polar4).
great_polar(polar6,polar5).
great_polar(polar7,polar5).
great_polar(polar8,polar5).
great_polar(polar9,polar5).
great_polar(polar7,polar6).
great_polar(polar8,polar6).
great_polar(polar9,polar6).
great_polar(polar8,polar7).
great_polar(polar9,polar7).
great_polar(polar9,polar8).

great0_polar(polar1).
great0_polar(polar2).
great0_polar(polar3).
great0_polar(polar4).
great0_polar(polar5).
great0_polar(polar6).
great0_polar(polar7).
great0_polar(polar8).
great0_polar(polar9).
great1_polar(polar2).
great1_polar(polar3).
great1_polar(polar4).
great1_polar(polar5).
great1_polar(polar6).
great1_polar(polar7).
great1_polar(polar8).
great1_polar(polar9).
great2_polar(polar3).
great2_polar(polar4).
great2_polar(polar5).
great2_polar(polar6).
great2_polar(polar7).
great2_polar(polar8).
great2_polar(polar9).
great3_polar(polar4).
great3_polar(polar5).
great3_polar(polar6).
great3_polar(polar7).
great3_polar(polar8).
great3_polar(polar9).
great4_polar(polar5).
great4_polar(polar6).
great4_polar(polar7).
great4_polar(polar8).
great4_polar(polar9).
great5_polar(polar6).
great5_polar(polar7).
great5_polar(polar8).
great5_polar(polar9).
great6_polar(polar7).
great6_polar(polar8).
great6_polar(polar9).
great7_polar(polar8).
great7_polar(polar9).

less9_polar(polar0).
less9_polar(polar1).
less9_polar(polar2).
less9_polar(polar3).
less9_polar(polar4).
less9_polar(polar5).
less9_polar(polar6).
less9_polar(polar7).
less9_polar(polar8).
less8_polar(polar0).
less8_polar(polar1).
less8_polar(polar2).
less8_polar(polar3).
less8_polar(polar4).
less8_polar(polar5).
less8_polar(polar6).
less8_polar(polar7).
less7_polar(polar0).
less7_polar(polar1).
less7_polar(polar2).
less7_polar(polar3).
less7_polar(polar4).
less7_polar(polar5).
less7_polar(polar6).
less6_polar(polar0).
less6_polar(polar1).
less6_polar(polar2).
less6_polar(polar3).
less6_polar(polar4).
less6_polar(polar5).
less5_polar(polar0).
less5_polar(polar1).
less5_polar(polar2).
less5_polar(polar3).
less5_polar(polar4).
less4_polar(polar0).
less4_polar(polar1).
less4_polar(polar2).
less4_polar(polar3).
less3_polar(polar0).
less3_polar(polar1).
less3_polar(polar2).
less2_polar(polar0).
less2_polar(polar1).

great_size(size1,size0).
great_size(size2,size0).
great_size(size3,size0).
great_size(size4,size0).
great_size(size5,size0).
great_size(size6,size0).
great_size(size7,size0).
great_size(size8,size0).
great_size(size9,size0).
great_size(size2,size1).
great_size(size3,size1).
great_size(size4,size1).
great_size(size5,size1).
great_size(size6,size1).
great_size(size7,size1).
great_size(size8,size1).
great_size(size9,size1).
great_size(size3,size2).
great_size(size4,size2).
great_size(size5,size2).
great_size(size6,size2).
great_size(size7,size2).
great_size(size8,size2).
great_size(size9,size2).
great_size(size4,size3).
great_size(size5,size3).
great_size(size6,size3).
great_size(size7,size3).
great_size(size8,size3).
great_size(size9,size3).
great_size(size5,size4).
great_size(size6,size4).
great_size(size7,size4).
great_size(size8,size4).
great_size(size9,size4).
great_size(size6,size5).
great_size(size7,size5).
great_size(size8,size5).
great_size(size9,size5).
great_size(size7,size6).
great_size(size8,size6).
great_size(size9,size6).
great_size(size8,size7).
great_size(size9,size7).
great_size(size9,size8).

great0_size(size1).
great0_size(size2).
great0_size(size3).
great0_size(size4).
great0_size(size5).
great0_size(size6).
great0_size(size7).
great0_size(size8).
great0_size(size9).
great1_size(size2).
great1_size(size3).
great1_size(size4).
great1_size(size5).
great1_size(size6).
great1_size(size7).
great1_size(size8).
great1_size(size9).
great2_size(size3).
great2_size(size4).
great2_size(size5).
great2_size(size6).
great2_size(size7).
great2_size(size8).
great2_size(size9).
great3_size(size4).
great3_size(size5).
great3_size(size6).
great3_size(size7).
great3_size(size8).
great3_size(size9).
great4_size(size5).
great4_size(size6).
great4_size(size7).
great4_size(size8).
great4_size(size9).
great5_size(size6).
great5_size(size7).
great5_size(size8).
great5_size(size9).
great6_size(size7).
great6_size(size8).
great6_size(size9).
great7_size(size8).
great7_size(size9).

less9_size(size0).
less9_size(size1).
less9_size(size2).
less9_size(size3).
less9_size(size4).
less9_size(size5).
less9_size(size6).
less9_size(size7).
less9_size(size8).
less8_size(size0).
less8_size(size1).
less8_size(size2).
less8_size(size3).
less8_size(size4).
less8_size(size5).
less8_size(size6).
less8_size(size7).
less7_size(size0).
less7_size(size1).
less7_size(size2).
less7_size(size3).
less7_size(size4).
less7_size(size5).
less7_size(size6).
less6_size(size0).
less6_size(size1).
less6_size(size2).
less6_size(size3).
less6_size(size4).
less6_size(size5).
less5_size(size0).
less5_size(size1).
less5_size(size2).
less5_size(size3).
less5_size(size4).
less4_size(size0).
less4_size(size1).
less4_size(size2).
less4_size(size3).
less3_size(size0).
less3_size(size1).
less3_size(size2).
less2_size(size0).
less2_size(size1).

great_flex(flex1,flex0).
great_flex(flex2,flex0).
great_flex(flex3,flex0).
great_flex(flex4,flex0).
great_flex(flex5,flex0).
great_flex(flex6,flex0).
great_flex(flex7,flex0).
great_flex(flex8,flex0).
great_flex(flex9,flex0).
great_flex(flex2,flex1).
great_flex(flex3,flex1).
great_flex(flex4,flex1).
great_flex(flex5,flex1).
great_flex(flex6,flex1).
great_flex(flex7,flex1).
great_flex(flex8,flex1).
great_flex(flex9,flex1).
great_flex(flex3,flex2).
great_flex(flex4,flex2).
great_flex(flex5,flex2).
great_flex(flex6,flex2).
great_flex(flex7,flex2).
great_flex(flex8,flex2).
great_flex(flex9,flex2).
great_flex(flex4,flex3).
great_flex(flex5,flex3).
great_flex(flex6,flex3).
great_flex(flex7,flex3).
great_flex(flex8,flex3).
great_flex(flex9,flex3).
great_flex(flex5,flex4).
great_flex(flex6,flex4).
great_flex(flex7,flex4).
great_flex(flex8,flex4).
great_flex(flex9,flex4).
great_flex(flex6,flex5).
great_flex(flex7,flex5).
great_flex(flex8,flex5).
great_flex(flex9,flex5).
great_flex(flex7,flex6).
great_flex(flex8,flex6).
great_flex(flex9,flex6).
great_flex(flex8,flex7).
great_flex(flex9,flex7).
great_flex(flex9,flex8).

great0_flex(flex1).
great0_flex(flex2).
great0_flex(flex3).
great0_flex(flex4).
great0_flex(flex5).
great0_flex(flex6).
great0_flex(flex7).
great0_flex(flex8).
great0_flex(flex9).
great1_flex(flex2).
great1_flex(flex3).
great1_flex(flex4).
great1_flex(flex5).
great1_flex(flex6).
great1_flex(flex7).
great1_flex(flex8).
great1_flex(flex9).
great2_flex(flex3).
great2_flex(flex4).
great2_flex(flex5).
great2_flex(flex6).
great2_flex(flex7).
great2_flex(flex8).
great2_flex(flex9).
great3_flex(flex4).
great3_flex(flex5).
great3_flex(flex6).
great3_flex(flex7).
great3_flex(flex8).
great3_flex(flex9).
great4_flex(flex5).
great4_flex(flex6).
great4_flex(flex7).
great4_flex(flex8).
great4_flex(flex9).
great5_flex(flex6).
great5_flex(flex7).
great5_flex(flex8).
great5_flex(flex9).
great6_flex(flex7).
great6_flex(flex8).
great6_flex(flex9).
great7_flex(flex8).
great7_flex(flex9).

less9_flex(flex0).
less9_flex(flex1).
less9_flex(flex2).
less9_flex(flex3).
less9_flex(flex4).
less9_flex(flex5).
less9_flex(flex6).
less9_flex(flex7).
less9_flex(flex8).
less8_flex(flex0).
less8_flex(flex1).
less8_flex(flex2).
less8_flex(flex3).
less8_flex(flex4).
less8_flex(flex5).
less8_flex(flex6).
less8_flex(flex7).
less7_flex(flex0).
less7_flex(flex1).
less7_flex(flex2).
less7_flex(flex3).
less7_flex(flex4).
less7_flex(flex5).
less7_flex(flex6).
less6_flex(flex0).
less6_flex(flex1).
less6_flex(flex2).
less6_flex(flex3).
less6_flex(flex4).
less6_flex(flex5).
less5_flex(flex0).
less5_flex(flex1).
less5_flex(flex2).
less5_flex(flex3).
less5_flex(flex4).
less4_flex(flex0).
less4_flex(flex1).
less4_flex(flex2).
less4_flex(flex3).
less3_flex(flex0).
less3_flex(flex1).
less3_flex(flex2).
less2_flex(flex0).
less2_flex(flex1).

great_h_don(h_don1,h_don0).
great_h_don(h_don2,h_don0).
great_h_don(h_don3,h_don0).
great_h_don(h_don4,h_don0).
great_h_don(h_don5,h_don0).
great_h_don(h_don6,h_don0).
great_h_don(h_don7,h_don0).
great_h_don(h_don8,h_don0).
great_h_don(h_don9,h_don0).
great_h_don(h_don2,h_don1).
great_h_don(h_don3,h_don1).
great_h_don(h_don4,h_don1).
great_h_don(h_don5,h_don1).
great_h_don(h_don6,h_don1).
great_h_don(h_don7,h_don1).
great_h_don(h_don8,h_don1).
great_h_don(h_don9,h_don1).
great_h_don(h_don3,h_don2).
great_h_don(h_don4,h_don2).
great_h_don(h_don5,h_don2).
great_h_don(h_don6,h_don2).
great_h_don(h_don7,h_don2).
great_h_don(h_don8,h_don2).
great_h_don(h_don9,h_don2).
great_h_don(h_don4,h_don3).
great_h_don(h_don5,h_don3).
great_h_don(h_don6,h_don3).
great_h_don(h_don7,h_don3).
great_h_don(h_don8,h_don3).
great_h_don(h_don9,h_don3).
great_h_don(h_don5,h_don4).
great_h_don(h_don6,h_don4).
great_h_don(h_don7,h_don4).
great_h_don(h_don8,h_don4).
great_h_don(h_don9,h_don4).
great_h_don(h_don6,h_don5).
great_h_don(h_don7,h_don5).
great_h_don(h_don8,h_don5).
great_h_don(h_don9,h_don5).
great_h_don(h_don7,h_don6).
great_h_don(h_don8,h_don6).
great_h_don(h_don9,h_don6).
great_h_don(h_don8,h_don7).
great_h_don(h_don9,h_don7).
great_h_don(h_don9,h_don8).

great0_h_don(h_don1).
great0_h_don(h_don2).
great0_h_don(h_don3).
great0_h_don(h_don4).
great0_h_don(h_don5).
great0_h_don(h_don6).
great0_h_don(h_don7).
great0_h_don(h_don8).
great0_h_don(h_don9).
great1_h_don(h_don2).
great1_h_don(h_don3).
great1_h_don(h_don4).
great1_h_don(h_don5).
great1_h_don(h_don6).
great1_h_don(h_don7).
great1_h_don(h_don8).
great1_h_don(h_don9).
great2_h_don(h_don3).
great2_h_don(h_don4).
great2_h_don(h_don5).
great2_h_don(h_don6).
great2_h_don(h_don7).
great2_h_don(h_don8).
great2_h_don(h_don9).
great3_h_don(h_don4).
great3_h_don(h_don5).
great3_h_don(h_don6).
great3_h_don(h_don7).
great3_h_don(h_don8).
great3_h_don(h_don9).
great4_h_don(h_don5).
great4_h_don(h_don6).
great4_h_don(h_don7).
great4_h_don(h_don8).
great4_h_don(h_don9).
great5_h_don(h_don6).
great5_h_don(h_don7).
great5_h_don(h_don8).
great5_h_don(h_don9).
great6_h_don(h_don7).
great6_h_don(h_don8).
great6_h_don(h_don9).
great7_h_don(h_don8).
great7_h_don(h_don9).

less9_h_don(h_don0).
less9_h_don(h_don1).
less9_h_don(h_don2).
less9_h_don(h_don3).
less9_h_don(h_don4).
less9_h_don(h_don5).
less9_h_don(h_don6).
less9_h_don(h_don7).
less9_h_don(h_don8).
less8_h_don(h_don0).
less8_h_don(h_don1).
less8_h_don(h_don2).
less8_h_don(h_don3).
less8_h_don(h_don4).
less8_h_don(h_don5).
less8_h_don(h_don6).
less8_h_don(h_don7).
less7_h_don(h_don0).
less7_h_don(h_don1).
less7_h_don(h_don2).
less7_h_don(h_don3).
less7_h_don(h_don4).
less7_h_don(h_don5).
less7_h_don(h_don6).
less6_h_don(h_don0).
less6_h_don(h_don1).
less6_h_don(h_don2).
less6_h_don(h_don3).
less6_h_don(h_don4).
less6_h_don(h_don5).
less5_h_don(h_don0).
less5_h_don(h_don1).
less5_h_don(h_don2).
less5_h_don(h_don3).
less5_h_don(h_don4).
less4_h_don(h_don0).
less4_h_don(h_don1).
less4_h_don(h_don2).
less4_h_don(h_don3).
less3_h_don(h_don0).
less3_h_don(h_don1).
less3_h_don(h_don2).
less2_h_don(h_don0).
less2_h_don(h_don1).

great_h_acc(h_acc1,h_acc0).
great_h_acc(h_acc2,h_acc0).
great_h_acc(h_acc3,h_acc0).
great_h_acc(h_acc4,h_acc0).
great_h_acc(h_acc5,h_acc0).
great_h_acc(h_acc6,h_acc0).
great_h_acc(h_acc7,h_acc0).
great_h_acc(h_acc8,h_acc0).
great_h_acc(h_acc9,h_acc0).
great_h_acc(h_acc2,h_acc1).
great_h_acc(h_acc3,h_acc1).
great_h_acc(h_acc4,h_acc1).
great_h_acc(h_acc5,h_acc1).
great_h_acc(h_acc6,h_acc1).
great_h_acc(h_acc7,h_acc1).
great_h_acc(h_acc8,h_acc1).
great_h_acc(h_acc9,h_acc1).
great_h_acc(h_acc3,h_acc2).
great_h_acc(h_acc4,h_acc2).
great_h_acc(h_acc5,h_acc2).
great_h_acc(h_acc6,h_acc2).
great_h_acc(h_acc7,h_acc2).
great_h_acc(h_acc8,h_acc2).
great_h_acc(h_acc9,h_acc2).
great_h_acc(h_acc4,h_acc3).
great_h_acc(h_acc5,h_acc3).
great_h_acc(h_acc6,h_acc3).
great_h_acc(h_acc7,h_acc3).
great_h_acc(h_acc8,h_acc3).
great_h_acc(h_acc9,h_acc3).
great_h_acc(h_acc5,h_acc4).
great_h_acc(h_acc6,h_acc4).
great_h_acc(h_acc7,h_acc4).
great_h_acc(h_acc8,h_acc4).
great_h_acc(h_acc9,h_acc4).
great_h_acc(h_acc6,h_acc5).
great_h_acc(h_acc7,h_acc5).
great_h_acc(h_acc8,h_acc5).
great_h_acc(h_acc9,h_acc5).
great_h_acc(h_acc7,h_acc6).
great_h_acc(h_acc8,h_acc6).
great_h_acc(h_acc9,h_acc6).
great_h_acc(h_acc8,h_acc7).
great_h_acc(h_acc9,h_acc7).
great_h_acc(h_acc9,h_acc8).

great0_h_acc(h_acc1).
great0_h_acc(h_acc2).
great0_h_acc(h_acc3).
great0_h_acc(h_acc4).
great0_h_acc(h_acc5).
great0_h_acc(h_acc6).
great0_h_acc(h_acc7).
great0_h_acc(h_acc8).
great0_h_acc(h_acc9).
great1_h_acc(h_acc2).
great1_h_acc(h_acc3).
great1_h_acc(h_acc4).
great1_h_acc(h_acc5).
great1_h_acc(h_acc6).
great1_h_acc(h_acc7).
great1_h_acc(h_acc8).
great1_h_acc(h_acc9).
great2_h_acc(h_acc3).
great2_h_acc(h_acc4).
great2_h_acc(h_acc5).
great2_h_acc(h_acc6).
great2_h_acc(h_acc7).
great2_h_acc(h_acc8).
great2_h_acc(h_acc9).
great3_h_acc(h_acc4).
great3_h_acc(h_acc5).
great3_h_acc(h_acc6).
great3_h_acc(h_acc7).
great3_h_acc(h_acc8).
great3_h_acc(h_acc9).
great4_h_acc(h_acc5).
great4_h_acc(h_acc6).
great4_h_acc(h_acc7).
great4_h_acc(h_acc8).
great4_h_acc(h_acc9).
great5_h_acc(h_acc6).
great5_h_acc(h_acc7).
great5_h_acc(h_acc8).
great5_h_acc(h_acc9).
great6_h_acc(h_acc7).
great6_h_acc(h_acc8).
great6_h_acc(h_acc9).
great7_h_acc(h_acc8).
great7_h_acc(h_acc9).

less9_h_acc(h_acc0).
less9_h_acc(h_acc1).
less9_h_acc(h_acc2).
less9_h_acc(h_acc3).
less9_h_acc(h_acc4).
less9_h_acc(h_acc5).
less9_h_acc(h_acc6).
less9_h_acc(h_acc7).
less9_h_acc(h_acc8).
less8_h_acc(h_acc0).
less8_h_acc(h_acc1).
less8_h_acc(h_acc2).
less8_h_acc(h_acc3).
less8_h_acc(h_acc4).
less8_h_acc(h_acc5).
less8_h_acc(h_acc6).
less8_h_acc(h_acc7).
less7_h_acc(h_acc0).
less7_h_acc(h_acc1).
less7_h_acc(h_acc2).
less7_h_acc(h_acc3).
less7_h_acc(h_acc4).
less7_h_acc(h_acc5).
less7_h_acc(h_acc6).
less6_h_acc(h_acc0).
less6_h_acc(h_acc1).
less6_h_acc(h_acc2).
less6_h_acc(h_acc3).
less6_h_acc(h_acc4).
less6_h_acc(h_acc5).
less5_h_acc(h_acc0).
less5_h_acc(h_acc1).
less5_h_acc(h_acc2).
less5_h_acc(h_acc3).
less5_h_acc(h_acc4).
less4_h_acc(h_acc0).
less4_h_acc(h_acc1).
less4_h_acc(h_acc2).
less4_h_acc(h_acc3).
less3_h_acc(h_acc0).
less3_h_acc(h_acc1).
less3_h_acc(h_acc2).
less2_h_acc(h_acc0).
less2_h_acc(h_acc1).

great_pi_don(pi_don1,pi_don0).
great_pi_don(pi_don2,pi_don0).
great_pi_don(pi_don3,pi_don0).
great_pi_don(pi_don4,pi_don0).
great_pi_don(pi_don5,pi_don0).
great_pi_don(pi_don6,pi_don0).
great_pi_don(pi_don7,pi_don0).
great_pi_don(pi_don8,pi_don0).
great_pi_don(pi_don9,pi_don0).
great_pi_don(pi_don2,pi_don1).
great_pi_don(pi_don3,pi_don1).
great_pi_don(pi_don4,pi_don1).
great_pi_don(pi_don5,pi_don1).
great_pi_don(pi_don6,pi_don1).
great_pi_don(pi_don7,pi_don1).
great_pi_don(pi_don8,pi_don1).
great_pi_don(pi_don9,pi_don1).
great_pi_don(pi_don3,pi_don2).
great_pi_don(pi_don4,pi_don2).
great_pi_don(pi_don5,pi_don2).
great_pi_don(pi_don6,pi_don2).
great_pi_don(pi_don7,pi_don2).
great_pi_don(pi_don8,pi_don2).
great_pi_don(pi_don9,pi_don2).
great_pi_don(pi_don4,pi_don3).
great_pi_don(pi_don5,pi_don3).
great_pi_don(pi_don6,pi_don3).
great_pi_don(pi_don7,pi_don3).
great_pi_don(pi_don8,pi_don3).
great_pi_don(pi_don9,pi_don3).
great_pi_don(pi_don5,pi_don4).
great_pi_don(pi_don6,pi_don4).
great_pi_don(pi_don7,pi_don4).
great_pi_don(pi_don8,pi_don4).
great_pi_don(pi_don9,pi_don4).
great_pi_don(pi_don6,pi_don5).
great_pi_don(pi_don7,pi_don5).
great_pi_don(pi_don8,pi_don5).
great_pi_don(pi_don9,pi_don5).
great_pi_don(pi_don7,pi_don6).
great_pi_don(pi_don8,pi_don6).
great_pi_don(pi_don9,pi_don6).
great_pi_don(pi_don8,pi_don7).
great_pi_don(pi_don9,pi_don7).
great_pi_don(pi_don9,pi_don8).

great0_pi_don(pi_don1).
great0_pi_don(pi_don2).
great0_pi_don(pi_don3).
great0_pi_don(pi_don4).
great0_pi_don(pi_don5).
great0_pi_don(pi_don6).
great0_pi_don(pi_don7).
great0_pi_don(pi_don8).
great0_pi_don(pi_don9).
great1_pi_don(pi_don2).
great1_pi_don(pi_don3).
great1_pi_don(pi_don4).
great1_pi_don(pi_don5).
great1_pi_don(pi_don6).
great1_pi_don(pi_don7).
great1_pi_don(pi_don8).
great1_pi_don(pi_don9).
great2_pi_don(pi_don3).
great2_pi_don(pi_don4).
great2_pi_don(pi_don5).
great2_pi_don(pi_don6).
great2_pi_don(pi_don7).
great2_pi_don(pi_don8).
great2_pi_don(pi_don9).
great3_pi_don(pi_don4).
great3_pi_don(pi_don5).
great3_pi_don(pi_don6).
great3_pi_don(pi_don7).
great3_pi_don(pi_don8).
great3_pi_don(pi_don9).
great4_pi_don(pi_don5).
great4_pi_don(pi_don6).
great4_pi_don(pi_don7).
great4_pi_don(pi_don8).
great4_pi_don(pi_don9).
great5_pi_don(pi_don6).
great5_pi_don(pi_don7).
great5_pi_don(pi_don8).
great5_pi_don(pi_don9).
great6_pi_don(pi_don7).
great6_pi_don(pi_don8).
great6_pi_don(pi_don9).
great7_pi_don(pi_don8).
great7_pi_don(pi_don9).

less9_pi_don(pi_don0).
less9_pi_don(pi_don1).
less9_pi_don(pi_don2).
less9_pi_don(pi_don3).
less9_pi_don(pi_don4).
less9_pi_don(pi_don5).
less9_pi_don(pi_don6).
less9_pi_don(pi_don7).
less9_pi_don(pi_don8).
less8_pi_don(pi_don0).
less8_pi_don(pi_don1).
less8_pi_don(pi_don2).
less8_pi_don(pi_don3).
less8_pi_don(pi_don4).
less8_pi_don(pi_don5).
less8_pi_don(pi_don6).
less8_pi_don(pi_don7).
less7_pi_don(pi_don0).
less7_pi_don(pi_don1).
less7_pi_don(pi_don2).
less7_pi_don(pi_don3).
less7_pi_don(pi_don4).
less7_pi_don(pi_don5).
less7_pi_don(pi_don6).
less6_pi_don(pi_don0).
less6_pi_don(pi_don1).
less6_pi_don(pi_don2).
less6_pi_don(pi_don3).
less6_pi_don(pi_don4).
less6_pi_don(pi_don5).
less5_pi_don(pi_don0).
less5_pi_don(pi_don1).
less5_pi_don(pi_don2).
less5_pi_don(pi_don3).
less5_pi_don(pi_don4).
less4_pi_don(pi_don0).
less4_pi_don(pi_don1).
less4_pi_don(pi_don2).
less4_pi_don(pi_don3).
less3_pi_don(pi_don0).
less3_pi_don(pi_don1).
less3_pi_don(pi_don2).
less2_pi_don(pi_don0).
less2_pi_don(pi_don1).

great_pi_acc(pi_acc1,pi_acc0).
great_pi_acc(pi_acc2,pi_acc0).
great_pi_acc(pi_acc3,pi_acc0).
great_pi_acc(pi_acc4,pi_acc0).
great_pi_acc(pi_acc5,pi_acc0).
great_pi_acc(pi_acc6,pi_acc0).
great_pi_acc(pi_acc7,pi_acc0).
great_pi_acc(pi_acc8,pi_acc0).
great_pi_acc(pi_acc9,pi_acc0).
great_pi_acc(pi_acc2,pi_acc1).
great_pi_acc(pi_acc3,pi_acc1).
great_pi_acc(pi_acc4,pi_acc1).
great_pi_acc(pi_acc5,pi_acc1).
great_pi_acc(pi_acc6,pi_acc1).
great_pi_acc(pi_acc7,pi_acc1).
great_pi_acc(pi_acc8,pi_acc1).
great_pi_acc(pi_acc9,pi_acc1).
great_pi_acc(pi_acc3,pi_acc2).
great_pi_acc(pi_acc4,pi_acc2).
great_pi_acc(pi_acc5,pi_acc2).
great_pi_acc(pi_acc6,pi_acc2).
great_pi_acc(pi_acc7,pi_acc2).
great_pi_acc(pi_acc8,pi_acc2).
great_pi_acc(pi_acc9,pi_acc2).
great_pi_acc(pi_acc4,pi_acc3).
great_pi_acc(pi_acc5,pi_acc3).
great_pi_acc(pi_acc6,pi_acc3).
great_pi_acc(pi_acc7,pi_acc3).
great_pi_acc(pi_acc8,pi_acc3).
great_pi_acc(pi_acc9,pi_acc3).
great_pi_acc(pi_acc5,pi_acc4).
great_pi_acc(pi_acc6,pi_acc4).
great_pi_acc(pi_acc7,pi_acc4).
great_pi_acc(pi_acc8,pi_acc4).
great_pi_acc(pi_acc9,pi_acc4).
great_pi_acc(pi_acc6,pi_acc5).
great_pi_acc(pi_acc7,pi_acc5).
great_pi_acc(pi_acc8,pi_acc5).
great_pi_acc(pi_acc9,pi_acc5).
great_pi_acc(pi_acc7,pi_acc6).
great_pi_acc(pi_acc8,pi_acc6).
great_pi_acc(pi_acc9,pi_acc6).
great_pi_acc(pi_acc8,pi_acc7).
great_pi_acc(pi_acc9,pi_acc7).
great_pi_acc(pi_acc9,pi_acc8).

great0_pi_acc(pi_acc1).
great0_pi_acc(pi_acc2).
great0_pi_acc(pi_acc3).
great0_pi_acc(pi_acc4).
great0_pi_acc(pi_acc5).
great0_pi_acc(pi_acc6).
great0_pi_acc(pi_acc7).
great0_pi_acc(pi_acc8).
great0_pi_acc(pi_acc9).
great1_pi_acc(pi_acc2).
great1_pi_acc(pi_acc3).
great1_pi_acc(pi_acc4).
great1_pi_acc(pi_acc5).
great1_pi_acc(pi_acc6).
great1_pi_acc(pi_acc7).
great1_pi_acc(pi_acc8).
great1_pi_acc(pi_acc9).
great2_pi_acc(pi_acc3).
great2_pi_acc(pi_acc4).
great2_pi_acc(pi_acc5).
great2_pi_acc(pi_acc6).
great2_pi_acc(pi_acc7).
great2_pi_acc(pi_acc8).
great2_pi_acc(pi_acc9).
great3_pi_acc(pi_acc4).
great3_pi_acc(pi_acc5).
great3_pi_acc(pi_acc6).
great3_pi_acc(pi_acc7).
great3_pi_acc(pi_acc8).
great3_pi_acc(pi_acc9).
great4_pi_acc(pi_acc5).
great4_pi_acc(pi_acc6).
great4_pi_acc(pi_acc7).
great4_pi_acc(pi_acc8).
great4_pi_acc(pi_acc9).
great5_pi_acc(pi_acc6).
great5_pi_acc(pi_acc7).
great5_pi_acc(pi_acc8).
great5_pi_acc(pi_acc9).
great6_pi_acc(pi_acc7).
great6_pi_acc(pi_acc8).
great6_pi_acc(pi_acc9).
great7_pi_acc(pi_acc8).
great7_pi_acc(pi_acc9).

less9_pi_acc(pi_acc0).
less9_pi_acc(pi_acc1).
less9_pi_acc(pi_acc2).
less9_pi_acc(pi_acc3).
less9_pi_acc(pi_acc4).
less9_pi_acc(pi_acc5).
less9_pi_acc(pi_acc6).
less9_pi_acc(pi_acc7).
less9_pi_acc(pi_acc8).
less8_pi_acc(pi_acc0).
less8_pi_acc(pi_acc1).
less8_pi_acc(pi_acc2).
less8_pi_acc(pi_acc3).
less8_pi_acc(pi_acc4).
less8_pi_acc(pi_acc5).
less8_pi_acc(pi_acc6).
less8_pi_acc(pi_acc7).
less7_pi_acc(pi_acc0).
less7_pi_acc(pi_acc1).
less7_pi_acc(pi_acc2).
less7_pi_acc(pi_acc3).
less7_pi_acc(pi_acc4).
less7_pi_acc(pi_acc5).
less7_pi_acc(pi_acc6).
less6_pi_acc(pi_acc0).
less6_pi_acc(pi_acc1).
less6_pi_acc(pi_acc2).
less6_pi_acc(pi_acc3).
less6_pi_acc(pi_acc4).
less6_pi_acc(pi_acc5).
less5_pi_acc(pi_acc0).
less5_pi_acc(pi_acc1).
less5_pi_acc(pi_acc2).
less5_pi_acc(pi_acc3).
less5_pi_acc(pi_acc4).
less4_pi_acc(pi_acc0).
less4_pi_acc(pi_acc1).
less4_pi_acc(pi_acc2).
less4_pi_acc(pi_acc3).
less3_pi_acc(pi_acc0).
less3_pi_acc(pi_acc1).
less3_pi_acc(pi_acc2).
less2_pi_acc(pi_acc0).
less2_pi_acc(pi_acc1).

great_polari(polari1,polari0).
great_polari(polari2,polari0).
great_polari(polari3,polari0).
great_polari(polari4,polari0).
great_polari(polari5,polari0).
great_polari(polari6,polari0).
great_polari(polari7,polari0).
great_polari(polari8,polari0).
great_polari(polari9,polari0).
great_polari(polari2,polari1).
great_polari(polari3,polari1).
great_polari(polari4,polari1).
great_polari(polari5,polari1).
great_polari(polari6,polari1).
great_polari(polari7,polari1).
great_polari(polari8,polari1).
great_polari(polari9,polari1).
great_polari(polari3,polari2).
great_polari(polari4,polari2).
great_polari(polari5,polari2).
great_polari(polari6,polari2).
great_polari(polari7,polari2).
great_polari(polari8,polari2).
great_polari(polari9,polari2).
great_polari(polari4,polari3).
great_polari(polari5,polari3).
great_polari(polari6,polari3).
great_polari(polari7,polari3).
great_polari(polari8,polari3).
great_polari(polari9,polari3).
great_polari(polari5,polari4).
great_polari(polari6,polari4).
great_polari(polari7,polari4).
great_polari(polari8,polari4).
great_polari(polari9,polari4).
great_polari(polari6,polari5).
great_polari(polari7,polari5).
great_polari(polari8,polari5).
great_polari(polari9,polari5).
great_polari(polari7,polari6).
great_polari(polari8,polari6).
great_polari(polari9,polari6).
great_polari(polari8,polari7).
great_polari(polari9,polari7).
great_polari(polari9,polari8).

great0_polari(polari1).
great0_polari(polari2).
great0_polari(polari3).
great0_polari(polari4).
great0_polari(polari5).
great0_polari(polari6).
great0_polari(polari7).
great0_polari(polari8).
great0_polari(polari9).
great1_polari(polari2).
great1_polari(polari3).
great1_polari(polari4).
great1_polari(polari5).
great1_polari(polari6).
great1_polari(polari7).
great1_polari(polari8).
great1_polari(polari9).
great2_polari(polari3).
great2_polari(polari4).
great2_polari(polari5).
great2_polari(polari6).
great2_polari(polari7).
great2_polari(polari8).
great2_polari(polari9).
great3_polari(polari4).
great3_polari(polari5).
great3_polari(polari6).
great3_polari(polari7).
great3_polari(polari8).
great3_polari(polari9).
great4_polari(polari5).
great4_polari(polari6).
great4_polari(polari7).
great4_polari(polari8).
great4_polari(polari9).
great5_polari(polari6).
great5_polari(polari7).
great5_polari(polari8).
great5_polari(polari9).
great6_polari(polari7).
great6_polari(polari8).
great6_polari(polari9).
great7_polari(polari8).
great7_polari(polari9).

less9_polari(polari0).
less9_polari(polari1).
less9_polari(polari2).
less9_polari(polari3).
less9_polari(polari4).
less9_polari(polari5).
less9_polari(polari6).
less9_polari(polari7).
less9_polari(polari8).
less8_polari(polari0).
less8_polari(polari1).
less8_polari(polari2).
less8_polari(polari3).
less8_polari(polari4).
less8_polari(polari5).
less8_polari(polari6).
less8_polari(polari7).
less7_polari(polari0).
less7_polari(polari1).
less7_polari(polari2).
less7_polari(polari3).
less7_polari(polari4).
less7_polari(polari5).
less7_polari(polari6).
less6_polari(polari0).
less6_polari(polari1).
less6_polari(polari2).
less6_polari(polari3).
less6_polari(polari4).
less6_polari(polari5).
less5_polari(polari0).
less5_polari(polari1).
less5_polari(polari2).
less5_polari(polari3).
less5_polari(polari4).
less4_polari(polari0).
less4_polari(polari1).
less4_polari(polari2).
less4_polari(polari3).
less3_polari(polari0).
less3_polari(polari1).
less3_polari(polari2).
less2_polari(polari0).
less2_polari(polari1).

great_sigma(sigma1,sigma0).
great_sigma(sigma2,sigma0).
great_sigma(sigma3,sigma0).
great_sigma(sigma4,sigma0).
great_sigma(sigma5,sigma0).
great_sigma(sigma6,sigma0).
great_sigma(sigma7,sigma0).
great_sigma(sigma8,sigma0).
great_sigma(sigma9,sigma0).
great_sigma(sigma2,sigma1).
great_sigma(sigma3,sigma1).
great_sigma(sigma4,sigma1).
great_sigma(sigma5,sigma1).
great_sigma(sigma6,sigma1).
great_sigma(sigma7,sigma1).
great_sigma(sigma8,sigma1).
great_sigma(sigma9,sigma1).
great_sigma(sigma3,sigma2).
great_sigma(sigma4,sigma2).
great_sigma(sigma5,sigma2).
great_sigma(sigma6,sigma2).
great_sigma(sigma7,sigma2).
great_sigma(sigma8,sigma2).
great_sigma(sigma9,sigma2).
great_sigma(sigma4,sigma3).
great_sigma(sigma5,sigma3).
great_sigma(sigma6,sigma3).
great_sigma(sigma7,sigma3).
great_sigma(sigma8,sigma3).
great_sigma(sigma9,sigma3).
great_sigma(sigma5,sigma4).
great_sigma(sigma6,sigma4).
great_sigma(sigma7,sigma4).
great_sigma(sigma8,sigma4).
great_sigma(sigma9,sigma4).
great_sigma(sigma6,sigma5).
great_sigma(sigma7,sigma5).
great_sigma(sigma8,sigma5).
great_sigma(sigma9,sigma5).
great_sigma(sigma7,sigma6).
great_sigma(sigma8,sigma6).
great_sigma(sigma9,sigma6).
great_sigma(sigma8,sigma7).
great_sigma(sigma9,sigma7).
great_sigma(sigma9,sigma8).

great0_sigma(sigma1).
great0_sigma(sigma2).
great0_sigma(sigma3).
great0_sigma(sigma4).
great0_sigma(sigma5).
great0_sigma(sigma6).
great0_sigma(sigma7).
great0_sigma(sigma8).
great0_sigma(sigma9).
great1_sigma(sigma2).
great1_sigma(sigma3).
great1_sigma(sigma4).
great1_sigma(sigma5).
great1_sigma(sigma6).
great1_sigma(sigma7).
great1_sigma(sigma8).
great1_sigma(sigma9).
great2_sigma(sigma3).
great2_sigma(sigma4).
great2_sigma(sigma5).
great2_sigma(sigma6).
great2_sigma(sigma7).
great2_sigma(sigma8).
great2_sigma(sigma9).
great3_sigma(sigma4).
great3_sigma(sigma5).
great3_sigma(sigma6).
great3_sigma(sigma7).
great3_sigma(sigma8).
great3_sigma(sigma9).
great4_sigma(sigma5).
great4_sigma(sigma6).
great4_sigma(sigma7).
great4_sigma(sigma8).
great4_sigma(sigma9).
great5_sigma(sigma6).
great5_sigma(sigma7).
great5_sigma(sigma8).
great5_sigma(sigma9).
great6_sigma(sigma7).
great6_sigma(sigma8).
great6_sigma(sigma9).
great7_sigma(sigma8).
great7_sigma(sigma9).

less9_sigma(sigma0).
less9_sigma(sigma1).
less9_sigma(sigma2).
less9_sigma(sigma3).
less9_sigma(sigma4).
less9_sigma(sigma5).
less9_sigma(sigma6).
less9_sigma(sigma7).
less9_sigma(sigma8).
less8_sigma(sigma0).
less8_sigma(sigma1).
less8_sigma(sigma2).
less8_sigma(sigma3).
less8_sigma(sigma4).
less8_sigma(sigma5).
less8_sigma(sigma6).
less8_sigma(sigma7).
less7_sigma(sigma0).
less7_sigma(sigma1).
less7_sigma(sigma2).
less7_sigma(sigma3).
less7_sigma(sigma4).
less7_sigma(sigma5).
less7_sigma(sigma6).
less6_sigma(sigma0).
less6_sigma(sigma1).
less6_sigma(sigma2).
less6_sigma(sigma3).
less6_sigma(sigma4).
less6_sigma(sigma5).
less5_sigma(sigma0).
less5_sigma(sigma1).
less5_sigma(sigma2).
less5_sigma(sigma3).
less5_sigma(sigma4).
less4_sigma(sigma0).
less4_sigma(sigma1).
less4_sigma(sigma2).
less4_sigma(sigma3).
less3_sigma(sigma0).
less3_sigma(sigma1).
less3_sigma(sigma2).
less2_sigma(sigma0).
less2_sigma(sigma1).

