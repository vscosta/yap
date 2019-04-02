################################################################
# SWI-Prolog CLPQR package
# Author:    Jan Wielemaker. jan@swi.psy.uva.nl
# Copyright: LGPL (see COPYING or www.gnu.org
################################################################


set (CLPRPRIV	clpr/bb_r.pl clpr/bv_r.pl 
		clpr/fourmotz_r.pl clpr/ineq_r.pl 
		clpr/itf_r.pl clpr/nf_r.pl 
		clpr/store_r.pl )
set(CLPQPRIV 	clpq/bb_q.pl clpq/bv_q.pl 
		clpq/fourmotz_q.pl clpq/ineq_q.pl 
		clpq/itf_q.pl clpq/nf_q.pl 
		clpq/store_q.pl)
set (CLPQRPRIV	clpqr/class.pl clpqr/dump.pl 
		clpqr/geler.pl clpqr/itf.pl
		clpqr/ordering.pl 
		clpqr/project.pl clpqr/redund.pl)
set (LIBPL	clpr.pl clpq.pl ${CLPRPRIV} ${CLPQPRIV} ${CLPQRPRIV}  )

install ( FILES	${CLPQPRIV} DESTINATION ${YAP_INSTALL_DATADIR}/clpq )
install ( FILES	${CLPRPRIV} DESTINATION ${YAP_INSTALL_DATADIR}/clpr )
install ( FILES	${CLPQRPRIV} DESTINATION ${YAP_INSTALL_DATADIR}/clpqr )
install ( FILES clpr.pl clpq.pl  DESTINATION ${YAP_INSTALL_DATADIR} )

#		$(PL) -q -f $(srcdir)/clpr_test.pl -g test,halt -t 'halt(1)'



