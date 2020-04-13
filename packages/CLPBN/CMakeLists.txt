CMAKE_MINIMUM_REQUIRED ( VERSION 2.8 )

PROJECT ( YAP_CLPBN CXX )

set (CLPBN_TOP
	clpbn.yap
	mlns.yap
	pfl.yap
	)

set( CLPBN_PL
	clpbn/aggregates.yap
	clpbn/bdd.yap
	clpbn/bnt.yap
	clpbn/connected.yap
	clpbn/discrete_utils.yap
	clpbn/display.yap
	clpbn/dists.yap
	clpbn/evidence.yap
	clpbn/gibbs.yap
	clpbn/graphs.yap
	clpbn/graphviz.yap
	clpbn/ground_factors.yap
	clpbn/hmm.yap
	clpbn/horus.yap
	clpbn/horus_ground.yap
	clpbn/horus_lifted.yap
	clpbn/jt.yap
	clpbn/matrix_cpt_utils.yap
	clpbn/numbers.yap
	clpbn/pgrammar.yap
	clpbn/table.yap
	clpbn/topsort.yap
	clpbn/utils.yap
	clpbn/ve.yap
	clpbn/viterbi.yap
	clpbn/vmap.yap
	clpbn/xbif.yap
	)

set( CLPBN_LEARNING_PROGRAMS
	learning/aleph_params.yap
	learning/bnt_parms.yap
	learning/em.yap
	learning/learn_utils.yap
	learning/learn_mln_wgts.yap
	learning/mle.yap
	)

set( CLPBN_EXAMPLES
  ex/burglary-alarm.fg
  ex/burglary-alarm.pfl
  ex/burglary-alarm.uai
  ex/city.pfl
  ex/comp_workshops.pfl
  ex/social_network1.pfl
  ex/social_network2.pfl
  ex/sprinkler.pfl
  ex/workshop_attrs.pfl
  )

set(
  CLPBN_SCHOOL_EXAMPLES
  ex/School/README
  ex/School/evidence_128.yap
  ex/School/parschema.pfl
  ex/School/school_128.yap
  ex/School/school_32.yap
  ex/School/school_64.yap
  ex/School/tables.yap
  )

set( CLPBN_HMMER_EXAMPLES
  ex/HMMer/Artemia.fa
  ex/HMMer/fasta.yap
  ex/HMMer/globin.hmm
  ex/HMMer/globin.yap
  ex/HMMer/plan7.yap
  ex/HMMer/scan.yap
  ex/HMMer/score.yap
  )

set(
  CLPBN_LEARNING_EXAMPLES
  ex/learning/debug_school.yap
  ex/learning/prof_params.pfl
  ex/learning/school_params.yap
  ex/learning/sprinkler_params.yap
  ex/learning/train.yap
  )

include(CheckCXXCompilerFlag)
CHECK_CXX_COMPILER_FLAG("-std=c++11" COMPILER_SUPPORTS_CXX11)
CHECK_CXX_COMPILER_FLAG("-std=c++0x" COMPILER_SUPPORTS_CXX0X)
if(COMPILER_SUPPORTS_CXX11)
  add_subDIRECTORY (horus)
  endif()

install(FILES
  ${CLPBN_TOP}
  DESTINATION ${YAP_INSTALL_DATADIR}
  )

install(FILES
  ${CLPBN_PL}
  DESTINATION ${YAP_INSTALL_DATADIR}/clpbn
  )

install(FILES
  ${CLPBN_LEARNING_PROGRAMS}
  DESTINATION ${YAP_INSTALL_DATADIR}/clpbn/learning
  )
