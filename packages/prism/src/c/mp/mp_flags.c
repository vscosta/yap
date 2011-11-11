/* -*- c-basic-offset: 4 ; tab-width: 4 -*- */

#ifdef MPI

/*------------------------------------------------------------------------*/

#include "bprolog.h"
#include "up/flags.h"
#include <mpi.h>

/*------------------------------------------------------------------------*/

#define PUT(msg,pos,type,value) \
	MPI_Pack(&(value),1,(type),(msg),sizeof(msg),&(pos),MPI_COMM_WORLD)

#define GET(msg,pos,type,value) \
	MPI_Unpack((msg),sizeof(msg),&(pos),&(value),1,(type),MPI_COMM_WORLD)

/*------------------------------------------------------------------------*/

int pc_mpm_share_prism_flags_0(void)
{
    char  msg[256];
    int   pos = 0;

    PUT( msg , pos , MPI_INT    , daem              );
    PUT( msg , pos , MPI_INT    , em_message        );
    PUT( msg , pos , MPI_INT    , em_progress       );
    PUT( msg , pos , MPI_INT    , error_on_cycle    );
    PUT( msg , pos , MPI_INT    , fix_init_order    );
    PUT( msg , pos , MPI_INT    , init_method       );
    PUT( msg , pos , MPI_DOUBLE , itemp_init        );
    PUT( msg , pos , MPI_DOUBLE , itemp_rate        );
    PUT( msg , pos , MPI_INT    , log_scale         );
    PUT( msg , pos , MPI_INT    , max_iterate       );
    PUT( msg , pos , MPI_INT    , num_restart       );
    PUT( msg , pos , MPI_DOUBLE , prism_epsilon     );
    PUT( msg , pos , MPI_DOUBLE , std_ratio         );
    PUT( msg , pos , MPI_INT    , verb_em           );
    PUT( msg , pos , MPI_INT    , verb_graph        );
    PUT( msg , pos , MPI_INT    , warn              );

    MPI_Bcast(msg, sizeof(msg), MPI_PACKED, 0, MPI_COMM_WORLD);

    return BP_TRUE;
}

int pc_mps_share_prism_flags_0(void)
{
    char  msg[256];
    int   pos = 0;

    MPI_Bcast(msg, sizeof(msg), MPI_PACKED, 0, MPI_COMM_WORLD);

    GET( msg , pos , MPI_INT    , daem              );
    GET( msg , pos , MPI_INT    , em_message        );
    GET( msg , pos , MPI_INT    , em_progress       );
    GET( msg , pos , MPI_INT    , error_on_cycle    );
    GET( msg , pos , MPI_INT    , fix_init_order    );
    GET( msg , pos , MPI_INT    , init_method       );
    GET( msg , pos , MPI_DOUBLE , itemp_init        );
    GET( msg , pos , MPI_DOUBLE , itemp_rate        );
    GET( msg , pos , MPI_INT    , log_scale         );
    GET( msg , pos , MPI_INT    , max_iterate       );
    GET( msg , pos , MPI_INT    , num_restart       );
    GET( msg , pos , MPI_DOUBLE , prism_epsilon     );
    GET( msg , pos , MPI_DOUBLE , std_ratio         );
    GET( msg , pos , MPI_INT    , verb_em           );
    GET( msg , pos , MPI_INT    , verb_graph        );
    GET( msg , pos , MPI_INT    , warn              );

    return BP_TRUE;
}

/*------------------------------------------------------------------------*/

#endif /* MPI */
