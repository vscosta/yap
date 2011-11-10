/* -*- c-basic-offset: 4 ; tab-width: 4 -*- */

/* [27 Aug 2007, by yuizumi]
 * FIXME: mp_debug() is currently platform-dependent.
 */

#ifdef MPI

#include "up/up.h"
#include "mp/mp.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <sys/time.h>
#include <unistd.h> /* STDOUT_FILENO */
#include <mpi.h>

/* Currently mpprism works only on Linux systems. */
#define DEV_NULL "/dev/null"

/*-------------------------------------------------------------------------*/

int fd_dup_stdout = -1;

int mp_size;
int mp_rank;

/*-------------------------------------------------------------------------*/

static void close_stdout(void)
{
    fd_dup_stdout = dup(STDOUT_FILENO);

    if (fd_dup_stdout < 0)
        return;

    if (freopen(DEV_NULL, "w", stdout) == NULL) {
        close(fd_dup_stdout);
        fd_dup_stdout = -1;
    }
}

/*-------------------------------------------------------------------------*/

void mp_init(int *argc, char **argv[])
{
    MPI_Init(argc, argv);

    MPI_Comm_size(MPI_COMM_WORLD, &mp_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mp_rank);

    if (mp_size < 2) {
        printf("Two or more processes required to run mpprism.\n");
        MPI_Finalize();
        exit(1);
    }

    if (mp_rank > 0) {
        close_stdout();
    }
}

void mp_done(void)
{
    MPI_Finalize();
}

NORET mp_quit(int status)
{
    fprintf(stderr, "The system is aborted by Rank #%d.\n", mp_rank);
    MPI_Abort(MPI_COMM_WORLD, status);
    exit(status); /* should not reach here */
}

/*-------------------------------------------------------------------------*/

void mp_debug(const char *fmt, ...)
{
#ifdef MP_DEBUG
    char str[1024];
    va_list ap;
    struct timeval tv;
    int s, u;

    va_start(ap, fmt);
    vsnprintf(str, sizeof(str), fmt, ap);
    va_end(ap);

    gettimeofday(&tv, NULL);

    s = tv.tv_sec;
    u = tv.tv_usec;

    fprintf(stderr, "[RANK:%d] %02d:%02d:%02d.%03d -- %s\n",
            mp_rank, (s / 3600) % 24, (s / 60) % 60, s % 60, u / 1000, str);
#endif
}

/*-------------------------------------------------------------------------*/

#endif /* MPI */
