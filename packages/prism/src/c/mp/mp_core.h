/* -*- c-basic-offset: 4 ; tab-width: 4 -*- */

#ifndef MP_CORE_H
#define MP_CORE_H

/*-------------------------------------------------------------------------*/

extern int mp_size;
extern int mp_rank;
extern int fd_dup_stdout;

/*-------------------------------------------------------------------------*/

void   mp_debug(const char *, ...);
NORET  mp_quit(int);

/*-------------------------------------------------------------------------*/

#endif /* MP_CORE_H */
