
#include "Yap.h"
#include "Yatom.h"
#include "Heap.h"
#include "yapio.h"
#include "Foreign.h"

#if LOAD_SHL

#include <dl.h>
#include <malloc.h>
#include <stdio.h>

/*
 *   YAPFindExecutable(argv[0]) should be called on yap initialization to
 *   locate the executable of Yap
*/

void YAPFindExecutable(char *name)
{
  /* not really needed for shl version */
  strcpy( YapExecutable, "yap" );
}


/*
 * LoadForeign(ofiles,libs,proc_name,init_proc) dynamically loads foreign
 * code files and libraries and locates an initialization routine
*/

Int LoadForeign( StringList ofiles, StringList libs,
		 char *proc_name, YapInitProc *init_proc )
{

  /* *init_proc is initialised to NULL in load_foreign.c */
  int init_missing = -1;

  int n, i;
  struct shl_symbol *p;

  while( ofiles ) {
    int valid_fname;

    /* shl_load wants to follow the LD_CONFIG_PATH */
    valid_fname = TrueFileName( ofiles->s, FileNameBuf, TRUE );

    if( !valid_fname ) {
      strcpy( LoadMsg, "[ Trying to open non-existing file in LoadForeign ]" );
      return LOAD_FAILLED;
    }

    ofiles->handle = AllocCodeSpace( sizeof(shl_t) );
    *(shl_t *)ofiles->handle = shl_load( FileNameBuf, BIND_DEFERRED, 0 );
    if( *(shl_t *)ofiles->handle == NULL ) {
      strncpy( LoadMsg, strerror(errno), 512 );
      return LOAD_FAILLED;
    }

    if( init_missing ) {
      init_missing = shl_findsym( ofiles->handle, proc_name,
				  TYPE_PROCEDURE, init_proc );
    }

    ofiles = ofiles->next;
  }

  if( init_missing ) {
    strcpy( LoadMsg, "Could not locate initialization routine" );
    return LOAD_FAILLED;
  }

  while( libs ) {
    
    if( libs->s[0] == '-' ) {
      strcpy( FileNameBuf, "lib" );
      strcat( FileNameBuf, libs->s+2 );
      strcat( FileNameBuf, ".sl" );
    }
    else {
      strcpy( FileNameBuf, libs->s );
    }

    *(shl_t *)libs->handle = shl_load( FileNameBuf, BIND_DEFERRED, 0 );
    if( *(shl_t *)libs->handle == NULL ) {
      strncpy( LoadMsg, strerror(errno), 512 );
      return LOAD_FAILLED;
    }

    libs = libs->next;
  }

  return LOAD_SUCCEEDED;
}


void ShutdownLoadForeign( void )
{
  ForeignObj *f_code;
  int err;

  f_code = ForeignCodeLoaded;
  while( f_code != NULL ) {
    StringList objs, libs;
    
    objs = f_code->objs;
    while( objs ) {
      err = shl_unload( *(shl_t *)objs->handle );
      if( err ) {
	/* dunno how to properly report an error here */
	perror( NULL );
	return;
      }
      FreeCodeSpace( objs->handle );
      objs = objs->next;
    }

    libs = f_code->libs;
    while( libs ) {
      err = shl_unload( *(shl_t *)libs->handle );
      if( err ) {
	/* dunno how to properly report an error here */
	perror( NULL );
	return;
      }
      FreeCodeSpace( libs->handle );
      libs = libs->next;
    }
    f_code = f_code->next;
  }
}

Int ReLoadForeign(StringList ofiles, StringList libs,
		  char *proc_name, YapInitProc *init_proc)
{
  ShutdownLoadForeign();
  return( LoadForeign( ofiles, libs, proc_name, init_proc ) );
}

/*
dunno what this one is supposed to do, no load_* defines it
void	STD_PROTO(ReOpenLoadForeign,(void));
*/

#endif

