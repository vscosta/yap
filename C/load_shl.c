
#include "Yap.h"
#include "Yatom.h"
#include "YapHeap.h"
#include "yapio.h"
#include "Foreign.h"

#if LOAD_SHL

#include <dl.h>
#include <malloc.h>
#include <stdio.h>

/*
 *   YAP_FindExecutable(argv[0]) should be called on yap initialization to
 *   locate the executable of Yap
*/

char * Yap_FindExecutable(void)
{
}


void *
Yap_LoadForeignFile(char *file, int flags)
{
  /* not implemented */
  return NULL;
}

int
Yap_CallForeignFile(void *handle, char *f)
{
  return FALSE;
}

int
Yap_CloseForeignFile(void *handle)
{
  return -1;
}


/*
 * LoadForeign(ofiles,libs,proc_name,init_proc) dynamically loads foreign
 * code files and libraries and locates an initialization routine
*/

static Int
LoadForeign( StringList ofiles, StringList libs,
		 char *proc_name, YapInitProc *init_proc )
{

  /* *init_proc is initialized to NULL in load_foreign.c */
  int init_missing = -1;

  int n, i;
  struct shl_symbol *p;

  while( ofiles ) {
    int valid_fname;

    /* shl_load wants to follow the LD_CONFIG_PATH */
    const char *file = AtomName(ofiles->name);
    valid_fname = Yap_findFile(file, NULL, NULL, LOCAL_FileNameBuf, true, YAP_OBJ, true, true);

    if( !valid_fname ) {
      strcpy( LOCAL_ErrorSay, "%% Trying to open non-existing file in LoadForeign" );
      return LOAD_FAILLED;
    }

    ofiles->handle = Yap_AllocCodeSpace( sizeof(shl_t) );
    *(shl_t *)ofiles->handle = shl_load( LOCAL_FileNameBuf, BIND_DEFERRED, 0 );
    if( *(shl_t *)ofiles->handle == NULL ) {
      strncpy( LOCAL_ErrorSay, strerror(errno), MAX_ERROR_MSG_SIZE );
      return LOAD_FAILLED;
    }

    if( init_missing ) {
      init_missing = shl_findsym( ofiles->handle, proc_name,
				  TYPE_PROCEDURE, init_proc );
    }

    ofiles = ofiles->next;
  }

  if( init_missing ) {
    strcpy( LOCAL_ErrorSay, "Could not locate initialization routine" );
    return LOAD_FAILLED;
  }

  while( libs ) {
    char *s = AtomName(lib->s);

    if( s[0] == '-' ) {
      strcpy( LOCAL_FileNameBuf, "lib" );
      strcat( LOCAL_FileNameBuf, s+2 );
      strcat( LOCAL_FileNameBuf, ".sl" );
    }
    else {
      strcpy( LOCAL_FileNameBuf, s );
    }

    *(shl_t *)libs->handle = shl_load( LOCAL_FileNameBuf, BIND_DEFERRED, 0 );
    if( *(shl_t *)libs->handle == NULL ) {
      strncpy( LOCAL_ErrorSay, strerror(errno), MAX_ERROR_MSG_SIZE );
      return LOAD_FAILLED;
    }

    libs = libs->next;
  }

  return LOAD_SUCCEEDED;
}


Int
Yap_LoadForeign(StringList ofiles, StringList libs,
	       char *proc_name,	YapInitProc *init_proc)
{
  return LoadForeign(ofiles, libs, proc_name, init_proc);
}

void
Yap_ShutdownLoadForeign( void )
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
      Yap_FreeCodeSpace( objs->handle );
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
      Yap_FreeCodeSpace( libs->handle );
      libs = libs->next;
    }
    f_code = f_code->next;
  }
}

Int
Yap_ReLoadForeign(StringList ofiles, StringList libs,
		  char *proc_name, YapInitProc *init_proc)
{
  ShutdownLoadForeign();
  return( LoadForeign( ofiles, libs, proc_name, init_proc ) );
}

/*
dunno what this one is supposed to do, no load_* defines it
void	 ReOpenLoadForeign(void);
*/

#endif

