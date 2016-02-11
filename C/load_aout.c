/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		load_aout.c						 *
* comments:	aout based dynamic loader of external routines		 *
*									 *
*************************************************************************/

#include "Yap.h"
#include "yapio.h"
#include "Foreign.h"

#ifdef A_OUT
this code is no being maintained anymore
#include <stdio.h>
#if STDC_HEADERS
#include <stdlib.h>
#endif
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif
#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#if HAVE_SYS_FILE_H
#include <sys/file.h>
#endif
#if HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#if HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#include <a.out.h>



#define oktox(n) \
	(0==stat(n,&stbuf)&&(stbuf.st_mode&S_IFMT)==S_IFREG&&0==access(n,X_OK))
#define oktow(n) \
	(0==stat(n,&stbuf)&&(stbuf.st_mode&S_IFMT)==S_IFDIR&&0==access(n,W_OK))

/*
 *   YAP_FindExecutable(argv[0]) should be called on yap initialization to
 *   locate the executable of Yap
*/
char *
Yap_FindExecutable(void)
{
  register char  *cp, *cp2;
  struct stat     stbuf;


  cp = (char *)getenv("PATH");
  if (cp == NULL)
    cp = ".:/usr/ucb:/bin:/usr/bin:/usr/local/bin";
  if (*GLOBAL_argv[0] == '/') {
    if (oktox(GLOBAL_argv[0])) {
      strcpy(LOCAL_FileNameBuf, GLOBAL_argv[0]);
      Yap_AbsoluteFileInBuffer(LOCAL_FileNameBuf, true);
      strncpy( GLOBAL_Executable, LOCAL_FileNameBuf, YAP_MAXPATHLEN);
      return;
    }
  }
  if (*cp == ':')
    cp++;
  for (; *cp;) {
    /*
     * copy over current directory and then append
     * argv[0] 
     */
      
    for (cp2 = LOCAL_FileNameBuf; (*cp) != 0 && (*cp) != ':';)
      *cp2++ = *cp++;
    *cp2++ = '/';
    strcpy(cp2, GLOBAL_argv[0]);
    if (*cp)
      cp++;
    if (!oktox(LOCAL_FileNameBuf))
      continue;
    Yap_AbsoluteFileInBuffer(Yap_AbsoluteFileInBuffer(LOCAL_FileNameBuf, GLOBAL_Executable, TRUE);
    return;
  }
  /* one last try for dual systems */
      strcpy(LOCAL_FileNameBuf, GLOBAL_argv[0]);
    Yap_AbsoluteFileInBuffer(Yap_AbsoluteFileInBuffer(LOCAL_FileNameBuf, GLOBAL_Executable, TRUE);
  if (oktox(GLOBAL_Executable))
    return GLOBAL_Executable;
  else
    Yap_Error(SYSTEM_ERROR_INTERNAL,MkAtomTerm(Yap_LookupAtom(GLOBAL_Executable)),
	  "cannot find file being executed");
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
static int
LoadForeign(StringList ofiles,
	    StringList libs,
	    char *proc_name,
	    YapInitProc *init_proc)
{
  char		  command[2*MAXPATHLEN];
  char            o_files[1024];    /* list of objects we want to load
				       */
  char            l_files[1024];    /* list of libraries we want to
				       load */ 
  char            tmp_buff[32] = "/tmp/YAP_TMP_XXXXXX";    /* used for
							 mktemp */
  char           *tfile;	    /* name of temporary file */
  int             fildes;	    /* temp file descriptor */
  struct exec     header;	    /* header for loaded file */
  unsigned long   loadImageSize, firstloadImSz;  /* size of image we will load */
  char           *FCodeBase;  /* where we load foreign code */

  /*
   * put in a string the names of the files you want to load and of any
   * libraries you want to use 
   */
  /* files first */
  *o_files = '\0';
  {
    StringList tmp = ofiles;

    while(tmp != NULL) {
      strcat(o_files," ");
      strcat(o_files,AtomName(tmp->name));
      tmp = tmp->next;
    }
  }
  /* same_trick for libraries */
  *l_files = '\0';
  {
    StringList tmp = libs;

    while(tmp != NULL) {
      strcat(l_files," ");
      strcat(l_files,AtomName(tmp->name));
      tmp = tmp->next;
    }
  }
  /* next, create a temp file to serve as loader output */
  tfile = mktemp(tmp_buff);

  /* prepare the magic */
  if (strlen(o_files) + strlen(l_files) + strlen(proc_name) +
	    strlen(GLOBAL_Executable) > 2*MAXPATHLEN) {
    strcpy(LOCAL_ErrorSay, " too many parameters in load_foreign/3 ");
    return LOAD_FAILLED;
  }
  sprintf(command, "/usr/bin/ld -N -A %s -o %s -u _%s %s %s -lc",
	  GLOBAL_Executable,
	  tfile, proc_name, o_files, l_files);
  /* now, do the magic */
  if (system(command) != 0) {
    unlink(tfile);
    strcpy(LOCAL_ErrorSay," ld returned error status in load_foreign_files ");
    return LOAD_FAILLED;
  }
  /* now check the music has played */
  if ((fildes = open(tfile, O_RDONLY)) < 0) {
    strcpy(LOCAL_ErrorSay," unable to open temp file in load_foreign_files ");
    return LOAD_FAILLED;
  }
  /* it did, get the mice */
  /* first, get the header */
  read(fildes, (char *) &header, sizeof(header));
  close(fildes);
  /* get the full size of what we need to load */
  loadImageSize = header.a_text + header.a_data + header.a_bss;
  /* add 16 just to play it safe */
  loadImageSize += 16;
  /* keep this copy */
  firstloadImSz = loadImageSize;
  /* now fetch the space we need */
  if (!(FCodeBase = Yap_AllocCodeSpace((int) loadImageSize))) {
    strcpy(LOCAL_ErrorSay," unable to allocate space for external code ");
    return LOAD_FAILLED;
  }
  /* now, a new incantation to load the new foreign code */
  sprintf(command, "/usr/bin/ld -N -A %s -T %lx -o %s -u _%s %s %s -lc",
	  GLOBAL_Executable,
	  (unsigned long) FCodeBase,
	  tfile, proc_name, o_files, l_files);
  /* and do it */ 
  if (system(command) != 0) {
    unlink(tfile);
    strcpy(LOCAL_ErrorSay," ld returned error status in load_foreign_files ");
    return LOAD_FAILLED;
  }
  if ((fildes = open(tfile, O_RDONLY)) < 0) {
    strcpy(LOCAL_ErrorSay," unable to open temp file in load_foreign_files ");
    return LOAD_FAILLED;
  }
  read(fildes, (char *) &header, sizeof(header));
  loadImageSize = header.a_text + header.a_data + header.a_bss;
  if (firstloadImSz < loadImageSize) {
    strcpy(LOCAL_ErrorSay," miscalculation in load_foreign/3 ");
    return LOAD_FAILLED;
  }
  /* now search for our init function */
  {
    char entry_fun[256];
    struct nlist    func_info[2];
    sprintf(entry_fun, "_%s", proc_name);
    func_info[0].n_un.n_name = entry_fun;
    func_info[1].n_un.n_name = NULL;
    if (nlist(tfile, func_info) == -1) {
      strcpy(LOCAL_ErrorSay," in nlist(3) ");
      return LOAD_FAILLED;
    }
    if (func_info[0].n_type == 0) {
      strcpy(LOCAL_ErrorSay," in nlist(3) ");
      return LOAD_FAILLED;
    }
    *init_proc = (YapInitProc)(func_info[0].n_value);
  }
  /* ok, we got our init point */
  /* now read our text */
  lseek(fildes, (long)(N_TXTOFF(header)), 0);
  {
    unsigned int u1 = header.a_text + header.a_data;
    read(fildes, (char *) FCodeBase, u1);
    /* zero the BSS segment */
    while (u1 < loadImageSize)
      FCodeBase[u1++] = 0;
  }
  close(fildes);
  unlink(tfile);
  return LOAD_SUCCEEDED;
}

Int
Yap_LoadForeign(StringList ofiles, StringList libs,
	       char *proc_name,	YapInitProc *init_proc)
{
  return LoadForeign(ofiles, libs, proc_name, init_proc);
}

void 
Yap_ShutdownLoadForeign(void)
{
}

Int
Yap_ReLoadForeign(StringList ofiles, StringList libs,
	       char *proc_name,	YapInitProc *init_proc)
{
  return(LoadForeign(ofiles,libs, proc_name, init_proc));
}


#endif




