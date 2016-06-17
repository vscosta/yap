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
 * File:		assets.c						 *
 * Last rev:	5/2/88							 *
 * mods:									 *
 * comments:	Asset Support in ANDROID			 *
 *									 *
 *************************************************************************/
#ifdef SCCSA
static char SccsId[] = "%W% %G%";
#endif

/**
 * @file   assets.c
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Thu Nov 19 10:53:20 2015
 *
 * @brief  File Aliases
 *
 *
 */

#include <stdbool.h>
#include "sysbits.h"
// for native asset manager
#include <sys/types.h>

#if __ANDROID__


static AAssetManager  * getMgr(struct vfs *me)
{
  return me->priv[0].mgr;
}


void
Java_pt_up_yap_app_YAPDroid_load(JNIEnv *env,
                                                             jobject assetManager) {

    AAssetManager *mgr = AAssetManager_fromJava(env, assetManager);
    if (mgr == NULL) {
        return;
    }
    VFS_t *me = GLOBAL_VFS;
    while ( strcmp(me->name, "/assets") == 0)
      me = me->next;
    me->priv[0].mgr = mgr;

}

static bool
open_asset(struct vfs *me, struct stream_desc *st, const char *fname, const char
	   *io_mode)
{
  AAssetManager *mgr;
  int mode;
  const void *buf;
  if (strstr(fname,"/assets") == fname) {
    // we're in
    mgr = getMgr(me);
    if (mgr == NULL) {
      return PlIOError(PERMISSION_ERROR_OPEN_SOURCE_SINK,  TermNil,
		       "asset manager",
		       fname);
    }
    if (strchr(io_mode, 'w') ||  strchr(io_mode, 'a')) {
      return PlIOError(PERMISSION_ERROR_OPEN_SOURCE_SINK, TermNil,
		       "%s: no writing but flags are %s",
		       fname, io_mode);
    }
    if (strchr(io_mode, 'B'))
      mode = AASSET_MODE_BUFFER;
    else
      {
	mode = AASSET_MODE_UNKNOWN;
      }
    AAsset *a = AAssetManager_open(mgr , fname, mode);
    // try not to use it as an asset
    off64_t sz = AAsset_getLength64(a), sz0 = 0;
    int fd;
    if ((fd = AAsset_openFileDescriptor64(a, &sz0, &sz)) >= 0) {
      // can use it as red-only file
      st->file = fdopen( fd, "r");
      st->vfs = me;
      st->vfs_handle = a;
      return true;
    } else if ((buf = AAsset_getBuffer(a)) ) {
      // copy to memory
      bool rc = Yap_set_stream_to_buf(st, buf, sz);
      AAsset_close(a);
      return rc;
    }
    // should be done, but if not
    st->vfs_handle = a;
    st->vfs = me;
    return true;
  }
  if (me->next) {
    return me->next->open(me->next, st, fname, io_mode);
  }
  return NULL;
}

static bool
close_asset(struct stream_desc *st)
{
  AAsset_close(st->vfs_handle);
  return true;
}

static int64_t seek64(struct stream_desc *st,  int64_t offset, int whence)
{
  return AAsset_seek64(st->vfs_handle, offset, whence);
}

static int getc_asset(struct stream_desc *st)
{
  int ch;
  if ( AAsset_read (st->vfs_handle, &ch, 1) )
    return ch;
  return -1;
}


static void   *opendir_a(struct vfs *me, const char *dirName)
{
   return (void *)AAssetManager_openDir (getMgr(me), dirName);
}

static const char  *readdir_a(void  *dirHandle)
{
  return AAssetDir_getNextFileName ((AAssetDir  *)dirHandle);
}

static bool  closedir_a(void  *dirHandle)
{
   AAssetDir_close ((AAssetDir  *)dirHandle);
   return true;
}


static bool  stat_a(struct vfs *me, const char *fname, vfs_stat *out)
{
  struct stat64 bf;
  if (stat64( "/assets", &bf)) {
    
    out->st_dev = bf.st_dev;
    out->st_uid = bf.st_uid;
    out->st_gid = bf.st_gid;
    memcpy(&out->st_atimespec, (const void *)&out->st_atimespec, sizeof(struct timespec));
    memcpy(&out->st_mtimespec,(const void *) &out->st_mtimespec, sizeof(struct timespec));
    memcpy(&out->st_ctimespec,  (const void *)&out->st_ctimespec, sizeof(struct timespec));
    memcpy(&out->st_birthtimespec, (const void *)&out->st_birthtimespec, sizeof(struct timespec));
  }
  AAssetManager *mgr = getMgr(me);
    AAsset *a = AAssetManager_open(mgr , fname,  AASSET_MODE_UNKNOWN);
    // try not to use it as an asset
     out->st_size  = AAsset_getLength64(a);
  AAsset_close(a);
      return true;
 
}

static
bool is_dir_a(struct vfs *me, const char *dirName)
{
  bool rc;
    // try not to use it as an asset
    AAssetDir *d = AAssetManager_openDir (getMgr(me), dirName);
    if (d == NULL)
      return false;
    rc = (AAssetDir_getNextFileName(d) != NULL);
  AAssetDir_close(d);
      return rc;
}

static
bool exists_a(struct vfs *me, const char *dirName)
{
    // try not to use it as an asset
    AAsset *d = AAssetManager_open (getMgr(me), dirName,  AASSET_MODE_UNKNOWN);
    if (d == NULL)
      return false;
  AAsset_close(d);
      return true;
}


static bool set_cwd (struct vfs *me, const char *dirName) {

    chdir("/assets");
    if (me->virtual_cwd)
      free(me->virtual_cwd);
    me->virtual_cwd = malloc( sizeof(dirName) + 1 );
    return me!= NULL;
}

#endif


/* create a new alias arg for stream sno */
VFS_t *
Yap_InitAssetManager(void)
{

#if __ANDROID__
    VFS_t *me;
  /* init standard VFS */
  me = (VFS_t *)Yap_AllocCodeSpace(sizeof(struct vfs));
  me->name = "/assets";
  me->vflags = VFS_CAN_EXEC|VFS_CAN_SEEK|VFS_HAS_PREFIX;  /// the main flags describing the operation of the Fs.
  me->prefix = "/assets";
  /** operations */
  me->open = open_asset; /// open an object in this space
  me->close= close_asset;         /// close the object
  me->get_char = getc_asset;          /// get an octet to the stream
  me->putc = NULL;  /// output an octet to the stream
  me->seek = seek64;  /// jump around the stream
  me->opendir = opendir_a; /// open a directory object, if one exists
  me->nextdir = readdir_a; /// open a directory object, if one exists
  me->closedir = closedir_a;            /// close access a directory object
  me->stat = stat_a;		    /// obtain size, age, permissions of a file.
  me->isdir = is_dir_a;		    /// obtain size, age, permissions of a file.
  me->exists = exists_a;		    /// obtain size, age, permissions of a file.
  me->chdir = set_cwd;		    /// chnage working directory.
  me->enc = ENC_ISO_UTF8;			/// how the file is encoded.
  me->parsers = NULL;					/// a set of parsers that can read the stream and generate a term
  me->writers = NULL;
  LOCK(BGL);
  me-> next = GLOBAL_VFS;
  GLOBAL_VFS = me;
  return me;
  UNLOCK(BGL);
  return me;
#else
    return NULL;
#endif
}


