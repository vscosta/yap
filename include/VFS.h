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
 * File:		VFS.h						 *
 * Last rev:	5/2/88							 *
 * mods: *
 * comments:	Virtual File System Access for YAP			 *
 *									 *
 *************************************************************************/
#ifndef VFS_H
#define VFS_H 1
#include <string.h>
#if HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#ifdef _WIN32
#include <stdint.h>
#ifndef uid_t
#define uid_t int
#endif
#ifndef gid_t
#define gid_t int
#endif
#endif

#include <encoding.h>

typedef struct {
  dev_t st_dev;                     /* ID of device containing file */
  mode_t st_mode;                   /* Mode of file (see below) */
  uid_t st_uid;                     /* User ID of the file */
  gid_t st_gid;                     /* Group ID of the file */
  struct timespec st_atimespec;     /* time of last access */
  struct timespec st_mtimespec;     /* time of last data modification */
  struct timespec st_ctimespec;     /* time of last status change */
  struct timespec st_birthtimespec; /* time of file creation(birth) */
#if __ANDROID__
  off64_t st_size; /* file size, in bytes */
#else
  off_t st_size; /* file size, in bytes */
#endif
} vfs_stat;


typedef enum vfs_flags {
  VFS_CAN_WRITE = 0x1,     /// we can write to files in this space
  VFS_CAN_EXEC = 0x2,      /// we can execute files in this space
  VFS_CAN_SEEK = 0x4,      /// we can seek within files in this space
  VFS_HAS_PREFIX = 0x8,    /// has a prefix that identifies a file in this space
  VFS_HAS_SUFFIX = 0x10,   /// has a suffix that describes the file.
  VFS_HAS_FUNCTION = 0x20 /// has a suffix that describes the file.
} vfs_flags_t;

typedef union {
  struct vfs *vfs;
  uintptr_t cell;
  size_t sz;
  void *pt;
  uintptr_t scalar;
#if __ANDROID__0
  AAssetManager *mgr;
  AAsset *asset;
#endif
} cell_size_t;

typedef struct vfs {
  const char *name; /// A text that explains the file system
  uintptr_t vflags; /// the main flags describing the operation of the Fs.
  /// a way to identify a file in this VFS: two special cases, prefix and suffix
  const char *prefix;
  const char *suffix;
  bool (*id)(struct vfs *me, const char *s);
  /** operations */
  void *(*open)(const char *s,
               const char *io_mode); /// open an object
  /// in this space, usual w,r,a,b flags plus B (store in a buffer)
  bool (*close)(int sno);   /// close the object
  int (*get_char)(int sno); /// get an octet to the stream
  int (*put_char)(int sno, int ch); /// output an octet to the stream
  void (*flush)(int sno); /// flush a stream
  int64_t (*seek)(int sno, int64_t offset,
                  int whence); /// jump around the stream
  void *(*opendir)(const char *s); /// open a directory object, if one exists
  const char *(*nextdir)(void *d); /// walk to the next entry in a directory object
  void (*closedir)(void *d);
  ; /// close access a directory object
  bool (*stat)(const char *s,
               vfs_stat *); /// obtain size, age, permissions of a file.
  bool (*isdir)(const char *s); /// verify whether is directory.
  bool (*exists)(const char *s); /// verify whether a file exists.
  bool (*chdir)(const char *s);      /// set working directory (may be virtual).
  encoding_t enc;                    /// default file encoded.
  YAP_Term (*parsers)(int sno); // a set of parsers that can read the
                                     // stream and generate a YAP_Term
  int (*writers)(int ch, int sno );
  /// convert a YAP_Term into this space
  const char *virtual_cwd;
  /** VFS dep
      endent area */
  cell_size_t priv[4];
  struct vfs *next;
} VFS_t;

extern VFS_t *GLOBAL_VFS;

extern void init_android_stream(void);

extern void Yap_InitStdStream(int sno, unsigned int flags, FILE *file, VFS_t *vfsp);

static inline VFS_t *vfs_owner(const char *fname) {
  VFS_t *me = GLOBAL_VFS;
  int d;
  size_t sz0 = strlen(fname), sz;

  while (me) {
    if ((me->vflags & VFS_HAS_PREFIX) && strstr(fname, me->prefix) == fname)
      return me;
    if (me->vflags & VFS_HAS_SUFFIX && (sz = strlen(me->suffix)) && (d = (sz0 - sz)) >= 0 &&
        strcmp(fname + d, me->suffix) == 0)
      return me;
    if (me->vflags & VFS_HAS_FUNCTION && (me->id(me, fname))) {
      return me;
    }
    me = me->next;
  }
  return NULL;
}

#endif
