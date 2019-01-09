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

#include <jni.h>
#include <android/asset_manager.h>
#include <android/native_activity.h>


extern X_API void
Java_pt_up_yap_yapdroid_YAPDroid_loadAssetManager(JNIEnv *env, jclass clazz, jobject assetManager);

jobject *Yap_aref;
JNIEnv *Yap_env;

AAssetManager *Yap_assetManager(void)
{
    return AAssetManager_fromJava(Yap_env, Yap_aref);
}

X_API void
Java_pt_up_yap_yapdroid_YAPDroid_loadAssetManager(JNIEnv *env, jclass clazz, jobject assetManager) {
    Yap_aref = (*env)->NewGlobalRef(env,assetManager);
    Yap_env = env;
}


static void *
open_asset(VFS_t *me,  const char *fname, const char *io_mode, int sno) {
    int mode;
    const void *buf;

    AAsset *am = NULL;
    //__android_log_print(ANDROID_LOG_INFO, "YAPDroid", "open %s-%s <%s>", fname, me->prefix,io_mode);
    if (strchr(io_mode, 'B')) {
        mode = AASSET_MODE_BUFFER;
    } else {
        mode = AASSET_MODE_UNKNOWN;
    }
    GLOBAL_Stream[sno].name = Yap_LookupAtom(fname);
    fname += strlen(me->prefix)+1;
//    strcpy(dir, fname);
//    AAssetDir *dp = AAssetManager_openDir( Yap_assetManager(), dirname(dir) );
//    strcpy(dir, fname);
//    char *d = basename(dir);
    am = AAssetManager_open(Yap_assetManager(), fname, AASSET_MODE_UNKNOWN);
    //if (am==NULL)
    //        __android_log_print(ANDROID_LOG_INFO, "YAPDroid", "failed open %s <%s>", fname, strerror(errno) );
 __android_log_print(ANDROID_LOG_INFO, "YAPDroid", "open %s <%s>", fname, io_mode);
//    while (dp) {
//        char *f = AAssetDir_getNextFileName(dp);
//        __android_log_print(ANDROID_LOG_INFO, "YAPDroid", "open %s <%s>", fname, mode);
//        if (f && strcasecmp(d,f) == 0) {
//
//        }
//    }
    if (!am) {
        __android_log_print(ANDROID_LOG_INFO, "YAPDroid", "failed %s <%s>", fname, io_mode);

        return NULL;
    }
    // try not to use it as an asset
    off64_t sz = AAsset_getLength64(am), sz0 = 0;
    int fd;
    StreamDesc *st = GLOBAL_Stream + sno;
    if ((buf = AAsset_getBuffer(am))) {
        // copy to memory
        char *bf = malloc(sz);
        memmove(bf, buf, sz);
        bool rc = Yap_set_stream_to_buf(st, bf, sz);
        if (rc) AAsset_close(am);
        st->vfs = NULL;
        st->vfs_handle = NULL;
         st->status = InMemory_Stream_f|Seekable_Stream_f|Input_Stream_f;
         return st;
    } else if ((fd = AAsset_openFileDescriptor64(am, &sz0, &sz)) >= 0) {
        // can use it as read-only file
        st->file = fdopen(fd, "r");
        st->vfs = NULL;
        st->vfs_handle = NULL;
        st->status = Seekable_Stream_f|Input_Stream_f;
        return st->file;
    } else {
        // should be done, but if not
        GLOBAL_Stream[sno].vfs_handle = am;
        st->vfs = me;
        st->status = Input_Stream_f;
        return am;
    }
}

static bool
close_asset(int sno) {
    AAsset_close(GLOBAL_Stream[sno].vfs_handle);
    return true;
}

static int64_t seek64(int sno, int64_t offset, int whence) {
    return AAsset_seek64(GLOBAL_Stream[sno].vfs_handle, offset, whence);
}

static int getc_asset(int sno) {
    int ch;
    if (AAsset_read(GLOBAL_Stream[sno].vfs_handle, &ch, 1))
        return ch;
    return -1;
}


static void *opendir_a(VFS_t *me, const char *dirName) {
    dirName += strlen(me->prefix) + 1;
    return (void *) AAssetManager_openDir(Yap_assetManager(), dirName);
}

static const char *readdir_a(void *dirHandle) {
    return AAssetDir_getNextFileName((AAssetDir *) dirHandle);
}

static bool closedir_a(void *dirHandle) {
    AAssetDir_close((AAssetDir *) dirHandle);
    return true;
}


static bool stat_a(VFS_t *me, const char *fname, vfs_stat *out) {
    struct stat bf;
    fname += strlen(me->prefix) + 1;
    if (stat("/assets", &bf)) {
        out->st_mode = me   ->vflags ;
        out->st_dev = bf.st_dev;
        out->st_uid = bf.st_uid;
        out->st_gid = bf.st_gid;
        memmove(&out->st_atimespec, (const void *) &bf.st_atim, sizeof(struct timespec));
        memmove(&out->st_mtimespec, (const void *) &bf.st_mtim, sizeof(struct timespec));
        memmove(&out->st_ctimespec, (const void *) &bf.st_ctim, sizeof(struct timespec));
        memmove(&out->st_birthtimespec, (const void *) &bf.st_ctim,
               sizeof(struct timespec));
    }
    AAsset *a = AAssetManager_open(Yap_assetManager(), fname, AASSET_MODE_UNKNOWN);
    // try not to use it as an asset
    if (!a)
        return false;
    out->st_size = AAsset_getLength64(a);
    AAsset_close(a);
    return true;

}

static
bool is_dir_a(VFS_t *me, const char *dirName) {
    dirName += strlen(me->prefix)+1;
    if (dirName[0] == '\0')
        return true;
    // try not to use it as an asset
    AAssetDir *d = AAssetManager_openDir(Yap_assetManager(), dirName);
    if (d == NULL || AAssetDir_getNextFileName(d) == NULL)
        return false;
     (AAssetDir_close(d));
    //__android_log_print(ANDROID_LOG_INFO, "YAPDroid", "isdir %s <%p>", dirName, d);

    return true;
}

static
bool exists_a(VFS_t *me, const char *dirName) {
    dirName += strlen(me->prefix) + 1;
    // try not to use it as an asset
    AAsset *d = AAssetManager_open(Yap_assetManager(), dirName, AASSET_MODE_UNKNOWN);
    //__android_log_print(ANDROID_LOG_INFO, "YAPDroid", "exists %s <%p>", dirName, d);
    if (d == NULL)
        return false;
    AAsset_close(d);
    return true;
}


char *virtual_cwd;

static bool set_cwd(VFS_t *me, const char *dirName) {

    chdir("/assets");
    if (GLOBAL_cwd) {
        free(GLOBAL_cwd);
    }
    if (!is_dir_a(me,dirName))
        dirName = dirname(dirName);
    GLOBAL_cwd = malloc(strlen(dirName)+1);
    strcpy(GLOBAL_cwd, dirName);
//__android_log_print(ANDROID_LOG_INFO, "YAPDroid", "chdir %s", GLOBAL_cwd);
return true;
}

#endif


VFS_t *
Yap_InitAssetManager(void) {

#if __ANDROID__
    VFS_t *me;
    /* init standard VFS */
    me = (VFS_t *) Yap_AllocCodeSpace(sizeof(struct vfs));
    me->name = "/assets";
    me->vflags = VFS_CAN_EXEC | VFS_CAN_SEEK | VFS_CAN_READ |
                 VFS_HAS_PREFIX;  /// the main flags describing the operation of the Fs.
    me->prefix = "/assets";
    /** operations */
    me->open = open_asset; /// open an object in this space
    me->close = close_asset;         /// close the object
    me->get_char = getc_asset;          /// get an octet to the stream
    me->put_char = NULL;  /// output an octet to the stream
    me->seek = seek64;  /// jump around the stream
    me->opendir = opendir_a; /// open a directory object, if one exists
    me->nextdir = readdir_a; /// open a directory object, if one exists
    me->closedir = closedir_a;            /// close access a directory object
    me->stat = stat_a;            /// obtain size, age, permissions of a file.
    me->isdir = is_dir_a;            /// obtain size, age, permissions of a file.
    me->exists = exists_a;            /// obtain size, age, permissions of a file.
    me->chdir = set_cwd;            /// chnage working directory.
    me->enc = ENC_ISO_UTF8;            /// how the file is encoded.
    me->parsers = NULL;                    /// a set of parsers that can read the stream and generate a term
    me->writers = NULL;
    GLOBAL_cwd = NULL;
    LOCK(BGL);
    me->next = GLOBAL_VFS;
    GLOBAL_VFS = me;
    return me;
    UNLOCK(BGL);
    return me;
#else
    return NULL;
#endif
}


