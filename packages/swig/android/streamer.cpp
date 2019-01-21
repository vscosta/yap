//
// Created by vsc on 7/6/17->
//
/* File : example->cxx */

#include "streamer.h"

extern "C" {

extern void Java_pt_up_yap_streamerJNI_swig_1module_1init(void);

}

static AndroidStreamer * streamerInstance = 0;

void setStreamer(AndroidStreamer* streamer) {
    streamerInstance = streamer;
    Java_pt_up_yap_streamerJNI_swig_1module_1init();
}

AndroidStreamer& getStreamer() {
    return *streamerInstance;
}

#include <gmpxx.h>

extern "C" {
#include <Yap.h>
#include <YapStreams.h>
#include <VFS.h>
#include <yapio.h>
#include <iopreds.h>

extern void Java_pt_up_yap_streamerJNI_swig_1module_1init(void);



static VFS_t *andstream;


static std::string buff0;

static void *
and_open(struct vfs *me, const char *name, const char *io_mode, int sno) {
    // we assume object is already open, so there is no need to open it.
    GLOBAL_Stream[sno].vfs_handle = streamerInstance;
    GLOBAL_Stream[sno].vfs = me;
    GLOBAL_Stream[sno].status = Append_Stream_f | Output_Stream_f;
    GLOBAL_Stream[sno].name = Yap_LookupAtom(name);
    buff0.clear(); // does not work?
    return streamerInstance;
}
}
static bool
and_close(int sno) {
  return true;
}

static int
and_put(int sno, int ch) {
    buff0 += ch;
    if (ch == '\n') {
        streamerInstance->display(buff0);
        buff0.clear();
    }

 return ch;
 }


static int
and_wput(int sno, int ch) {
    unsigned char b0[8];

    size_t extra = put_utf8(b0, ch);
    if (extra < 0)
        PlIOError(DOMAIN_ERROR_ENCODING, MkIntegerTerm(ch), "ch %C found at putw", ch);
    else if(extra==0)
        return false;
    for (int i=0; i < extra; i++) {
        buff0 += b0[i];
    }
    if (ch == '\n') {
        streamerInstance->display(buff0);
        buff0.clear();
    }

    return ch;
}

static int
and_get(int sno) {
    PlIOError(PERMISSION_ERROR_OUTPUT_STREAM, MkIntTerm(sno), "streamer is just for writing");
  return EOF;
}

static int64_t  and_seek(int sno, int64_t where, int how) {
  return EOF;
}

static void
and_flush(int sno) {

}


extern "C" {


void Java_pt_up_yap_streamerJNI_swig_1module_1init(void) {
    andstream = new VFS_t();

    andstream->name = "/android/user";
    andstream->vflags = VFS_CAN_WRITE | VFS_HAS_PREFIX;
    andstream->prefix = "/android";
    andstream->suffix = NULL;
    andstream->open = and_open;
    andstream->close = and_close;
    andstream->get_char = and_get;
    andstream->get_wchar = and_get;
    andstream->put_char = and_put;
    andstream->put_wchar = and_wput;
    andstream->flush = and_flush;
    andstream->seek = and_seek;
    andstream->next = GLOBAL_VFS;
    GLOBAL_VFS = andstream;
    Yap_InitStdStream(StdOutStream, Output_Stream_f | Append_Stream_f, NULL, andstream);
    Yap_InitStdStream(StdErrStream, Output_Stream_f | Append_Stream_f, NULL, andstream);    //streamerInstance = 0;
} ;


}
