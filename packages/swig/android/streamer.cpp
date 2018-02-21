//
// Created by vsc on 7/6/17.
//
/* File : example.cxx */

#include "streamer.h"


static AndroidStreamer * streamerInstance = 0;

void setStreamer(AndroidStreamer* streamer) {
    streamerInstance = streamer;
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

extern void Java_pt_up_yap_streamerJNI_swig_1module_1init__(void);

static VFS_t andstream;

void Java_pt_up_yap_streamerJNI_swig_1module_1init__(void) {
 //   streamerInstance = 0;
} ;

static std::string buff0;

static void *
and_open(struct vfs *me, int sno, const char *name, const char *io_mode) {
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
 if (ch=='\n'  || buff0.length() == 128) { //buff0+= '\0';
     streamerInstance->display(buff0);
 }
 return ch;
 }

static int
and_get(int sno) {
  return EOF;
}

static int64_t  and_seek(int sno, int64_t where, int how) {
  return EOF;
}

static void
and_flush(int sno) {

buff0 += '\0';
streamerInstance->display(buff0);



//
// Created by vsc on 11-07-2017.
//

}

void
AndroidStreamer::bind() {
    buff0 = *new std::string[256];
    andstream.name = "/android/user_error";
    andstream.vflags = VFS_CAN_WRITE | VFS_HAS_PREFIX;
    andstream.prefix = "/android";
    andstream.suffix = NULL;
    andstream.open = and_open;
    andstream.close = and_close;
    andstream.get_char = and_get;
    andstream.put_char = and_put;
    andstream.flush = and_flush;
    andstream.seek = and_seek;
    andstream.next = GLOBAL_VFS;
    GLOBAL_VFS = &andstream;
    Yap_InitStdStream(StdOutStream, Output_Stream_f | Append_Stream_f, NULL, &andstream);
    Yap_InitStdStream(StdErrStream, Output_Stream_f | Append_Stream_f, NULL, &andstream);
}
