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




static VFS_t *andstream;


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


extern "C" {


void Java_pt_up_yap_streamerJNI_swig_1module_1init(void) {
    if (andstream)
        return;
    andstream = new VFS_t();

    andstream->name = "android output window";
    andstream->vflags = VFS_CAN_WRITE | VFS_HAS_PREFIX;
    andstream->prefix = "/android";
    andstream->suffix = NULL;
    andstream->open = and_open;
    andstream->close = and_close;
    andstream->get_char = and_get;
    andstream->put_char = and_put;
    andstream->flush = and_flush;
    andstream->seek = and_seek;
    andstream->next = GLOBAL_VFS;
    GLOBAL_VFS = andstream;

    YAP_Term ts[1], args[1], goal;
    ts[0] = MkAtomTerm(Yap_LookupAtom("android"));
    args[0] = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("library"),1), 1, ts);
    goal = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("compile"),1), 1, args);
    YAP_RunGoalOnce(goal);
 }


}