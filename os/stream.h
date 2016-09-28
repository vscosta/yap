/**************************************************************************
 *									 *
 * File:		stream.h *
 * Last rev:	5/2/88							 *
 * mods: *
 * comments:	Stream core routunes			 *
 *									 *
 *************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif

#if __ANDROID__
#undef HAVE_FMEMOPEN
#undef HAVE_OPEN_MEMSTREAM
#endif

#if _WIN32
#undef MAY_WRITE
#undef MAY_READ
#endif

typedef struct mem_desc {
  char *buf;    /* where the file is being read from/written to */
  int src;      /* where the space comes from, 0 code space, 1 malloc */
  Int max_size; /* maximum buffer size (may be changed dynamically) */
  UInt pos;     /* cursor */
  volatile void *error_handler;
} memHandle;

typedef struct stream_desc {
  Atom name;
  Term user_name;
  FILE *file;
  // useful in memory streams
  char *nbuf;
  size_t nsize;
  union {
    struct {
#define PLGETC_BUF_SIZE 4096
      unsigned char *buf, *ptr;
      int left;
    } file;
    memHandle mem_string;
    struct {
      int fd;
    } pipe;
#if HAVE_SOCKET
    struct {
      socket_domain domain;
      socket_info flags;
      int fd;
    } socket;
#endif
    struct {
      const unsigned char *buf, *ptr;
    } irl;
  } u;

  Int charcount, linecount, linepos;
  stream_flags_t status;
#if defined(YAPOR) || defined(THREADS)
  lockvar streamlock; /* protect stream access */
#endif
  int (*stream_putc)(
      int, int); /** function the stream uses for writing a single octet */
  int (*stream_wputc)(
      int, wchar_t); /** function the stream uses for writing a character */
  int (*stream_getc)(int); /** function the stream uses for reading an octet. */
  int (*stream_wgetc)(
      int);         /** function the stream uses for reading a character. */
  struct vfs *vfs;  /** stream belongs to a space */
  void *vfs_handle; /** direct handle to stream in that space. */
  int (*stream_wgetc_for_read)(
      int); /* function the stream uses for parser. It may be different
               from above if the ISO  character conversion is on */
  encoding_t encoding; /** current encoding for stream */
} StreamDesc;

