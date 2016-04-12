bplist00—_WebMainResource’	
_WebResourceTextEncodingName_WebResourceFrameName^WebResourceURL_WebResourceData_WebResourceMIMETypeUUTF-8P_Phttps://raw.githubusercontent.com/j-jorge/android-stdioext/master/src/fmemopen.cO#H<html><head></head><body><pre style="word-wrap: break-word; white-space: pre-wrap;">/* Copyright (C) 2007 Eric Blake
 * Permission to use, copy, modify, and distribute this software
 * is freely granted, provided that this notice is preserved.
 *
 * Modifications for Android written Jul 2009 by Alan Viverette
 */

/*
FUNCTION
&lt;&lt;fmemopen&gt;&gt;---open a stream around a fixed-length string

INDEX
	fmemopen

ANSI_SYNOPSIS
	#include &lt;stdio.h&gt;
	FILE *fmemopen(void *restrict &lt;[buf]&gt;, size_t &lt;[size]&gt;,
		       const char *restrict &lt;[mode]&gt;);

DESCRIPTION
&lt;&lt;fmemopen&gt;&gt; creates a seekable &lt;&lt;FILE&gt;&gt; stream that wraps a
fixed-length buffer of &lt;[size]&gt; bytes starting at &lt;[buf]&gt;.  The stream
is opened with &lt;[mode]&gt; treated as in &lt;&lt;fopen&gt;&gt;, where append mode
starts writing at the first NUL byte.  If &lt;[buf]&gt; is NULL, then
&lt;[size]&gt; bytes are automatically provided as if by &lt;&lt;malloc&gt;&gt;, with
the initial size of 0, and &lt;[mode]&gt; must contain &lt;&lt;+&gt;&gt; so that data
can be read after it is written.

The stream maintains a current position, which moves according to
bytes read or written, and which can be one past the end of the array.
The stream also maintains a current file size, which is never greater
than &lt;[size]&gt;.  If &lt;[mode]&gt; starts with &lt;&lt;r&gt;&gt;, the position starts at
&lt;&lt;0&gt;&gt;, and file size starts at &lt;[size]&gt; if &lt;[buf]&gt; was provided.  If
&lt;[mode]&gt; starts with &lt;&lt;w&gt;&gt;, the position and file size start at &lt;&lt;0&gt;&gt;,
and if &lt;[buf]&gt; was provided, the first byte is set to NUL.  If
&lt;[mode]&gt; starts with &lt;&lt;a&gt;&gt;, the position and file size start at the
location of the first NUL byte, or else &lt;[size]&gt; if &lt;[buf]&gt; was
provided.

When reading, NUL bytes have no significance, and reads cannot exceed
the current file size.  When writing, the file size can increase up to
&lt;[size]&gt; as needed, and NUL bytes may be embedded in the stream (see
&lt;&lt;open_memstream&gt;&gt; for an alternative that automatically enlarges the
buffer).  When the stream is flushed or closed after a write that
changed the file size, a NUL byte is written at the current position
if there is still room; if the stream is not also open for reading, a
NUL byte is additionally written at the last byte of &lt;[buf]&gt; when the
stream has exceeded &lt;[size]&gt;, so that a write-only &lt;[buf]&gt; is always
NUL-terminated when the stream is flushed or closed (and the initial
&lt;[size]&gt; should take this into account).  It is not possible to seek
outside the bounds of &lt;[size]&gt;.  A NUL byte written during a flush is
restored to its previous value when seeking elsewhere in the string.

RETURNS
The return value is an open FILE pointer on success.  On error,
&lt;&lt;NULL&gt;&gt; is returned, and &lt;&lt;errno&gt;&gt; will be set to EINVAL if &lt;[size]&gt;
is zero or &lt;[mode]&gt; is invalid, ENOMEM if &lt;[buf]&gt; was NULL and memory
could not be allocated, or EMFILE if too many streams are already
open.

PORTABILITY
This function is being added to POSIX 200x, but is not in POSIX 2001.

Supporting OS subroutines required: &lt;&lt;sbrk&gt;&gt;.
*/

#include &lt;stdlib.h&gt;
#include &lt;stdio.h&gt;
#include &lt;errno.h&gt;
#include &lt;string.h&gt;
#include &lt;malloc.h&gt;
#include "stdioext.h"

/* Describe details of an open memstream.  */
typedef struct fmemcookie {
  void *storage; /* storage to free on close */
  char *buf; /* buffer start */
  size_t pos; /* current position */
  size_t eof; /* current file size */
  size_t max; /* maximum file size */
  char append; /* nonzero if appending */
  char writeonly; /* 1 if write-only */
  char saved; /* saved character that lived at pos before write-only NUL */
} fmemcookie;

/* Read up to non-zero N bytes into BUF from stream described by
   COOKIE; return number of bytes read (0 on EOF).  */
static int
fmemread(void *cookie, char *buf, int n)
{
  fmemcookie *c = (fmemcookie *) cookie;
  /* Can't read beyond current size, but EOF condition is not an error.  */
  if (c-&gt;pos &gt; c-&gt;eof)
    return 0;
  if (n &gt;= c-&gt;eof - c-&gt;pos)
    n = c-&gt;eof - c-&gt;pos;
  memcpy (buf, c-&gt;buf + c-&gt;pos, n);
  c-&gt;pos += n;
  return n;
}

/* Write up to non-zero N bytes of BUF into the stream described by COOKIE,
   returning the number of bytes written or EOF on failure.  */
static int
fmemwrite(void *cookie, const char *buf, int n)
{
  fmemcookie *c = (fmemcookie *) cookie;
  int adjust = 0; /* true if at EOF, but still need to write NUL.  */

  /* Append always seeks to eof; otherwise, if we have previously done
     a seek beyond eof, ensure all intermediate bytes are NUL.  */
  if (c-&gt;append)
    c-&gt;pos = c-&gt;eof;
  else if (c-&gt;pos &gt; c-&gt;eof)
    memset (c-&gt;buf + c-&gt;eof, '\0', c-&gt;pos - c-&gt;eof);
  /* Do not write beyond EOF; saving room for NUL on write-only stream.  */
  if (c-&gt;pos + n &gt; c-&gt;max - c-&gt;writeonly)
    {
      adjust = c-&gt;writeonly;
      n = c-&gt;max - c-&gt;pos;
    }
  /* Now n is the number of bytes being modified, and adjust is 1 if
     the last byte is NUL instead of from buf.  Write a NUL if
     write-only; or if read-write, eof changed, and there is still
     room.  When we are within the file contents, remember what we
     overwrite so we can restore it if we seek elsewhere later.  */
  if (c-&gt;pos + n &gt; c-&gt;eof)
    {
      c-&gt;eof = c-&gt;pos + n;
      if (c-&gt;eof - adjust &lt; c-&gt;max)
	c-&gt;saved = c-&gt;buf[c-&gt;eof - adjust] = '\0';
    }
  else if (c-&gt;writeonly)
    {
      if (n)
	{
	  c-&gt;saved = c-&gt;buf[c-&gt;pos + n - adjust];
	  c-&gt;buf[c-&gt;pos + n - adjust] = '\0';
	}
      else
	adjust = 0;
    }
  c-&gt;pos += n;
  if (n - adjust)
    memcpy (c-&gt;buf + c-&gt;pos - n, buf, n - adjust);
  else
    {
      return EOF;
    }
  return n;
}

/* Seek to position POS relative to WHENCE within stream described by
   COOKIE; return resulting position or fail with EOF.  */
static fpos_t
fmemseek(void *cookie, fpos_t pos, int whence)
{
  fmemcookie *c = (fmemcookie *) cookie;
  off_t offset = (off_t) pos;

  if (whence == SEEK_CUR)
    offset += c-&gt;pos;
  else if (whence == SEEK_END)
    offset += c-&gt;eof;
  if (offset &lt; 0)
    {
      offset = -1;
    }
  else if (offset &gt; c-&gt;max)
    {
      offset = -1;
    }
  else
    {
      if (c-&gt;writeonly &amp;&amp; c-&gt;pos &lt; c-&gt;eof)
	{
	  c-&gt;buf[c-&gt;pos] = c-&gt;saved;
	  c-&gt;saved = '\0';
	}
      c-&gt;pos = offset;
      if (c-&gt;writeonly &amp;&amp; c-&gt;pos &lt; c-&gt;eof)
	{
	  c-&gt;saved = c-&gt;buf[c-&gt;pos];
	  c-&gt;buf[c-&gt;pos] = '\0';
	}
    }
  return (fpos_t) offset;
}

/* Reclaim resources used by stream described by COOKIE.  */
static int
fmemclose(void *cookie)
{
  fmemcookie *c = (fmemcookie *) cookie;
  free (c-&gt;storage);
  return 0;
}

/* Open a memstream around buffer BUF of SIZE bytes, using MODE.
   Return the new stream, or fail with NULL.  */
FILE *
fmemopen(void *buf, size_t size, const char *mode)
{
  FILE *fp;
  fmemcookie *c;
  int flags;
  int dummy;

  if ((flags = __sflags (mode, &amp;dummy)) == 0)
    return NULL;
  if (!size || !(buf || flags &amp; __SAPP))
    {
      return NULL;
    }
  if ((fp = (FILE *) __sfp ()) == NULL)
    return NULL;
  if ((c = (fmemcookie *) malloc (sizeof *c + (buf ? 0 : size))) == NULL)
    {
      fp-&gt;_flags = 0;		/* release */

      return NULL;
    }

  c-&gt;storage = c;
  c-&gt;max = size;
  /* 9 modes to worry about.  */
  /* w/a, buf or no buf: Guarantee a NUL after any file writes.  */
  c-&gt;writeonly = (flags &amp; __SWR) != 0;
  c-&gt;saved = '\0';
  if (!buf)
    {
      /* r+/w+/a+, and no buf: file starts empty.  */
      c-&gt;buf = (char *) (c + 1);
      *(char *) buf = '\0';
      c-&gt;pos = c-&gt;eof = 0;
      c-&gt;append = (flags &amp; __SAPP) != 0;
    }
  else
    {
      c-&gt;buf = (char *) buf;
      switch (*mode)
	{
	case 'a':
	  /* a/a+ and buf: position and size at first NUL.  */
	  buf = memchr (c-&gt;buf, '\0', size);
	  c-&gt;eof = c-&gt;pos = buf ? (char *) buf - c-&gt;buf : size;
	  if (!buf &amp;&amp; c-&gt;writeonly)
	    /* a: guarantee a NUL within size even if no writes.  */
	    c-&gt;buf[size - 1] = '\0';
	  c-&gt;append = 1;
	  break;
	case 'r':
	  /* r/r+ and buf: read at beginning, full size available.  */
	  c-&gt;pos = c-&gt;append = 0;
	  c-&gt;eof = size;
	  break;
	case 'w':
	  /* w/w+ and buf: write at beginning, truncate to empty.  */
	  c-&gt;pos = c-&gt;append = c-&gt;eof = 0;
	  *c-&gt;buf = '\0';
	  break;
	default:
	  abort();
	}
    }

  fp-&gt;_file = -1;
  fp-&gt;_flags = flags;
  fp-&gt;_cookie = c;
  fp-&gt;_read = flags &amp; (__SRD | __SRW) ? fmemread : NULL;
  fp-&gt;_write = flags &amp; (__SWR | __SRW) ? fmemwrite : NULL;
  fp-&gt;_seek = fmemseek;
  fp-&gt;_close = fmemclose;

  return fp;
}

</pre></body></html>Ztext/plain    ( F ] l ~ î ö õ Ó$:                           $E