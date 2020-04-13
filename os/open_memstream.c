/* Open a write stream around a malloc'd string.
   Copyright (C) 2010 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* Written by Eric Blake <e...@byu.net>, 2010.  */

#include "YapConfig.h"

/* Specification.  */
#include <stdio.h>

#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

// #include "verify.h"

#if !HAVE_OPEN_MEMSTREAM && !_WIN32

#if !HAVE_FUNOPEN
#error Sorry, not ported to your platform yet
#else

FILE *open_memstream(char **buf, size_t *len);

#define INITIAL_ALLOC 64

struct data {
  char **buf;       /* User's argument.  */
  size_t *len;      /* User's argument.  Smaller of pos or eof.  */
  size_t pos;       /* Current position.  */
  size_t eof;       /* End-of-file position.  */
  size_t allocated; /* Allocated size of *buf, always > eof.  */
  char c; /* Temporary storage for byte overwritten by NUL, if pos < eof.  */
};
typedef struct data data;

/* Stupid BSD interface uses int/int instead of ssize_t/size_t.  */
// verify (sizeof (int) <= sizeof (size_t));
// verify (sizeof (int) <= sizeof (ssize_t));

static int mem_write(void *c, const char *buf, int n) {
  data *cookie = c;
  char *cbuf = *cookie->buf;

  /* Be sure we don't overflow.  */
  if ((ssize_t)(cookie->pos + n) < 0) {
    errno = EFBIG;
    return EOF;
  }
  /* Grow the buffer, if necessary.  Use geometric growth to avoid
     quadratic realloc behavior.  Overallocate, to accomodate the
     requirement to always place a trailing NUL not counted by length.
     Thus, we want max(prev_size*1.5, cookie->posn1).  */
  if (cookie->allocated <= cookie->pos + n) {
    size_t newsize = cookie->allocated * 3 / 2;
    if (newsize < cookie->pos + n + 1)
      newsize = cookie->pos + n + 1;
    cbuf = realloc(cbuf, newsize);
    if (!cbuf)
      return EOF;
    *cookie->buf = cbuf;
    cookie->allocated = newsize;
  }
  /* If we have previously done a seek beyond eof, ensure all
     intermediate bytges are NUL.  */
  if (cookie->eof < cookie->pos)
    memset(cbuf + cookie->eof, '\0', cookie->pos - cookie->eof);
  memmove(cbuf + cookie->pos, buf, n);
  cookie->pos += n;
  /* If the user has previously written beyond the current position,
     remember what the trailing NUL is overwriting.  Otherwise,
     extend the stream.  */
  if (cookie->eof < cookie->pos)
    cookie->eof = cookie->pos;
  else
    cookie->c = cbuf[cookie->pos];
  cbuf[cookie->pos] = '\0';
  *cookie->len = cookie->pos;
  return n;
}

static fpos_t mem_seek(void *c, fpos_t pos, int whence) {
  data *cookie = c;
  off_t offset = pos;

  if (whence == SEEK_CUR)
    offset = cookie->pos;
  else if (whence == SEEK_END)
    offset = cookie->eof;
  if (offset < 0) {
    errno = EINVAL;
    offset = -1;
  } else if ((size_t)offset != offset) {
    errno = ENOSPC;
    offset = -1;
  } else {
    if (cookie->pos < cookie->eof) {
      (*cookie->buf)[cookie->pos] = cookie->c;
      cookie->c = '\0';
    }
    cookie->pos = offset;
    if (cookie->pos < cookie->eof) {
      cookie->c = (*cookie->buf)[cookie->pos];
      (*cookie->buf)[cookie->pos] = '\0';
      *cookie->len = cookie->pos;
    } else
      *cookie->len = cookie->eof;
  }
  return offset;
}

static int mem_close(void *c) {
  data *cookie = c;
  char *buf;

  /* Be nice and try to reduce excess memory.  */
  buf = realloc(*cookie->buf, *cookie->len + 1);
  if (buf)
    *cookie->buf = buf;
  free(cookie);
  return 0;
}

FILE *open_memstream(char **buf, size_t *len) {
  FILE *f;
  data *cookie;

  if (!buf || !len) {
    errno = EINVAL;
    return NULL;
  }
  if (!(cookie = malloc(sizeof *cookie)))
    return NULL;
  if (!(*buf = malloc(INITIAL_ALLOC))) {
    free(cookie);
    errno = ENOMEM;
    return NULL;
  }
  **buf = '\0';
  *len = 0;

  f = funopen(cookie, NULL, mem_write, mem_seek, mem_close);
  if (!f) {
    int saved_errno = errno;
    free(cookie);
    errno = saved_errno;
  } else {
    cookie->buf = buf;
    cookie->len = len;
    cookie->pos = 0;
    cookie->eof = 0;
    cookie->c = '\0';
    cookie->allocated = INITIAL_ALLOC;
  }
  return f;
}
#endif /* HAVE_FUNOPEN */

#endif /* HAVE_OPEN_MEMSTREAM*/
