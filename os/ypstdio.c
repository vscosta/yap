/*************************************************************************
*									 *
*	 YAP Prolog 	%W% %G%
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		io.h							 *
* Last rev:	19/2/88							 *
* mods:									 *
* comments:	simple replacement for stdio				 *
*									 *
*************************************************************************/

#include "Yap.h"

#ifdef YAP_STDIO

#include <malloc.h>

#if HAVE_FCNTL_H
#include <fcntl.h>
#endif
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if WINDOWS
#include <io.h>
#endif
#include <stdarg.h>

#ifndef O_BINARY
#define O_BINARY 0
#endif

YP_FILE yp_iob[YP_MAX_FILES];

static void
clear_iob(YP_FILE *f)
{
  f->flags = f->cnt = 0;
  f->buflen = 1;
  f->ptr = f->base = (char *) &f->buf;
  f->close = close;
  f->read = read;
  f->write = write;
}

void
init_yp_stdio()
{
  int i;
  /* mark all descriptors as free */
  for(i=0; i<YP_MAX_FILES; ++i) {
    yp_iob[i].check = i;
    clear_iob(&yp_iob[i]);
  }
  /* initialize standard ones */
  yp_iob[0].fd = 0;
  yp_iob[0].flags = _YP_IO_FILE | _YP_IO_READ;
  yp_iob[1].fd = 1;
  yp_iob[1].flags = _YP_IO_FILE | _YP_IO_WRITE;
  yp_iob[2].fd = 2;
  yp_iob[2].flags = _YP_IO_FILE | _YP_IO_WRITE;
}

int
YP_fillbuf(YP_FILE *f)
{
  if (!(f->flags & _YP_IO_READ)||(f->flags & (_YP_IO_ERR|_YP_IO_EOF)))
    return -1;
  if ((f->cnt = (f->read)(f->fd,f->base,f->buflen)) < 0) {
    f->flags |= _YP_IO_ERR;
    return -1;
  }
  if (f->cnt==0) {
    f->flags |= _YP_IO_EOF;
    return -1;
  }
  f->ptr = f->base;
  return YP_getc(f);
}


int
YP_flushbuf(int c,YP_FILE *f)
{
  if(!(f->flags & _YP_IO_WRITE)||(f->flags & _YP_IO_ERR)) return -1;
  *(f->ptr++) = c;
  {
    int cnt = f->ptr-f->base;
    int r = (f->write)(f->fd,f->base,cnt);
    f->ptr = f->base;
    if (r!=cnt) {
      f->flags |= _YP_IO_ERR;
      return -1;
    }
    f->ptr = f->base;
    f->cnt = f->buflen-1;
  }
  return c;
}

int
YP_fflush(YP_FILE *f)
{
  if(!(f->flags & _YP_IO_WRITE)||(f->flags & _YP_IO_ERR)) return -1;
  if (f->ptr==f->base) return 0;
  {
    int cnt = f->ptr-f->base;
    int r = (f->write)(f->fd,f->base,cnt);
    f->ptr = f->base;
    if (r!=cnt) {
      f->flags |= _YP_IO_ERR;
      return -1;
    }
    f->ptr = f->base;
    f->cnt = f->buflen-1;
  }
  return 0;
}

int
YP_fputs(char *s, YP_FILE *f)
{
  int count = 0;
  while (*s) {
    if (putc(*s++,f)<0) return -1;
    ++count;
  }
  return count;
}

int
YP_puts(char *s)
{
  return YP_fputs(s,YP_stdout);
}


char *
YP_fgets(char *s, int n, YP_FILE *f)
{
  char *p=s;
  if (f->flags & _YP_IO_ERR) return 0;
  while(--n) {
    int ch = YP_getc(f);
    if (ch<0) return 0;
    *p++ = ch;
    if (ch=='\n') break;
  }
  *p = 0;
  return s;
}

char *
YP_gets(char *s)
{
  char *p=s;
  while(1) {
    int ch = YP_getchar();
    if (ch<0) return 0;
    if (ch=='\n') break;
    *p++ = ch;
  }
  *p = 0;
  return s;
}


YP_FILE*
YP_fopen(char *path, char *mode)
{
  YP_FILE *f = 0;
  int i, fd, flags, ch1, ch2;
  for(i=3; i<YP_MAX_FILES; ++i)
    if (!yp_iob[i].flags) {
      f = &yp_iob[i];
      break;
    }
  if (!f) return f;
  /* try to open the file */
  flags = 0;
  ch1 = *mode++;
  ch2 = *mode;
  if(ch2=='b') {
    flags = O_BINARY;
    ch2 = *++mode;
  }
  if (ch2) return 0;
  switch (ch1) {
  case 'r':
    flags |= O_RDONLY;
    break;
  case 'w':
    flags |= O_WRONLY | O_TRUNC | O_CREAT;
    break;
  case 'a':
    flags |= O_WRONLY | O_CREAT | O_APPEND;
    break;
  default:
    return 0;
  }
  if ((fd=open(path,flags,0644))<0) return 0;
  f->fd = fd;
  f->flags = _YP_IO_FILE | (ch1=='r' ? _YP_IO_READ : _YP_IO_WRITE);
  f->ptr = f->base;
  /* todo: add buffers */
  f->cnt = 0;
  f->close = close;
  f->read = read;
  f->write = write;
  return f;
}

int
YP_fclose(YP_FILE *f)
{
  if (f != &yp_iob[f->check]) return -1;
  if (f->flags & _YP_IO_WRITE) {
    YP_fflush(f);
  }
  (f->close)(f->fd);
  /* todo: release buffers */
  clear_iob(f);
  return 0;
}


#define MAXBSIZE 32768

int
YP_printf(char *format,...)
{
  va_list ap;
  char *buf = (char *) alloca(MAXBSIZE);
  int r;

  va_start(ap,format);
  vsprintf(buf,format,ap);
  r = YP_puts(buf);

  va_end(ap);

  return r;
}


int
YP_fprintf(YP_FILE *f, char *format,...)
{
  va_list ap;
  char *buf = (char *) alloca(MAXBSIZE);
  int r;

  va_start(ap,format);
  vsprintf(buf,format,ap);
  r = YP_fputs(buf,f);

  va_end(ap);

  return r;
}

int
YP_fileno(YP_FILE *f)
{
  return f->fd;
}

int
YP_clearerr(YP_FILE *f)
{
  f->flags &= ~ _YP_IO_ERR | _YP_IO_EOF;
  return 0;
}

int
YP_feof(YP_FILE *f)
{
  return f->flags & _YP_IO_EOF ? 1 : 0;
}

int
YP_setbuf(YP_FILE *f, char *b)
{
  return 0;
}

int
YP_fseek(YP_FILE *f, int offset, int whence)
{
  /* todo: implement fseek */
  return 0;
}

int
YP_ftell(YP_FILE*f)
{
  return 0;
}

#endif /* YAP_STDIO */

