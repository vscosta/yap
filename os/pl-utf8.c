/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include <string.h>			/* get size_t */
#include "pl-utf8.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
UTF-8 Decoding, based on http://www.cl.cam.ac.uk/~mgk25/unicode.html
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define CONT(i)   ISUTF8_CB(in[i])
#define VAL(i, s) ((in[i]&0x3f) << s)

char *
_PL__utf8_get_char(const char *in, int *chr)
{ 					/* 2-byte, 0x80-0x7ff */
  if ( (in[0]&0xe0) == 0xc0 && CONT(1) )
  { *chr = ((in[0]&0x1f) << 6)|VAL(1,0);
    return (char *)in+2;
  }
					/* 3-byte, 0x800-0xffff */
  if ( (in[0]&0xf0) == 0xe0 && CONT(1) && CONT(2) )
  { *chr = ((in[0]&0xf) << 12)|VAL(1,6)|VAL(2,0);
    return (char *)in+3;
  }
					/* 4-byte, 0x10000-0x1FFFFF */
  if ( (in[0]&0xf8) == 0xf0 && CONT(1) && CONT(2) && CONT(3) )
  { *chr = ((in[0]&0x7) << 18)|VAL(1,12)|VAL(2,6)|VAL(3,0);
    return (char *)in+4;
  }
					/* 5-byte, 0x200000-0x3FFFFFF */
  if ( (in[0]&0xfc) == 0xf8 && CONT(1) && CONT(2) && CONT(3) && CONT(4) )
  { *chr = ((in[0]&0x3) << 24)|VAL(1,18)|VAL(2,12)|VAL(3,6)|VAL(4,0);
    return (char *)in+5;
  }
					/* 6-byte, 0x400000-0x7FFFFFF */
  if ( (in[0]&0xfe) == 0xfc && CONT(1) && CONT(2) && CONT(3) && CONT(4) && CONT(5) )
  { *chr = ((in[0]&0x1) << 30)|VAL(1,24)|VAL(2,18)|VAL(3,12)|VAL(4,6)|VAL(5,0);
    return (char *)in+4;
  }

  *chr = *in;

  return (char *)in+1;
}

unicode_type_t
_PL__utf8_type(const char *in0, size_t len)
{ 					/* 2-byte, 0x80-0x7ff */
  int chr;
  char *in = (char *) in0;
  int type = S_ASCII;

  while (in[0] != '\0' && in-in0 < len) {
    if ( (in[0]&0xe0) == 0xc0 && CONT(1) )
      { chr = ((in[0]&0x1f) << 6)|VAL(1,0);
	if (chr > 255) return S_WIDE;
	if (chr > 127) type = S_LATIN;
	in += 2;
	break;
      }
    /* 3-byte, 0x800-0xffff */
    if ( (in[0]&0xf0) == 0xe0 && CONT(1) && CONT(2) )
      { chr = ((in[0]&0xf) << 12)|VAL(1,6)|VAL(2,0);
	if (chr > 255) return S_WIDE;
	if (chr > 127) type = S_LATIN;
	in += 3;
      }
    /* 4-byte, 0x10000-0x1FFFFF */
    if ( (in[0]&0xf8) == 0xf0 && CONT(1) && CONT(2) && CONT(3) )
      { chr = ((in[0]&0x7) << 18)|VAL(1,12)|VAL(2,6)|VAL(3,0);
	if (chr > 255) return S_WIDE;
	if (chr > 127) type = S_LATIN;
	in += 4;
      }
    /* 5-byte, 0x200000-0x3FFFFFF */
    if ( (in[0]&0xfc) == 0xf8 && CONT(1) && CONT(2) && CONT(3) && CONT(4) )
      { chr = ((in[0]&0x3) << 24)|VAL(1,18)|VAL(2,12)|VAL(3,6)|VAL(4,0);
	if (chr > 255) return S_WIDE;
	if (chr > 127) type = S_LATIN;
	in += 5;
      }
    /* 6-byte, 0x400000-0x7FFFFFF */
    if ( (in[0]&0xfe) == 0xfc && CONT(1) && CONT(2) && CONT(3) && CONT(4) && CONT(5) )
      { chr = ((in[0]&0x1) << 30)|VAL(1,24)|VAL(2,18)|VAL(3,12)|VAL(4,6)|VAL(5,0);
	if (chr > 255) return S_WIDE;
	if (chr > 127) type = S_LATIN;
	in += 6;
      }
    in ++;
  }
  return type;
}


char *
_PL__utf8_put_char(char *out, int chr)
{ if ( chr < 0x80 )
  { *out++ = chr;
  } else if ( chr < 0x800 )
  { *out++ = 0xc0|((chr>>6)&0x1f);
    *out++ = 0x80|(chr&0x3f);
  } else if ( chr < 0x10000 )
  { *out++ = 0xe0|((chr>>12)&0x0f);
    *out++ = 0x80|((chr>>6)&0x3f);
    *out++ = 0x80|(chr&0x3f);
  } else if ( chr < 0x200000 )
  { *out++ = 0xf0|((chr>>18)&0x07);
    *out++ = 0x80|((chr>>12)&0x3f);
    *out++ = 0x80|((chr>>6)&0x3f);
    *out++ = 0x80|(chr&0x3f);
  } else if ( chr < 0x4000000 )
  { *out++ = 0xf8|((chr>>24)&0x03);
    *out++ = 0x80|((chr>>18)&0x3f);
    *out++ = 0x80|((chr>>12)&0x3f);
    *out++ = 0x80|((chr>>6)&0x3f);
    *out++ = 0x80|(chr&0x3f);
  } else if ( (unsigned)chr < 0x80000000 )
  { *out++ = 0xfc|((chr>>30)&0x01);
    *out++ = 0x80|((chr>>24)&0x3f);
    *out++ = 0x80|((chr>>18)&0x3f);
    *out++ = 0x80|((chr>>12)&0x3f);
    *out++ = 0x80|((chr>>6)&0x3f);
    *out++ = 0x80|(chr&0x3f);
  }

  return out;
}

char *
_PL__utf8_skip_char(const char *in)
{ 					/* 2-byte, 0x80-0x7ff */
  if ( (in[0]&0xe0) == 0xc0 && CONT(1) )
  {
    return (char *)in+2;
  }
					/* 3-byte, 0x800-0xffff */
  if ( (in[0]&0xf0) == 0xe0 && CONT(1) && CONT(2) )
  {
    return (char *)in+3;
  }
					/* 4-byte, 0x10000-0x1FFFFF */
  if ( (in[0]&0xf8) == 0xf0 && CONT(1) && CONT(2) && CONT(3) )
  {
    return (char *)in+4;
  }
					/* 5-byte, 0x200000-0x3FFFFFF */
  if ( (in[0]&0xfc) == 0xf8 && CONT(1) && CONT(2) && CONT(3) && CONT(4) )
  {
    return (char *)in+5;
  }
					/* 6-byte, 0x400000-0x7FFFFFF */
  if ( (in[0]&0xfe) == 0xfc && CONT(1) && CONT(2) && CONT(3) && CONT(4) && CONT(5) )
  {
    return (char *)in+4;
  }

  return (char *)in+1;
}


size_t
utf8_strlen(const char *s, size_t len)
{ const char *e = &s[len];
  unsigned int l = 0;

  while(s<e)
  { int chr;

    s = utf8_get_char(s, &chr);
    l++;
  }

  return l;
}

size_t
utf8_strlen1(const char *s)
{
  unsigned int l = 0;

  while( s [0] )
  {
    s = utf8_skip_char(s);
    l++;
  }

  return l;
}

const char *
utf8_skip(const char *s, int n)
{
  while(n--)
  {
    if (!s[0]) return NULL;
    s = utf8_skip_char(s);
  }

  return s;
}

int
utf8_strncmp(const char *s1, const char *s2, size_t n)
{

  while(n-- >0)
    { int chr1, chr2;

    s1 = utf8_get_char(s1, &chr1);
    s2 = utf8_get_char(s2, &chr2);
    if (chr1-chr2) return chr1-chr2;
    if (!chr1) return 0;
  }

  return 0;
}

int
utf8_strprefix(const char *s1, const char *s2)
{

  while(1)
    { int chr1, chr2;

    s1 = utf8_get_char(s1, &chr1);
    s2 = utf8_get_char(s2, &chr2);
    if (!chr2) return 1;
    if (chr1-chr2) return 0;
  }

  return 0;
}

char *
utf8_wcscpy(char *sf, const wchar_t *s0)
{
  char *sf0 = sf;
  while(1)
  { int chr1;

    chr1 = * s0++;
    if (chr1 == '\0') {
      *sf++ = '\0';
      return sf0;
    }
    sf = utf8_put_char(sf, chr1);
  }

  return NULL;
}
