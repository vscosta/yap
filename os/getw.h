
#define utf_cont(ch)  (((ch) & 0xc0) == 0x80)

#define encoding_error(ch,v,st) post_process_read_wchar(1, v, st)

static int post_process_f_weof(StreamDesc *st)
{
  if (ferror(st->file)) {
    clearerr(st->file);
    return 1;
  } else {
    return post_process_weof(st);
  }

}

/// compose a wide char from a sequence of getchars
///  this is a slow lane routine, called if no specialised code
///  isavailable.
extern int get_wchar(int sno) {
  StreamDesc *st = GLOBAL_Stream + sno;
  int ch = st->stream_getc(sno);

  if (ch == -1)
    return post_process_f_weof(st);

  switch (st->encoding) {
  case ENC_OCTET:
    // no error detection, all characters are ok.
  case ENC_ISO_LATIN1:
    return post_process_read_wchar(ch, 1, st);
    // 7 bits code, anything above is bad news
  case ENC_ISO_ASCII:
    if (ch & 0x80) {
      /* error */
    }
    return post_process_read_wchar(ch, 1, st);
    // default OS encoding, depends on locale.
  case ENC_ISO_ANSI: {
    char buf[8];
    int out;
    wchar_t wch;
    mbstate_t mbstate;

    memset((void *)&(mbstate), 0, sizeof(mbstate_t));
    buf[0] = ch;
    int n = 1;
    while ((out = mbrtowc(&wch, buf, 1, &(mbstate))) != 1) {
      int ch = buf[0] = st->stream_getc(sno);
      n++;
      if (ch == -1)
        return post_process_weof(st);
    }
    return post_process_read_wchar(wch, n, st);
  }
    // UTF-8 works o 8 bits.
  case ENC_ISO_UTF8: {
    int wch;
    unsigned char buf[8];

    if (ch < 0x80) {
      return post_process_read_wchar(ch, 1, st);
    }
    if ((ch - 0xc2) > (0xf4-0xc2)) {
      return encoding_error(ch, 1, st);
    }
    if (ch < 0xe0) { // 2-byte sequence
                     // Must have valid continuation character
      int c1 = buf[0] = st->stream_getc(sno);
      if (c1 == -1)
        return post_process_weof(st);
      if (!utf_cont(c1)) {
	return encoding_error(ch, 2, st);
      }
      wch = ((ch & 0x1f) << 6) | (c1 & 0x3f);
      return post_process_read_wchar(wch, 2, st);
    }
    if (ch < 0xf0) { // 3-byte sequence
      int c1 = st->stream_getc(sno);
      if (c1 == -1)
        return post_process_weof(st);
      //    return UTF8PROC_ERROR_INVALIDUTF8;
      if (ch == 0xed && c1 > 0x9f) {
        return encoding_error(ch, 1, st);
      }
      int c2 = st->stream_getc(sno);
      if (c2 == -1)
        return post_process_weof(st);
      if ( !utf_cont(c1) || !utf_cont(c2)) {
	return encoding_error(ch, 2, st);
	// Check for surrogate chars

      }
      wch = ((ch & 0xf) << 12) | ((c1 & 0x3f) << 6) | (c2 & 0x3f);
      return post_process_read_wchar(wch, 3, st);
    } else {
      int c1 = st->stream_getc(sno);
      if (c1 == -1)
        return post_process_weof(st);
      int c2 = st->stream_getc(sno);
      if (c2 == -1)
        return post_process_weof(st);
      int c3 = st->stream_getc(sno);
      if (c3 == -1)
        return post_process_weof(st);
      if ( !utf_cont(c1) || !utf_cont(c2) || !utf_cont(c3)) {
	return encoding_error(ch, 3, st);
      }
      wch = ((ch & 7) << 18) | ((c1 & 0x3f) << 12) | ((c2 & 0x3f) << 6) |
	(c3 & 0x3f);
      return post_process_read_wchar(wch, 4, st);
    }
  }
  case ENC_UTF16_BE: // check http://unicode.org/faq/utf_bom.html#utf16-3
      // big-endian: most significant octet first
    {
      unsigned int wch;
      int c1 = st->stream_getc(sno);
      if (c1 == -1)
        return post_process_weof(st);
      wch = (unsigned int)(c1 << 8) + ch;
      if (wch >= 0xd800 && wch < 0xdc00) {
        int c2 = st->stream_getc(sno);
        if (c2 == -1)
          return post_process_weof(st);
        int c3 = st->stream_getc(sno);
        if (c3 == -1)
          return post_process_weof(st);
        wch = wch + ((unsigned int)((unsigned int)((c3 << 8) + c2) << 8) + SURROGATE_OFFSET);
        return post_process_read_wchar(wch, 4, st);
      }
      return post_process_read_wchar(wch, 2, st);
    }

  case ENC_UTF16_LE: // check http://unicode.org/faq/utf_bom.html#utf16-3
      // little-endian: least significant octet first
  {
      unsigned int wch;
      int c1 = st->stream_getc(sno);
      if (c1 == -1)
        return post_process_weof(st);
      wch = (c1) + (ch << 8);
 //     printf("%d %c %d %d \n", wch, wch, ch, c1);
      if (wch >= 0xd800 && wch < 0xdc00) {
        int c3 = st->stream_getc(sno);
        if (c3 == -1)
          return post_process_weof(st);
        int c2 = st->stream_getc(sno);
        if (c2 == -1)
          return post_process_weof(st);
        wch = (((c3 << 8) + c2) << 8) + wch + SURROGATE_OFFSET;
        return post_process_read_wchar(wch, 4, st);
      }
      return post_process_read_wchar(wch, 2, st);
    }

  case ENC_UCS2_BE: // check http://unicode.org/faq/utf_bom.html#utf16-3
                    // big-endian: most significant byte first
    {
      unsigned int wch;
      int c1 = st->stream_getc(sno);
      if (c1 == -1)
        return post_process_weof(st);
      wch = (c1) + (ch << 8);
      return post_process_read_wchar(wch, 2, st);
    }

  case ENC_UCS2_LE: // check http://unicode.org/faq/utf_bom.html#utf16-3
                    // little-endian: least significant byte first
    {
      unsigned int wch;
      int c1 = st->stream_getc(sno);
      if (c1 == -1)
        return post_process_weof(st);
      wch = (c1 << 8) + ch;

      return post_process_read_wchar(wch, 2, st);
    }

  case ENC_ISO_UTF32_BE: // check http://unicode.org/faq/utf_bom.html#utf16-3
    // big-endian: from most to least significant
    {
      unsigned int wch = ch;
      {
        int c1 = st->stream_getc(sno);
        if (c1 == -1)
          return post_process_weof(st);
        wch = wch + (unsigned int)c1;
      }
      {
        int c1 = st->stream_getc(sno);
        if (c1 == -1)
          return post_process_weof(st);
        wch = (wch << 8) + (unsigned int)c1;
      }
      {
        int c1 = st->stream_getc(sno);
        if (c1 == -1)
          return post_process_weof(st);
        wch = (wch << 8) + (unsigned int)c1;
      }
      return post_process_read_wchar(wch, 4, st);
    }
  case ENC_ISO_UTF32_LE: // check http://unicode.org/faq/utf_bom.html#utf16-3
    // little-endian: from least to most significant
    {
      unsigned int wch = ch;
      {
        int c1 = st->stream_getc(sno);
        if (c1 == -1)
          return post_process_weof(st);
        wch += (unsigned int)(c1 << 8);
      }
      {
        int c1 = st->stream_getc(sno);
        if (c1 == -1)
          return post_process_weof(st);
        wch += (unsigned int)(c1 << 16);
      }
      {
        int c1 = st->stream_getc(sno);
        if (c1 == -1)
          return post_process_weof(st);
        wch += (unsigned int)(c1 << 24);
      }
      return post_process_read_wchar(wch, 4, st);
    }
  default:
    Yap_Error(SYSTEM_ERROR_OPERATING_SYSTEM, MkIntTerm(st->encoding),
              "Unsupported Encoding %d\n", st->encoding);
    return -1;
  }
}

extern int get_wchar_UTF8(int sno) {
  StreamDesc *st = GLOBAL_Stream + sno;
  int ch = st->stream_getc(sno);
  if (ch == -1)
    return post_process_weof(st);
  else {
    int wch;
    unsigned char buf[8];

    if (ch < 0x80) {
      return post_process_read_wchar(ch, 1, st);
    }
    // if ((ch - 0xc2) > (0xf4-0xc2)) return UTF8PROC_ERROR_INVALIDUTF8;
    if (ch < 0xe0) { // 2-byte sequence
                     // Must have valid continuation character
      int c1 = buf[0] = st->stream_getc(sno);
      if (c1 == -1)
        return post_process_weof(st);
      if (!utf_cont(c1)) {
	return encoding_error(ch, 2, st);
      }
      wch = ((ch & 0x1f) << 6) | (c1 & 0x3f);
      return post_process_read_wchar(wch, 2, st);
    }
    if (ch < 0xf0) { // 3-byte sequence
      // if ((str + 1 >= end) || !utf_cont(*str) || !utf_cont(str[1]))
      //   return UTF8PROC_ERROR_INVALIDUTF8;
      // Check for surrogate chars
      // if (ch == 0xed && *str > 0x9f)
      //    return UTF8PROC_ERROR_INVALIDUTF8;
      int c1 = st->stream_getc(sno);
      if (c1 == -1)
        return post_process_weof(st);
      if (ch == 0xed && c1 > 0x9f)
         return  encoding_error(ch, 2, st);
      int c2 = st->stream_getc(sno);
      if (c2 == -1)
        return post_process_weof(st);
     wch = ((ch & 0xf)<<12) | ((c1 & 0x3f)<<6) | (c2 & 0x3f);
     if (wch < 0x800)
         return encoding_error(ch, 3, st);
      return post_process_read_wchar(wch, 3, st);
    } else {
      int c1 = st->stream_getc(sno);
      if (c1 == -1)
	return post_process_weof(st);
      int c2 = st->stream_getc(sno);
      if (c2 == -1)
	return post_process_weof(st);
      int c3 = st->stream_getc(sno);
      if (c3 == -1)
	return post_process_weof(st);
   if (ch == 0xf0) {
    if (c1 < 0x90) return  encoding_error(ch, 4, st);
  } else if (c1 == 0xf4) {
    if (c2 > 0x8f) return  encoding_error(ch, 4, st);
  }
   wch = ((ch & 7)<<18) | ((c1 & 0x3f)<<12) | ((c2 & 0x3f)<<6) | (c3 & 0x3f);
      return post_process_read_wchar(wch, 4, st);
    }
  }
}
