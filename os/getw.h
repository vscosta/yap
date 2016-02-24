
/// compose a wide char from a sequence of getchars                                                
///  this is a slow lane routine, called if no specialised code
///  isavailable.
static int GETW(int sno) {
  StreamDesc *st = GLOBAL_Stream + sno;
  int ch = GETC();

  if (ch == -1)
    return post_process_weof(st);

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
    int n=1;
    while ((out = mbrtowc(&wch, buf, 1, &(mbstate))) != 1) {
      int ch = buf[0] = GETC();
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
  // if ((ch - 0xc2) > (0xf4-0xc2)) return UTF8PROC_ERROR_INVALIDUTF8;
  if (ch < 0xe0) {         // 2-byte sequence
     // Must have valid continuation character
    int c1 = buf[0] = GETC();
    if (c1 == -1)
        return post_process_weof(st);
    // if (!utf_cont(*str)) return UTF8PROC_ERROR_INVALIDUTF8;
    wch = ((ch & 0x1f)<<6) | (c1 & 0x3f);
        return post_process_read_wchar(wch, 2, st);
  }
  if (ch < 0xf0) {        // 3-byte sequence
    //if ((str + 1 >= end) || !utf_cont(*str) || !utf_cont(str[1]))
    //   return UTF8PROC_ERROR_INVALIDUTF8;
     // Check for surrogate chars
     //if (ch == 0xed && *str > 0x9f)
     //    return UTF8PROC_ERROR_INVALIDUTF8;
    int c1 = GETC();
    if (c1 == -1)
        return post_process_weof(st);
    int c2 =  GETC();
    if (c2 == -1)
        return post_process_weof(st);
    wch = ((ch & 0xf)<<12) | ((c1 & 0x3f)<<6) | (c2 & 0x3f);
        return post_process_read_wchar(wch, 3, st);
  } else {
    int c1 = GETC();
    if (c1 == -1)
        return post_process_weof(st);
    int c2 =  GETC();
    if (c2 == -1)
        return post_process_weof(st);
     int c3 =  GETC();
    if (c3 == -1)
        return post_process_weof(st);
    wch = ((ch & 7)<<18) | ((c1 & 0x3f)<<12) | ((c2 & 0x3f)<<6) | (c3 & 0x3f);
        return post_process_read_wchar(wch, 4, st);
  }
 }
case ENC_UTF16_LE: // check http://unicode.org/faq/utf_bom.html#utf16-3
                   // little-endian: start with big shot
  {
    int wch;
      int c1 = GETC();
  if (c1 == -1)
    return post_process_weof(st);
  wch = (c1 << 8) + ch;
  if (wch >= 0xd800 && wch < 0xdc00) {
    int c2 = GETC();
    if (c2 == -1)
      return post_process_weof(st);
    int c3 = GETC();
    if (c3 == -1)
      return post_process_weof(st);
    wch = wch + (((c3 << 8) + c2)<<wch) + SURROGATE_OFFSET;
        return post_process_read_wchar(wch, 4, st);
  }
    return post_process_read_wchar(wch, 2, st);
  }


case ENC_UTF16_BE: // check http://unicode.org/faq/utf_bom.html#utf16-3
                   // little-endian: start with big shot
  {
    int wch;
      int c1 = GETC();
  if (c1 == -1)
    return post_process_weof(st);
  wch = (c1) + (ch<<8);
  if (wch >= 0xd800 && wch < 0xdc00) {
    int c3 = GETC();
    if (c3 == -1)
      return post_process_weof(st);
    int c2 = GETC();
    if (c2 == -1)
      return post_process_weof(st);
    wch = (((c3 << 8) + c2) << 10) + wch + SURROGATE_OFFSET;
        return post_process_read_wchar(wch, 4, st);
  }
    return post_process_read_wchar(wch, 2, st);
  }
  
  case ENC_UCS2_BE: // check http://unicode.org/faq/utf_bom.html#utf16-3
                   // little-endian: start with big shot
  {
    int wch;
      int c1 = GETC();
  if (c1 == -1)
    return post_process_weof(st);
  wch = (c1) + (ch<<8);
    return post_process_read_wchar(wch, 2, st);
  }
  
  
case ENC_UCS2_LE: // check http://unicode.org/faq/utf_bom.html#utf16-3
                   // little-endian: start with big shot
  {
    int wch;
      int c1 = GETC();
  if (c1 == -1)
    return post_process_weof(st);
  wch = (c1 << 8) + ch;

    return post_process_read_wchar(wch, 2, st);
  }

case ENC_ISO_UTF32_BE: // check http://unicode.org/faq/utf_bom.html#utf16-3
  // little-endian: start with big shot
  {
    int wch = ch;
   {
      int c1 = GETC();
      if (c1 == -1)
        return post_process_weof(st);
      wch = wch + c1;
    }
   {
      int c1 = GETC();
      if (c1 == -1)
        return post_process_weof(st);
      wch = (wch << 8 )+c1; 
    }
   {
      int c1 = GETC();
      if (c1 == -1)
        return post_process_weof(st);
      wch = (wch << 8) +c1; 
   }
    return post_process_read_wchar(wch, 4, st);
  }
case ENC_ISO_UTF32_LE: // check http://unicode.org/faq/utf_bom.html#utf16-3
  // little-endian: start with big shot
  {
    int wch = ch;
   {
      int c1 = GETC();
      if (c1 == -1)
        return post_process_weof(st);
      wch +=  c1<<8;
    }
   {
      int c1 = GETC();
      if (c1 == -1)
        return post_process_weof(st);
      wch +=  c1<<16;
    }
   {
      int c1 = GETC();
      if (c1 == -1)
        return post_process_weof(st);
      wch +=  c1<<24;
   }
    return post_process_read_wchar(wch, 4, st);
  }
  }  
}
