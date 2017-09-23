/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		iopreds.c						 *
* Last rev:	5/2/88							 *
* mods:									 *
* comments:	Input/Output C implemented predicates			 *
*									 *
*************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif

/*
 * This file includes the definition of a miscellania of standard predicates
 * for yap refering to: Files and GLOBAL_Streams, Simple Input/Output,
 *
 */

#include "sysbits.h"

#if _MSC_VER || defined(__MINGW32__)
#define SYSTEM_STAT _stat
#else
#define SYSTEM_STAT stat
#endif

bool Yap_GetFileName(Term t, char *buf, size_t len, encoding_t enc) {
  while (IsApplTerm(t) && FunctorOfTerm(t) == FunctorSlash) {
    if (!Yap_GetFileName(ArgOfTerm(1, t), buf, len, enc))
      return false;
    size_t szl = strlen(buf);
    buf += szl;
    *buf++ = '/';
    t = ArgOfTerm(2, t);
    len -= (szl + 1);
  }
  return Yap_TextTermToText(t, buf, enc);
}

static Int file_name_extension(USES_REGS1) {
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);
  Term t3 = Deref(ARG3);
  char f[YAP_FILENAME_MAX + 1];
#if __APPLE__ || _WIN32
  bool lowcase = true;
#endif

  if (!IsVarTerm((t3))) {
    char *f2;
    if (!Yap_GetFileName(t3, f, YAP_FILENAME_MAX, ENC_ISO_UTF8)) {
      return false;
    }
    char *pts = strrchr(f, '/');
#if WIN32_
    char *pts1 = strrchr(f, '\\');
    if (pts11 > pts)
      pts = pts1;
#endif
    char *ss = strrchr(f, '.');
    if (pts > ss) {
      ss = f + strlen(f);
    } else if (ss == NULL) {
      ss = "";
    } else {
      ss++;
    }
    if (IsVarTerm(t2)) {
      Term t = Yap_MkTextTerm(ss, ENC_ISO_UTF8, t3);
      Yap_unify(t2, t);
    } else {
      f2 = ss + (strlen(ss) + 1);
      if (!Yap_TextTermToText(t2, f2, ENC_ISO_UTF8))
        return false;
#if __APPLE__ || _WIN32
      Yap_OverwriteUTF8BufferToLowCase(f2);
      lowcase = true;
#endif
      if (strcmp(f2, ss) != 0 && (ss > f && strcmp(f2, ss - 1) != 0)) {
        return false;
      }
    }
    if (f[0] && ss[0] && ss[0] != '.') {
      ss[-1] = '\0';
    }
    if (IsVarTerm(t1)) {
      Term t = Yap_MkTextTerm(f, ENC_ISO_UTF8, t3);
      Yap_unify(t1, t);
    } else {
      char f1[YAP_FILENAME_MAX + 1];
#if __APPLE || _WIN32
      Yap_OverwriteUTF8BufferToLowCase(f);
#endif
      if (!Yap_GetFileName(t2, f1, YAP_FILENAME_MAX, ENC_ISO_UTF8))
        return false;
#if __APPLE__ || _WIN32
      if (!lowcase)
        Yap_OverwriteUTF8BufferToLowCase(f2);
#endif
      if (strcmp(f1, f) != 0) {
        return false;
      }
    }
    return true;
  } else {
    char *f2;
    if (!Yap_TextTermToText(t1, f, ENC_ISO_UTF8)) {
      return false;
    }
    f2 = f + strlen(f);
    if (!Yap_TextTermToText(t2, f2, ENC_ISO_UTF8)) {
      return false;
    }
    if (f2[0] != '.') {
      memmove(f2 + 1, f2, strlen(f2) + 1);
      f2[0] = '.';
    }
    Term t = Yap_MkTextTerm(f, ENC_ISO_UTF8, t1);
    if (!t)
      return false;
    return Yap_unify(t, t3);
  }
}

static Int access_path(USES_REGS1) {
  Term tname = Deref(ARG1);

  if (IsVarTerm(tname)) {
    Yap_Error(INSTANTIATION_ERROR, tname, "access");
    return false;
  } else if (!IsAtomTerm(tname)) {
    Yap_Error(TYPE_ERROR_ATOM, tname, "access");
    return false;
  } else {
#if HAVE_STAT
    struct SYSTEM_STAT ss;
    char *file_name;

    file_name = RepAtom(AtomOfTerm(tname))->StrOfAE;
    if (SYSTEM_STAT(file_name, &ss) != 0) {
      /* ignore errors while checking a file */
      return true;
    }
    return true;
#else
    return false;
#endif
  }
}

static Int exists_file(USES_REGS1) {
  Term tname = Deref(ARG1);
  char *file_name;

  if (IsVarTerm(tname)) {
    Yap_Error(INSTANTIATION_ERROR, tname, "access");
    return FALSE;
  } else if (!IsAtomTerm(tname)) {
    Yap_Error(TYPE_ERROR_ATOM, tname, "access");
    return FALSE;
  } else {
#if HAVE_STAT
    struct SYSTEM_STAT ss;

    file_name = RepAtom(AtomOfTerm(tname))->StrOfAE;
    if (SYSTEM_STAT(file_name, &ss) != 0) {
      /* ignore errors while checking a file */
      return FALSE;
    }
#if _MSC_VER
    return ss.st_mode & S_IFREG;
#else
    return S_ISREG(ss.st_mode);
#endif
#else
    return FALSE;
#endif
  }
}

static Int file_exists(USES_REGS1) {
  Term tname = Deref(ARG1);
  char *file_name;

  if (IsVarTerm(tname)) {
    Yap_Error(INSTANTIATION_ERROR, tname, "access");
    return FALSE;
  } else if (!IsAtomTerm(tname)) {
    Yap_Error(TYPE_ERROR_ATOM, tname, "access");
    return FALSE;
  } else {
#if HAVE_STAT
    struct SYSTEM_STAT ss;

    file_name = RepAtom(AtomOfTerm(tname))->StrOfAE;
    if (SYSTEM_STAT(file_name, &ss) != 0) {
      if (errno == ENOENT)
        return false;
      PlIOError(SYSTEM_ERROR_OPERATING_SYSTEM, tname, "error %s",
                strerror(errno));
      return false;
    }
    return true;
#else
    return FALSE;
#endif
  }
}

static Int time_file(USES_REGS1) {
  Term tname = Deref(ARG1);

  if (IsVarTerm(tname)) {
    Yap_Error(INSTANTIATION_ERROR, tname, "access");
    return FALSE;
  } else if (!IsAtomTerm(tname)) {
    Yap_Error(TYPE_ERROR_ATOM, tname, "access");
    return FALSE;
  } else {
    const char *n = RepAtom(AtomOfTerm(tname))->StrOfAE;
#if __WIN32
    FILETIME ft;
    HANDLE hdl;
    Term rc;

    if ((hdl = CreateFile(n, 0, 0, NULL, OPEN_EXISTING, 0, 0)) == 0) {
      Yap_WinError("in time_file");
      return false;
    }
    if (GetFileTime(hdl, NULL, NULL, &ft) == 0) {
      Yap_WinError("in time_file");
      return false;
    }
    // Convert the last-write time to local time.
    // FileTimeToSystemTime(&ftWrite, &stUTC);
    // SystemTimeToTzSpecificLocalTime(NULL, &stUTC, &stLocal);
    CloseHandle(hdl);
    ULONGLONG qwResult;

    // Copy the time into a quadword.
    qwResult = (((ULONGLONG)ft.dwHighDateTime) << 32) + ft.dwLowDateTime;
#if SIZEOF_INT_P == 8
    rc = MkIntegerTerm(qwResult);
#elif USE_GMP
    char s[64];
    MP_INT rop;

    snprintf(s, 64, "%I64d", (long long int)n);
    mpz_init_set_str(&rop, s, 10);
    rc = Yap_MkBigIntTerm((void *)&rop PASS_REGS);
#else
    rc = MkIntegerTerm(ft.dwHighDateTime);
#endif
    return Yap_unify(ARG2, rc);
#elif HAVE_STAT
    struct SYSTEM_STAT ss;

    if (SYSTEM_STAT(n, &ss) != 0) {
      /* ignore errors while checking a file */
      return FALSE;
    }
    return Yap_unify(ARG2, MkIntegerTerm(ss.st_mtime));
#else
    return FALSE;
#endif
  }
}

static Int file_size(USES_REGS1) {
  int rc;
  Int sno = Yap_CheckStream(
      ARG1, (Input_Stream_f | Output_Stream_f | Socket_Stream_f),
      "file_size/2");
  if (sno < 0)
    return (FALSE);
  if (GLOBAL_Stream[sno].status & Seekable_Stream_f &&
      !(GLOBAL_Stream[sno].status &
        (InMemory_Stream_f | Socket_Stream_f | Pipe_Stream_f))) {
    // there
    struct stat file_stat;
    if ((rc = fstat(fileno(GLOBAL_Stream[sno].file), &file_stat)) < 0) {
      UNLOCK(GLOBAL_Stream[sno].streamlock);
      if (rc == ENOENT)
        PlIOError(EXISTENCE_ERROR_SOURCE_SINK, ARG1, "%s in file_size",
                  strerror(errno));
      else
        PlIOError(PERMISSION_ERROR_INPUT_STREAM, ARG1, "%s in file_size",
                  strerror(errno));
      return false;
    }
    // and back again
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    return Yap_unify_constant(ARG2, MkIntegerTerm(file_stat.st_size));
  }
  UNLOCK(GLOBAL_Stream[sno].streamlock);
  return false;
}

static Int lines_in_file(USES_REGS1) {
  Int sno = Yap_CheckStream(ARG1, (Input_Stream_f), "lines_in_file/2");
  if (sno < 0)
    return false;
  FILE *f = GLOBAL_Stream[sno].file;
  size_t count = 0;
  int ch;
#if __ANDROID__
#define getw getc
#endif
  if (!f)
    return false;
  while ((ch = getw(f)) >= 0) {
    if (ch == '\n') {
      count++;
    }
  }
  return Yap_unify(ARG2, MkIntegerTerm(count));
}

static Int access_file(USES_REGS1) {
  Term tname = Deref(ARG1);
  Term tmode = Deref(ARG2);
  char *ares;
  Atom atmode;

  if (IsVarTerm(tmode)) {
    Yap_Error(INSTANTIATION_ERROR, tmode, "access_file/2");
    return FALSE;
  } else if (!IsAtomTerm(tmode)) {
    Yap_Error(TYPE_ERROR_ATOM, tname, "access_file/2");
    return FALSE;
  }
  atmode = AtomOfTerm(tmode);
  if (IsVarTerm(tname)) {
    Yap_Error(INSTANTIATION_ERROR, tname, "access_file/2");
    return FALSE;
  } else if (!IsAtomTerm(tname)) {
    Yap_Error(TYPE_ERROR_ATOM, tname, "access_file/2");
    return FALSE;
  } else {
    if (atmode == AtomNone)
      return TRUE;
    if (!(ares = RepAtom(AtomOfTerm(tname))->StrOfAE))
      return FALSE;
  }
#if HAVE_ACCESS
#if _WIN32
  {
    int mode;

    if (atmode == AtomExist)
      mode = 00;
    else if (atmode == AtomExists)
      mode = 00;
    else if (atmode == AtomWrite)
      mode = 02;
    else if (atmode == AtomRead)
      mode = 04;
    else if (atmode == AtomAppend)
      mode = 03;
    else if (atmode == AtomCsult)
      mode = 04;
    else if (atmode == AtomExecute)
      mode = 00; // can always execute?
    else {
      Yap_Error(DOMAIN_ERROR_IO_MODE, tmode, "access_file/2");
      return FALSE;
    }
    if (access(ares, mode) < 0) {
      /* ignore errors while checking a file */
      return false;
    }
    return true;
  }
#else
  {
    int mode;

    if (atmode == AtomExist)
      mode = F_OK;
    else if (atmode == AtomExists)
      mode = F_OK;
    else if (atmode == AtomWrite)
      mode = W_OK;
    else if (atmode == AtomRead)
      mode = R_OK;
    else if (atmode == AtomAppend)
      mode = W_OK;
    else if (atmode == AtomCsult)
      mode = R_OK;
    else if (atmode == AtomExecute)
      mode = X_OK;
    else {
      Yap_Error(DOMAIN_ERROR_IO_MODE, tmode, "access_file/2");
      return FALSE;
    }
    if (access(ares, mode) < 0) {
      /* ignore errors while checking a file */
      return false;
    }
    return true;
  }
#endif
#elif HAVE_STAT
  {
    struct SYSTEM_STAT ss;

    if (SYSTEM_STAT(ares, &ss) != 0) {
      /* ignore errors while checking a file */
      return FALSE;
    }
    return TRUE;
  }
#else
  return FALSE;
#endif
}

static Int exists_directory(USES_REGS1) {
  Term tname = Deref(ARG1);
  char *file_name;

  if (IsVarTerm(tname)) {
    Yap_Error(INSTANTIATION_ERROR, tname, "exists_directory/1");
    return FALSE;
  } else if (!IsAtomTerm(tname)) {
    Yap_Error(TYPE_ERROR_ATOM, tname, "exists_directory/1");
    return FALSE;
  } else {
#if HAVE_STAT
    struct SYSTEM_STAT ss;

    file_name = RepAtom(AtomOfTerm(tname))->StrOfAE;
    if (SYSTEM_STAT(file_name, &ss) != 0) {
      /* ignore errors while checking a file */
      return false;
    }
    return (S_ISDIR(ss.st_mode));
#else
    return FALSE;
#endif
  }
}

static Int is_absolute_file_name(USES_REGS1) { /* file_base_name(Stream,N) */
  Term t = Deref(ARG1);
  Atom at;
  bool rc;

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "file_base_name/2");
    return false;
  }
  const char *buf = Yap_TextTermToText(t, NULL, LOCAL_encoding);
  if (buf) {
    rc = Yap_IsAbsolutePath(buf);
  } else {
    at = AtomOfTerm(t);
#if _WIN32
    rc = PathIsRelative(RepAtom(at)->StrOfAE);
#else
    rc = RepAtom(at)->StrOfAE[0] == '/';
#endif
    freeBuffer(buf);
  }
  return rc;
}

static Int file_base_name(USES_REGS1) { /* file_base_name(Stream,N) */
  Term t = Deref(ARG1);
  Atom at;
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "file_base_name/2");
    return FALSE;
  }
  at = AtomOfTerm(t);
  const char *c = RepAtom(at)->StrOfAE;
  const char *s;
#if HAVE_BASENAME && 0 // DISABLED: Linux basename is not compatible with
                       // file_base_name in SWI and GNU
  char c1[YAP_FILENAME_MAX + 1];
  strncpy(c1, c, YAP_FILENAME_MAX);
  s = basename(c1);
#else
  Int i = strlen(c);
  while (i && !Yap_dir_separator((int)c[--i]))
    ;
  if (Yap_dir_separator((int)c[i])) {
    i++;
  }
  s = c + i;
#endif
  return Yap_unify(ARG2, MkAtomTerm(Yap_LookupAtom(s)));
}

static Int file_directory_name(USES_REGS1) { /* file_directory_name(Stream,N) */
  Term t = Deref(ARG1);
  Atom at;
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "file_directory_name/2");
    return false;
  }
  at = AtomOfTerm(t);
  const char *c = RepAtom(at)->StrOfAE;
#if HAVE_BASENAME && 0 // DISABLED: Linux basename is not compatible with
                       // file_base_name in SWI and GNU
  const char *s;
  char c1[YAP_FILENAME_MAX + 1];
  strncpy(c1, c, YAP_FILENAME_MAX);
  s = dirname(c1);
#else
  char s[YAP_FILENAME_MAX + 1];
  Int i = strlen(c);
  strncpy(s, c, YAP_FILENAME_MAX);
  while (--i) {
    if (Yap_dir_separator((int)c[i]))
      break;
  }
  s[i] = '\0';
#endif
  return Yap_unify(ARG2, MkAtomTerm(Yap_LookupAtom(s)));
}

static Int same_file(USES_REGS1) {
  char *f1 = RepAtom(AtomOfTerm(Deref(ARG1)))->StrOfAE;
  char *f2 = RepAtom(AtomOfTerm(Deref(ARG2)))->StrOfAE;

  if (strcmp(f1, f2) == 0)
    return TRUE;
#if HAVE_LSTAT
  {
    int out;
    struct stat *b1, *b2;
    while ((char *)HR + sizeof(struct stat) * 2 > (char *)(ASP - 1024)) {
      if (!Yap_gcl(2 * sizeof(struct stat), 2, ENV, Yap_gcP())) {
        Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
        return FALSE;
      }
    }
    b1 = (struct stat *)HR;
    b2 = b1 + 1;
    if (strcmp(f1, "user_input") == 0) {
      if (fstat(fileno(GLOBAL_Stream[0].file), b1) == -1) {
        /* file does not exist, but was opened? Return -1 */
        return FALSE;
      }
    } else if (strcmp(f1, "user_output") == 0) {
      if (fstat(fileno(GLOBAL_Stream[1].file), b1) == -1) {
        /* file does not exist, but was opened? Return -1 */
        return FALSE;
      }
    } else if (strcmp(f1, "user_error") == 0) {
      if (fstat(fileno(GLOBAL_Stream[2].file), b1) == -1) {
        /* file does not exist, but was opened? Return -1 */
        return FALSE;
      }
    } else if (stat(f1, b1) == -1) {
      /* file does not exist, but was opened? Return -1 */
      return FALSE;
    }
    if (strcmp(f2, "user_input") == 0) {
      if (fstat(fileno(GLOBAL_Stream[0].file), b2) == -1) {
        /* file does not exist, but was opened? Return -1 */
        return FALSE;
      }
    } else if (strcmp(f2, "user_output") == 0) {
      if (fstat(fileno(GLOBAL_Stream[1].file), b2) == -1) {
        /* file does not exist, but was opened? Return -1 */
        return FALSE;
      }
    } else if (strcmp(f2, "user_error") == 0) {
      if (fstat(fileno(GLOBAL_Stream[2].file), b2) == -1) {
        /* file does not exist, but was opened? Return -1 */
        return FALSE;
      }
    } else if (stat(f2, b2) == -1) {
      /* file does not exist, but was opened? Return -1 */
      return FALSE;
    }
    out = (b1->st_ino == b2->st_ino
#ifdef __LCC__
           &&
           memcmp((const void *)&(b1->st_dev), (const void *)&(b2->st_dev),
                  sizeof(buf1.st_dev)) == 0
#else
           && b1->st_dev == b2->st_dev
#endif
           );
    return out;
  }
#else
  return (FALSE);
#endif
}

void Yap_InitFiles(void) {
  Yap_InitCPred("file_base_name", 2, file_base_name, SafePredFlag);
  Yap_InitCPred("file_directory_name", 2, file_directory_name, SafePredFlag);
  Yap_InitCPred("is_absolute_file_name", 1, is_absolute_file_name,
                SafePredFlag);
  Yap_InitCPred("same_file", 2, same_file, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$access_file", 2, access_file, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$lines_in_file", 2, lines_in_file,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("access", 1, access_path, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("exists_directory", 1, exists_directory,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("exists_file", 1, exists_file, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$file_exists", 1, file_exists, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("time_file64", 2, time_file, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("time_file", 2, time_file, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("file_size", 2, file_size, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("file_name_extension", 3, file_name_extension,
                SafePredFlag | SyncPredFlag);
}
