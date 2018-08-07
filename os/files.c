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
 * File:		iopreds.c *
 * Last rev:	5/2/88							 *
 * mods: *
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
#include "yapio.h"

#if HAVE_DIRENT_H
#include <dirent.h>
#endif
#if HAVE_DIRECT_H
#include <direct.h>
#endif
#if defined(__MINGW32__) || _MSC_VER
#include <io.h>
#include <windows.h>
#endif
#if _MSC_VER || defined(__MINGW32__)
#define SYSTEM_STAT _stat
#else
#define SYSTEM_STAT stat
#endif

const char *Yap_GetFileName(Term t USES_REGS) {
  char *buf = Malloc(YAP_FILENAME_MAX + 1);
  if (IsApplTerm(t) && FunctorOfTerm(t) == FunctorSlash) {
    snprintf(buf, YAP_FILENAME_MAX, "%s/%s", Yap_GetFileName(ArgOfTerm(1, t)),
             Yap_GetFileName(ArgOfTerm(2, t)));
  }
  if (IsAtomTerm(t)) {
    return RepAtom(AtomOfTerm(t))->StrOfAE;
  }
  if (IsStringTerm(t)) {
    return StringOfTerm(t);
  }
  return Yap_TextTermToText(t PASS_REGS);
}

/**
 * @pred file_name_extension( ? BaseFile, ?Extension, ?FullNameO)
 *
 * Relate a file name with an extension. The extension is the filename's suffix
 * and indicates the kind of the file.
 *
 * The predicate can be used to:
 * - Given __FullName__, extract the extension as _Extension_, and the remainder
 * as _BaseFile_. - Given _BaseFile_ and _?Extension_ obtain a _FullNameO_.
 * ~~~~
 * ~~~~
 *   Notice that:
 *   + if no suffix is found, file_name_extension/3 generates the empty
 * suffu]kx, `''`. + the extension does not include the `,` separator; + the
 * suffix may be longer thsn 3 characters + case should not matter in Windows
 * and MacOS + paths may not correspond to valid file names.
 *
 * @return G
 */
static Int file_name_extension(USES_REGS1) {
  Term t1;
  Term t2;
  Term t3 = Deref(ARG3);
  int l = push_text_stack();
  if (!IsVarTerm(t3)) {
    // full path is given.
    const char *f = Yap_GetFileName(t3);
    const char *ext;
    char *base;
    bool rc = true;
    seq_type_t typ = Yap_TextType(t3);
    if (!f) {
      pop_text_stack(l);
      return false;
    }
    size_t len_b = strlen(f), lenb_b;
    char *candidate = strrchr(f, '.');
    char *file = strrchr(f, '/');
    if (candidate  && candidate > file) {
      lenb_b = candidate - f;
      ext = candidate + 1;
    } else {
      lenb_b = len_b;
      ext = "";
    }
    base = Malloc(lenb_b + 1);
    memmove(base, f, lenb_b);
    base[lenb_b] = '\0';
    if (IsVarTerm(t1 = Deref(ARG1))) {
      // should always succeed
      rc = Yap_unify(t1, Yap_MkTextTerm(base, typ));
    } else {
      char *f_a = (char *)Yap_GetFileName(t1 PASS_REGS);
#if __APPLE__ || _WIN32
      rc = strcasecmp(f_a, base) == 0;
#else
      rc = strcmp(f_a, base) == 0;
#endif
    }
    if (rc) {
      if (IsVarTerm(t2 = Deref(ARG2))) {
        // should always succeed
        rc = Yap_unify(t2, Yap_MkTextTerm(ext, typ));
      } else {
        char *f_a = (char *)Yap_TextTermToText(t2 PASS_REGS);
        if (f_a[0] == '.') {
          f_a += 1;
        }
#if __APPLE__ || _WIN32
        rc = strcasecmp(f_a, ext) == 0;
#else
        rc = strcmp(f_a, ext) == 0;
#endif
      }
    }
    pop_text_stack(l);
    return rc;
  } else {
    const char *f;
    char *f2;
    seq_type_t typ, typ1 = Yap_TextType((t1 = Deref(ARG1))),
                    typ2 = Yap_TextType((t2 = Deref(ARG2)));
    if (typ1 == typ2) {
      typ = typ1;
    } else if (typ1 == YAP_STRING_ATOM || typ2 == YAP_STRING_ATOM) {
      typ = YAP_STRING_ATOM;
    } else {
      typ = YAP_STRING_STRING;
    }
    if (!(f = Yap_TextTermToText(t1 PASS_REGS))) {
      pop_text_stack(l);
      return false;
    }
    if (!(f2 = (char *)Yap_TextTermToText(t2 PASS_REGS))) {
      pop_text_stack(l);
      return false;
    }
    if (f2[0] == '.') {
      f2++;
    }

    size_t lenb_b = strlen(f);
    char *o = Realloc((void *)f, lenb_b + strlen(f2) + 2);
    o[lenb_b] = '.';
    o += lenb_b + 1;
    pop_text_stack(l);
    return strcpy(o, f2) && (t3 = Yap_MkTextTerm(o, typ)) &&
           Yap_unify(t3, ARG3);
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
          VFS_t *vfs;
          char *s =  RepAtom(AtomOfTerm(tname))->StrOfAE;
          if (!s) return false;
          if ((vfs = vfs_owner(s))) {
              vfs_stat st;
              bool rc = vfs->stat(vfs, s, &st);
              UNLOCK(GLOBAL_Stream[sno].streamlock);
              return rc;
          }
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
      VFS_t *vfs;
      char *s =  RepAtom(AtomOfTerm(tname))->StrOfAE;
      if (!s) return false;
      if ((vfs = vfs_owner(s))) {
          vfs_stat st;
          bool rc = vfs->stat(vfs, s, &st);
          UNLOCK(GLOBAL_Stream[sno].streamlock);
return rc;
      }
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
    VFS_t *vfs;
    if ((vfs = vfs_owner(n))) {
      vfs_stat s;
      vfs->stat(vfs, n, &s);
      return Yap_unify(ARG2, MkIntegerTerm(s.st_mtimespec.tv_sec));
    }
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
  VFS_t *vfs;
  char *s = RepAtom(GLOBAL_Stream[sno].name)->StrOfAE;
  if (!s) return false;
  if ((vfs = vfs_owner(s))) {
    vfs_stat st;
    vfs->stat(vfs, s, &st);
    UNLOCK(GLOBAL_Stream[sno].streamlock);
    return Yap_unify_constant(ARG2, MkIntegerTerm(st.st_size));
  }
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
    VFS_t *vfs;
    if ((vfs = vfs_owner(ares))) {
        vfs_stat o;
        if (vfs->stat(vfs, ares, &o)) {
            if (atmode == AtomExist)
                return true;
            else if (atmode == AtomExists)
                return true;
            else if (atmode == AtomWrite)
                return o.st_mode & VFS_CAN_WRITE;
            else if (atmode == AtomRead)
                return o.st_mode & VFS_CAN_READ;
            else if (atmode == AtomAppend)
                return o.st_mode & VFS_CAN_WRITE;
            else if (atmode == AtomCsult)
                return o.st_mode & VFS_CAN_READ;
            else if (atmode == AtomExecute)
                return o.st_mode & VFS_CAN_EXEC;
            else {
                Yap_Error(DOMAIN_ERROR_IO_MODE, tmode, "access_file/2");
                return FALSE;
            }
        } else {
            return false;
        }
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
    VFS_t *vfs;
    char *s = Yap_VF(RepAtom(AtomOfTerm(tname))->StrOfAE);
    if (!s) return false;
    if ((vfs = vfs_owner(s))) {
bool rc = true;
      return vfs->isdir(vfs, s);

      UNLOCK(GLOBAL_Stream[sno].streamlock);
      return rc;
    }
#if HAVE_STAT
    struct SYSTEM_STAT ss;

    file_name = Yap_VF(RepAtom(AtomOfTerm(tname))->StrOfAE);
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
  int l = push_text_stack();
  const char *buf = Yap_TextTermToText(t PASS_REGS);
  if (buf) {
    rc = Yap_IsAbsolutePath(buf, true);
  } else {
    at = AtomOfTerm(t);
#if _WIN32
    rc = PathIsRelative(RepAtom(at)->StrOfAE);
#else
    rc = RepAtom(at)->StrOfAE[0] == '/';
#endif
  }
  pop_text_stack(l);
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
  if (i == 0) {
    s[0] = '.';
    i = 1;
  }
  s[i] = '\0';
#endif
  return Yap_unify(ARG2, MkAtomTerm(Yap_LookupAtom(s)));
}


/* Return a list of files for a directory */
static Int list_directory(USES_REGS1) {
  Term tf = MkAtomTerm(Yap_LookupAtom("[]"));
  yhandle_t sl = Yap_InitSlot(tf);
VFS_t *vfsp;
  char *buf = (char *)AtomName(AtomOfTerm(ARG1));
    if ((vfsp = vfs_owner(buf))) {
        void *de;
      const char *dp;

    if ((de = vfsp->opendir(vfsp, buf)) == NULL) {
      PlIOError(PERMISSION_ERROR_INPUT_STREAM, ARG1, "%s in list_directory",
		strerror(errno));
    }
    while ((dp = vfsp->nextdir( de))) {
      YAP_Term ti = MkAtomTerm(Yap_LookupAtom(dp));
      Yap_PutInHandle(sl, MkPairTerm(ti, Yap_GetFromHandle(sl)));
    }
    vfsp->closedir( de);
 } else {
#if defined(__MINGW32__) || _MSC_VER
        struct _finddata_t c_file;
        char bs[BUF_SIZE];
        long hFile;

        bs[0] = '\0';
#if HAVE_STRNCPY
        strncpy(bs, buf, BUF_SIZE);
#else
        strcpy(bs, buf);
#endif
#if HAVE_STRNCAT
        strncat(bs, "/*", BUF_SIZE);
#else
        strcat(bs, "/*");
#endif
        if ((hFile = _findfirst(bs, &c_file)) == -1L) {
          return (Yap_Unify(ARD2, tf));
        }
        YAP_PutInSlot(sl, YAP_MkPairTerm(YAP_MkAtomTerm(YAP_LookupAtom(c_file.name)),
                                         YAP_GetFromSlot(sl)));
        while (_findnext(hFile, &c_file) == 0) {
          YAP_Term ti = YAP_MkAtomTerm(YAP_LookupAtom(c_file.name));
          YAP_PutInSlot(sl, YAP_MkPairTerm(ti, YAP_GetFromSlot(sl)));
        }
        _findclose(hFile);
#elif HAVE_OPENDIR
        {
            DIR *de;
            struct dirent *dp;

            if ((de = opendir(buf)) == NULL) {
                PlIOError(PERMISSION_ERROR_INPUT_STREAM, ARG1, "%s in list_directory",
                          strerror(errno));

                return false;
            }
            while ((dp = readdir(de))) {
                Term ti = MkAtomTerm(Yap_LookupAtom(dp->d_name));
                Yap_PutInSlot(sl, MkPairTerm(ti, Yap_GetFromSlot(sl)));
            }
            closedir(de);
        }
#endif /* HAVE_OPENDIR */
    }
  tf = Yap_GetFromSlot(sl); 
  return Yap_unify(ARG2, tf);
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
           && memcmp((const void *)&(b1->st_dev), (const void *)&(b2->st_dev),
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
  Yap_InitCPred("list_directory", 2, list_directory, SyncPredFlag);
}
