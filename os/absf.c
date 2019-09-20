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

#include "Yap.h"
#include "YapFlags.h"
#include "yapio.h"
#include "sysbits.h"

#include <cwalk.h>

/**
 * @file   absf.c
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Wed Jan 20 00:45:56 2016
 *
 * @brief  absolute file name: C ut in a different light.
 *
 */

static Term do_glob(const char *spec, bool glob_vs_wordexp);
static const char *PlExpandVars(const char *source, const char *root);

static Term gethdir(Term t) {
    CACHE_REGS
    Atom aref = AtomOfTerm(t);
    char *s = RepAtom(aref)->StrOfAE;
    size_t nsz;

    s = strncpy(LOCAL_FileNameBuf, RepAtom(aref)->StrOfAE, MAXPATHLEN - 1);
    if (!s) {
        return false;
    }
    if (TermDot == t) {
        return TermEmptyAtom;
    }
    nsz = strlen(s);
    if (!Yap_dir_separator(s[nsz - 1])) {
#if _WIN32
        s[nsz] = '\\';
#else
        s[nsz] = '/';
#endif
        s[nsz + 1] = '\0';
    }
    return MkAtomTerm(Yap_LookupAtom(s));
}

static Term issolutions(Term t) {
    if (t == TermFirst || t == TermAll)
        return t;

    if (IsVarTerm(t)) {
        Yap_Error(INSTANTIATION_ERROR, t, "solutions in {first, all}.");
        return TermZERO;
    }
    if (IsAtomTerm(t)) {
        Yap_Error(DOMAIN_ERROR_SOLUTIONS, t, "solutions in {first, all}");
        return TermZERO;
    }
    Yap_Error(TYPE_ERROR_ATOM, t, "solutions in {first, all}}");
    return TermZERO;
}

static Term is_file_errors(Term t) {
    if (t == TermFail || t == TermError)
        return t;

    if (IsVarTerm(t)) {
        Yap_Error(INSTANTIATION_ERROR, t, "file_error in {fail,error}.");
        return TermZERO;
    }
    if (IsAtomTerm(t)) {
        Yap_Error(DOMAIN_ERROR_FILE_ERRORS, t, "file_error in {fail,error}.");
        return TermZERO;
    }
    Yap_Error(TYPE_ERROR_ATOM, t, "file_error in {fail,error}.");
    return TermZERO;
}


static Term is_file_type(Term t) {
  if (t == TermTxt || t == TermProlog || t == TermSource ||
      t == TermExecutable || t == TermQly || t == TermDirectory)
    return t;

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t,
              "file_type in {txt,prolog,exe,directory...}");
    return TermZERO;
  }
  if (IsAtomTerm(t)) {
    Yap_Error(DOMAIN_ERROR_FILE_TYPE, t,
              "file_type in {txt,prolog,exe,directory...}");
    return TermZERO;
  }
  Yap_Error(TYPE_ERROR_ATOM, t, "file_type in {txt,prolog,exe,directory...}");
  return TermZERO;
}

#define ABSOLUTE_FILE_NAME_DEFS()                                              \
  PAR("access", isatom, ABSOLUTE_FILE_NAME_ACCESS)                             \
  , PAR("expand", booleanFlag, ABSOLUTE_FILE_NAME_EXPAND),                     \
      PAR("extensions", ok, ABSOLUTE_FILE_NAME_EXTENSIONS),                    \
      PAR("file_errors", is_file_errors, ABSOLUTE_FILE_NAME_FILE_ERRORS),      \
      PAR("file_type", is_file_type, ABSOLUTE_FILE_NAME_FILE_TYPE),            \
      PAR("glob", ok, ABSOLUTE_FILE_NAME_GLOB),                                \
      PAR("relative_to", isatom, ABSOLUTE_FILE_NAME_RELATIVE_TO),              \
      PAR("solutions", issolutions, ABSOLUTE_FILE_NAME_SOLUTIONS),             \
      PAR("verbose_file_search", booleanFlag,                                  \
          ABSOLUTE_FILE_NAME_VERBOSE_FILE_SEARCH),                             \
      PAR(NULL, ok, ABSOLUTE_FILE_NAME_END)

#define PAR(x, y, z) z

typedef enum ABSOLUTE_FILE_NAME_enum_ {
  ABSOLUTE_FILE_NAME_DEFS()
} absolute_file_name_choices_t;

#undef PAR

#define PAR(x, y, z)                                                           \
  { x, y, z }

static const param_t absolute_file_name_search_defs[] = {
    ABSOLUTE_FILE_NAME_DEFS()};
#undef PAR

static Int abs_file_parameters(USES_REGS1) {
  Term t[ABSOLUTE_FILE_NAME_END];
  Term tlist = Deref(ARG1), tf;
  /* get options */
  xarg *args = Yap_ArgListToVector(tlist, absolute_file_name_search_defs,
                                   ABSOLUTE_FILE_NAME_END,
                                   DOMAIN_ERROR_ABSOLUTE_FILE_NAME_OPTION);
  if (args == NULL) {
    if (LOCAL_Error_TYPE != YAP_NO_ERROR) {
      Yap_Error(LOCAL_Error_TYPE, tlist, NULL);
    }
    return false;
  }
  /* done */
  if (args[ABSOLUTE_FILE_NAME_EXTENSIONS].used) {
    t[ABSOLUTE_FILE_NAME_EXTENSIONS] =
        args[ABSOLUTE_FILE_NAME_EXTENSIONS].tvalue;
  } else {
    t[ABSOLUTE_FILE_NAME_EXTENSIONS] = TermNil;
  }
  if (args[ABSOLUTE_FILE_NAME_RELATIVE_TO].used) {
    t[ABSOLUTE_FILE_NAME_RELATIVE_TO] =
        gethdir(args[ABSOLUTE_FILE_NAME_RELATIVE_TO].tvalue);
  } else {
    t[ABSOLUTE_FILE_NAME_RELATIVE_TO] = gethdir(TermDot);
  }
  if (args[ABSOLUTE_FILE_NAME_FILE_TYPE].used)
    t[ABSOLUTE_FILE_NAME_FILE_TYPE] = args[ABSOLUTE_FILE_NAME_FILE_TYPE].tvalue;
  else
    t[ABSOLUTE_FILE_NAME_FILE_TYPE] = TermTxt;
  if (args[ABSOLUTE_FILE_NAME_ACCESS].used)
    t[ABSOLUTE_FILE_NAME_ACCESS] = args[ABSOLUTE_FILE_NAME_ACCESS].tvalue;
  else
    t[ABSOLUTE_FILE_NAME_ACCESS] = TermNone;
  if (args[ABSOLUTE_FILE_NAME_FILE_ERRORS].used)
    t[ABSOLUTE_FILE_NAME_FILE_ERRORS] =
        args[ABSOLUTE_FILE_NAME_FILE_ERRORS].tvalue;
  else
    t[ABSOLUTE_FILE_NAME_FILE_ERRORS] = TermError;
  if (args[ABSOLUTE_FILE_NAME_SOLUTIONS].used)
    t[ABSOLUTE_FILE_NAME_SOLUTIONS] = args[ABSOLUTE_FILE_NAME_SOLUTIONS].tvalue;
  else
    t[ABSOLUTE_FILE_NAME_SOLUTIONS] = TermFirst;
  if (args[ABSOLUTE_FILE_NAME_EXPAND].used)
    t[ABSOLUTE_FILE_NAME_EXPAND] = args[ABSOLUTE_FILE_NAME_EXPAND].tvalue;
  else
    t[ABSOLUTE_FILE_NAME_EXPAND] = TermFalse;
  if (args[ABSOLUTE_FILE_NAME_GLOB].used) {
    t[ABSOLUTE_FILE_NAME_GLOB] = args[ABSOLUTE_FILE_NAME_GLOB].tvalue;
    t[ABSOLUTE_FILE_NAME_EXPAND] = TermTrue;
  } else
    t[ABSOLUTE_FILE_NAME_GLOB] = TermEmptyAtom;
  if (args[ABSOLUTE_FILE_NAME_VERBOSE_FILE_SEARCH].used)
    t[ABSOLUTE_FILE_NAME_VERBOSE_FILE_SEARCH] =
        args[ABSOLUTE_FILE_NAME_VERBOSE_FILE_SEARCH].tvalue;
  else
    t[ABSOLUTE_FILE_NAME_VERBOSE_FILE_SEARCH] =
        (trueGlobalPrologFlag(VERBOSE_FILE_SEARCH_FLAG) ? TermTrue : TermFalse);
  tf = Yap_MkApplTerm(Yap_MkFunctor(AtomOpt, ABSOLUTE_FILE_NAME_END),
                      ABSOLUTE_FILE_NAME_END, t);
  return (Yap_unify(ARG2, tf));
}

static Int get_abs_file_parameter(USES_REGS1) {
  Term t = Deref(ARG1), topts = Deref(ARG2);
  /* get options */
  /* done */
  int i = Yap_ArgKey(AtomOfTerm(t), absolute_file_name_search_defs,
                     ABSOLUTE_FILE_NAME_END);
  if (i >= 0)
    return Yap_unify(ARG3, ArgOfTerm(i + 1, topts));
  Yap_Error(DOMAIN_ERROR_ABSOLUTE_FILE_NAME_OPTION, ARG1, NULL);
  return false;
}

    static const char *myrealpath(const char *path USES_REGS) {
  int lvl = push_text_stack();
  char *o = Malloc(FILENAME_MAX+1), *wd = Malloc(FILENAME_MAX+1);
  const char *cwd =  Yap_getcwd(wd, FILENAME_MAX);
 size_t sz = cwk_path_get_absolute(
				  cwd, path,
				     o,
			 FILENAME_MAX
			 );
  if (sz <0)
    return NULL;
return pop_output_text_stack(lvl,o);
}


/**
 * generate absolute path, if ok first expand SICStus Prolog style
 *
 * @param[in]  spec the file path, including `~` and `$`.
 * @param[in]  ok where to process `~` and `$`.
 *
 * @return tmp, or NULL, in malloced memory
 */
const char *Yap_AbsoluteFile(const char *spec, bool ok) {
  const char *rc;
  const char *spec1;
  const char *spec2;
  int lvl = push_text_stack();

/// spec gothe original spec;
  /// rc0 may be an outout buffer
  /// rc1 the internal buffer
  ///
  /// PlExpandVars

#if _WIN32
  char rc2[YAP_FILENAME_MAX];
  if ((rc = unix2win(spec, rc2, YAP_FILENAME_MAX)) == NULL) {
    return NULL;
  }
  spec1 = rc;
#else
  spec1 = spec;
#endif
  /// spec gothe original spec;
  /// rc1 the internal buffer
  if (ok) {
    const char *q = PlExpandVars(spec1, NULL);
    if (!q)
      spec2 = spec1;
    else
      spec2 = q;
  } else {
    spec2 = spec1;
  }
  rc = myrealpath(spec2 PASS_REGS);
  return pop_output_text_stack(lvl, rc);
}

static Term
/* Expand the string for the program to run.  */
do_glob(const char *spec, bool glob_vs_wordexp) {
  CACHE_REGS
  if (spec == NULL) {
    return TermNil;
  }
#if _WIN32
  char u[YAP_FILENAME_MAX + 1];
  {
    WIN32_FIND_DATA find;
    HANDLE hFind;
    CELL *dest;
    Term tf;
    char drive[_MAX_DRIVE];
    char dir[_MAX_DIR];
    char fname[_MAX_FNAME];
    char ext[_MAX_EXT];

    _splitpath(spec, drive, dir, fname, ext);
    _makepath(u, drive, dir, fname, ext);

    // first pass, remove Unix style stuff
    hFind = FindFirstFile(u, &find);
    if (hFind == INVALID_HANDLE_VALUE) {
      return TermNil;
    } else {
      tf = AbsPair(HR);
      _makepath(u, drive, dir, find.cFileName, NULL);
      HR[0] = MkAtomTerm(Yap_LookupAtom(u));
      HR[1] = TermNil;
      dest = HR + 1;
      HR += 2;
      while (FindNextFile(hFind, &find)) {
        *dest = AbsPair(HR);
        _makepath(u, drive, dir, find.cFileName, NULL);
        HR[0] = MkAtomTerm(Yap_LookupAtom(u));
        HR[1] = TermNil;
        dest = HR + 1;
        HR += 2;
      }
      FindClose(hFind);
    }
    return tf;
  }
#elif __ANDROID__
     return MkPairTerm(MkAtomTerm(Yap_LookupAtom(spec)), TermNil);
#elif HAVE_WORDEXP || HAVE_GLOB
  char u[YAP_FILENAME_MAX + 1];
  const char *espec = u;
  strncpy(u, spec, sizeof(u));
  /* Expand the string for the program to run.  */
  size_t pathcount;
#if HAVE_GLOB
  glob_t gresult;
#endif
#if HAVE_WORDEXP
  wordexp_t wresult;
#endif
#if HAVE_GLOB || HAVE_WORDEXP
  char **ss = NULL;
  int flags = 0, j;
#endif
  if (glob_vs_wordexp) {
#if HAVE_GLOB
#ifdef GLOB_NOCHECK
    flags = GLOB_NOCHECK;
#else
    flags = 0;
#endif
#ifdef GLOB_BRACE
    flags |= GLOB_BRACE | GLOB_TILDE;
#endif
    switch (glob(espec, flags, NULL, &gresult)) {
    case 0: /* Successful.  */
      ss = gresult.gl_pathv;
      pathcount = gresult.gl_pathc;
      if (pathcount) {
        break;
      }
    case GLOB_NOMATCH:
      globfree(&gresult);
      { return TermNil; }
    case GLOB_ABORTED:
      PlIOError(SYSTEM_ERROR_OPERATING_SYSTEM, ARG1, "glob aborted: %sn",
                strerror(errno));
      globfree(&gresult);
      return TermNil;
    case GLOB_NOSPACE:
      Yap_Error(RESOURCE_ERROR_HEAP, ARG1, "glob ran out of space: %sn",
                strerror(errno));
      globfree(&gresult);
      return TermNil;
    /* If the error was WRDE_NOSPACE,
     then perhaps part of the result was allocated.  */
    default: /* Some other error.  */
      return TermNil;
    }
#endif
  } else {
#if HAVE_WORDEXP
    int rc;
    memset(&wresult, 0, sizeof(wresult));
    switch ((rc = wordexp(espec, &wresult, flags))) {
    case 0: /* Successful.  */
      ss = wresult.we_wordv;
      pathcount = wresult.we_wordc;
      if (pathcount) {
        break;
      } else {
        Term t;
        t = MkAtomTerm(Yap_LookupAtom(espec));
        wordfree(&wresult);
        return MkPairTerm(t, TermNil);
      }
    case WRDE_NOSPACE:
      /* If the error was WRDE_NOSPACE,
       then perhaps part of the result was allocated.  */
      Yap_Error(RESOURCE_ERROR_HEAP, ARG1, "wordexp ran out of space: %s",
                strerror(errno));
      wordfree(&wresult);
      return TermNil;
    default: /* Some other error.  */
      PlIOError(SYSTEM_ERROR_OPERATING_SYSTEM, ARG1, "wordexp failed: %s",
                strerror(errno));
      wordfree(&wresult);
      return TermNil;
    }
#endif
  }
  const char *tmp;
  Term tf = TermNil;
  for (j = 0; j < pathcount; j++) {
    const char *s = ss[pathcount - (j + 1)];
    tmp = s;
    // if (!exists(s))
    //  continue;
    Atom a = Yap_LookupAtom(tmp);
    tf = MkPairTerm(MkAtomTerm(a), tf);
  }
#if HAVE_GLOB
  if (glob_vs_wordexp)
    globfree(&gresult);
#endif
#if HAVE_WORDEXP
  if (!glob_vs_wordexp)
    wordfree(&wresult);
#endif
  return tf;
#else
  // just use basic
  return MkPairTerm(MkAtomTerm(Yap_LookupAtom(spec)), TermNil);
#endif
}

// this is necessary because
// support for ~expansion at the beginning
// systems like Android do not do this.
static const char *PlExpandVars(const char *source, const char *root) {
    CACHE_REGS
    int lvl = push_text_stack();
    const char *src = source;
    char *result = Malloc(YAP_FILENAME_MAX + 1);

    if (strlen(source) >= YAP_FILENAME_MAX) {
        Yap_Error(SYSTEM_ERROR_OPERATING_SYSTEM, TermNil,
                  "%s in true_file-name is larger than the buffer size (%d bytes)",
                  source, strlen(source));
    }
    /* step 1: eating home information */
    if (source[0] == '~') {
        if (Yap_dir_separator(source[1]) || source[1] == '\0') {
            char *s;
            src++;
#if defined(_WIN32)
            s = getenv("HOMEDRIVE");
      if (s != NULL)
        strncpy(result, getenv("HOMEDRIVE"), YAP_FILENAME_MAX);
// s = getenv("HOMEPATH");
#else
            s = getenv("HOME");
#endif
            if (s != NULL)
                strncpy(result, s, YAP_FILENAME_MAX);
            strcat(result, src);
            result = pop_output_text_stack(lvl, result);
            return result;
        } else {
#if HAVE_GETPWNAM
            struct passwd *user_passwd;
            char *res = result;

            src++;
            while (!Yap_dir_separator((*res = *src)) && *res != '\0')
                res++, src++;
            res[0] = '\0';
            if ((user_passwd = getpwnam(result)) == NULL) {
                FileError(SYSTEM_ERROR_OPERATING_SYSTEM,
                          MkAtomTerm(Yap_LookupAtom(source)),
                          "User %s does not exist in %s", result, source);
                pop_text_stack(lvl);
                return NULL;
            }
            strncpy(result, user_passwd->pw_dir, YAP_FILENAME_MAX);
            strcat(result, src);
#else
            FileError(
          SYSTEM_ERROR_OPERATING_SYSTEM, MkAtomTerm(Yap_LookupAtom(source)),
          "User %s cannot be found in %s, missing getpwnam", result, source);
      return NULL;
#endif
        }
        result = pop_output_text_stack(lvl, result);
        return result;
    }
        // do VARIABLE expansion
    else if (source[0] == '$') {
        /* follow SICStus expansion rules */
        char v[YAP_FILENAME_MAX + 1];
        int ch;
        char *s, *res;
        src = source + 1;
        if (src[0] == '{') {
            res = v;
            src++;
            while ((*res = (ch = *src++)) && isValidEnvChar(ch) && ch != '}') {
                res++;
            }
            if (ch == '}') {
                // {...}
                // done
                res[0] = '\0';
            }
        } else {
            res = v;
            while ((*res = (ch = *src++)) && isValidEnvChar(ch) && ch != '}') {
                res++;
            }
            src--;
            res[0] = '\0';
        }
        if ((s = (char *)getenv(v))) {
            strcpy(result, s);
            strcat(result, src);
        } else
            strcpy(result, src);
    } else {
        size_t tocp = strlen(src);
        if (root) {
            tocp = strlen(root) + 1;
        }
        if (tocp > YAP_FILENAME_MAX) {
            Yap_Error(SYSTEM_ERROR_OPERATING_SYSTEM, MkStringTerm(src),
                      "path too long");
            pop_text_stack(lvl);
            return NULL;
        }
        if (root && !Yap_IsAbsolutePath(source, false)) {
            strncpy(result, root, YAP_FILENAME_MAX);
            if (root[strlen(root) - 1] != '/')
                strncat(result, "/", YAP_FILENAME_MAX);
            strncat(result, source, YAP_FILENAME_MAX);
        } else {
            strncpy(result, source, strlen(src) + 1);
        }
    }
    result = pop_output_text_stack(lvl, result);
    return result;
}

/**
  @pred absolute_file_name(+Name:atom,+Path:atom) is nondet

  Converts the given file specification into an absolute path, using default options. See absolute_file_name/3 for details on the options.
*/
static Int real_path(USES_REGS1) {
  Term t1 = Deref(ARG1);
  const char *cmd, *rc0;

  if (IsAtomTerm(t1)) {
    cmd = RepAtom(AtomOfTerm(t1))->StrOfAE;
  } else if (IsStringTerm(t1)) {
    cmd = StringOfTerm(t1);
  } else {
    return false;
  }
#if _WIN32
  char cmd2[YAP_FILENAME_MAX + 1];
  char *rc;

  if ((rc = unix2win(cmd, cmd2, YAP_FILENAME_MAX)) == NULL) {
    return false;
  }
  cmd = rc;
#endif
  int lvl = push_text_stack();
  rc0 = myrealpath(cmd PASS_REGS);
  if (!rc0) {
    pop_text_stack(lvl);
    PlIOError(SYSTEM_ERROR_OPERATING_SYSTEM, ARG1, NULL);
  }
  bool out = Yap_unify(MkAtomTerm(Yap_LookupAtom(rc0)), ARG2);
  pop_text_stack(lvl);
  return out;
}

#if undefined
static char *close_path(char *b0, char *o0, char *o) {
     if (b0[0] == '\0') {
           return o;
          } else if (!strcmp(b0, "..")) {
            while (o-- > o0) {
                  if (Yap_dir_separator(*o)) {
                        break;
                      }
               }

                  } else if (strcmp(b0, ".") != 0) {
           *o++ = '/';
           strcpy(o, b0);
           o += strlen(b0);
          }
     return o;
    }
#endif



bool Yap_IsAbsolutePath(const char *p0, bool expand) {
    // verify first if expansion is needed: ~/ or $HOME/
    const char *p = p0;
    bool nrc;
    if (expand) {
        p = PlExpandVars(p0, NULL);
    }
#if _WIN32 || __MINGW32__
    nrc = !PathIsRelative(p);
#else
    nrc = (p[0] == '/');
#endif
    return nrc;
}

 


static Int true_file_name(USES_REGS1) {
    Term t = Deref(ARG1);
    const char *s;

    if (IsVarTerm(t)) {
        Yap_Error(INSTANTIATION_ERROR, t, "argument to true_file_name unbound");
        return FALSE;
    }
    if (IsAtomTerm(t)) {
        s = RepAtom(AtomOfTerm(t))->StrOfAE;
    } else if (IsStringTerm(t)) {
        s = StringOfTerm(t);
    } else {
        Yap_Error(TYPE_ERROR_ATOM, t, "argument to true_file_name");
        return FALSE;
    }
    int l = push_text_stack();
    if (!(s = Yap_AbsoluteFile(s, true)))
        return false;
    bool rc = Yap_unify(ARG2, MkAtomTerm(Yap_LookupAtom(s)));
    pop_text_stack(l);
    return rc;
}


static Int absolute_file_system_path(USES_REGS1) {
  Term t = Deref(ARG1);
  int l = push_text_stack();
  const char *text = Yap_TextTermToText(t);
  const char *fp;
  bool rc;

  if (text == NULL) {
    pop_text_stack(l);
    return false;
  }
  if (!(fp = Yap_AbsoluteFile(text, true))) {
    pop_text_stack(l);
    return false;
  }
  rc = Yap_unify(Yap_MkTextTerm(fp, Yap_TextType(t)), ARG2);
  pop_text_stack(l);
  return rc;
}

static Int prolog_to_os_filename(USES_REGS1) {
  Term t = Deref(ARG1), t2 = Deref(ARG2);
  char *fp;
  char out[MAXPATHLEN + 1];

  if (IsVarTerm(t)) {

    if (IsVarTerm(t2)) {
      Yap_Error(INSTANTIATION_ERROR, t, "prolog_to_os_filename");
      return false;
    } else if (IsAtomTerm(t2)) {
      if (!(fp = PrologPath(RepAtom(AtomOfTerm(t2))->StrOfAE, out)))
        return false;
      return Yap_unify(ARG1, MkAtomTerm(Yap_LookupAtom(fp)));
    } else {
      Yap_Error(TYPE_ERROR_ATOM, t2, "prolog_to_os_filename");
      return false;
    }
  } else if (!IsAtomTerm(t)) {
    Yap_Error(TYPE_ERROR_ATOM, t, "prolog_to_os_filename");
    return false;
  }

  if (!(fp = OsPath(RepAtom(AtomOfTerm(t))->StrOfAE, out)))
    return false;
  return Yap_unify(MkAtomTerm(Yap_LookupAtom(fp)), ARG2);
}


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
 * Given __FullName__, extract the extension as _Extension_, and the remainder
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
#if 0
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

static char *clean_path(const char *path) {
  const char *p, *p0;
  int lvl = push_text_stack();

  //__android_log_print(ANDROID_LOG_INFO, "YAPDroid ", " looking at %s", path);
  char *o0 = Malloc(FILENAME_MAX + 1), *o = o0;
  int ch;
  char *b0 = Malloc(FILENAME_MAX + 1), *b = b0;
  p = p0 = path;
  while ((ch = *p++)) {
    if (Yap_dir_separator(ch)) {
      if (b == b0) {
        o = o0;
      } else {
        b[0] = '\0';
        o = close_path(b0, o0, o);
        b = b0;
      }
    } else {
      *b++ = ch;
    }
  }
  if (!Yap_dir_separator(p[-1])) {
    b[0] = '\0';
    o = close_path(b0, o0, o);
  }
  if (o == o0)
    *o++ = '/';
  *o = '\0';
//  __android_log_print(ANDROID_LOG_INFO, "YAPDroid ", " %s at %s, %p-%p", p0, o0,
//                      o, o0);
  return pop_output_text_stack(lvl, o0);
}
#endif

static Int p_expand_file_name(USES_REGS1) {
  Term t = Deref(ARG1);
  const char *text, *text2;

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "argument to true_file_name unbound");
    return FALSE;
  }
  int l = push_text_stack();
  text = Yap_TextTermToText(t);
  if (!text) {
    pop_text_stack(l);
    return false;
  }
  if (!(text2 = PlExpandVars(text, NULL))) {
    pop_text_stack(l);
    return false;
  }
  bool rc = Yap_unify(ARG2, do_glob(text2, true) );
  pop_text_stack(l);
  return rc;
}

static Int true_file_name3(USES_REGS1) {
  Term t = Deref(ARG1), t2 = Deref(ARG2);

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "argument to true_file_name unbound");
    return FALSE;
  }
  if (!IsAtomTerm(t)) {
    Yap_Error(TYPE_ERROR_ATOM, t, "argument to true_file_name");
    return FALSE;
  }
  if (!IsVarTerm(t2)) {
    if (!IsAtomTerm(t)) {
      Yap_Error(TYPE_ERROR_ATOM, t2, "argument to true_file_name");
      return FALSE;
    }
    //    root = RepAtom(AtomOfTerm(t2))->StrOfAE;
  }
  int lvl = push_text_stack();
  const char *tmp = Yap_AbsoluteFile(RepAtom(AtomOfTerm(t))->StrOfAE, true);
  Atom at = NULL;
  bool rc = (tmp != NULL && (at = Yap_LookupAtom(tmp)) != NULL);
  pop_text_stack(lvl);
  return rc && Yap_unify(ARG3, MkAtomTerm(at));
}

static Int path_concat(USES_REGS1) {
  int l = push_text_stack();
  size_t len;
  Term t = Deref(ARG1);
  Term *tailp;
  int n = Yap_SkipList(&t, &tailp);
  if (*tailp != TermNil) {
      Yap_ThrowError(TYPE_ERROR_LIST, t, "while concatenating sub-paths");
  }
  const  char **inp = Malloc( (n+1)*sizeof(const char *) );
  int i=0;
  while (IsPairTerm(t)) {
    Term th = HeadOfTerm(t);
      if (IsAtomTerm(th)) {
	inp[i++] = RepAtom(AtomOfTerm(th))->StrOfAE;
      } else if (IsStringTerm(th)) {
	inp[i++] = StringOfTerm(th);
      }
   t = TailOfTerm(t);
    }
  inp[i] = NULL;
    len = MAXPATHLEN;
   size_t sz;
           char *buf = Malloc(len);
  do {

    sz = cwk_path_join_multiple(inp,buf,len-1);
      if (sz >= len) {
          len = sz+1;
        buf = Realloc (buf, len);
      } else {
    bool rc= Yap_unify(MkAtomTerm(Yap_LookupAtom(buf)),ARG2);
    pop_text_stack(l);
    return rc;
      }
  } while (true);
  }

  
void Yap_InitAbsfPreds(void) {
  Yap_InitCPred("path_concat", 2, path_concat,0);
  Yap_InitCPred("expand_file_name", 2, p_expand_file_name, SyncPredFlag);
  Yap_InitCPred("prolog_to_os_filename", 2, prolog_to_os_filename,
                SyncPredFlag);
  Yap_InitCPred("absolute_file_system_path", 2, absolute_file_system_path, 0);
  Yap_InitCPred("absolute_file_system_path", 2, absolute_file_system_path, 0);
  Yap_InitCPred("absolute_file_name", 2, real_path, 0);
  Yap_InitCPred("true_file_name", 2, true_file_name, SyncPredFlag);
  Yap_InitCPred("true_file_name", 3, true_file_name3, SyncPredFlag);
  Yap_InitCPred("abs_file_parameters", 2, abs_file_parameters,  HiddenPredFlag);
  Yap_InitCPred("get_abs_file_parameter", 3, get_abs_file_parameter, HiddenPredFlag);
  Yap_InitCPred("file_name_extension", 3, file_name_extension, 0);
}
