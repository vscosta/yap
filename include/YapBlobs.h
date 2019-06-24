//
//  blobs.h
//  yap
//
//  Created by VITOR SANTOS COSTA on 09/05/15.
//  Copyright (c) 2015 VITOR SANTOS COSTA. All rights reserved.
//

// based on the SWI Blob implementation, an extension of atoms for SWI-Prolog

#ifndef BLOBS_H
#define BLOBS_H

/*******************************
 *	       BLOBS		*
 *******************************/

#define PL_BLOB_MAGIC_B 0x75293a00 /* Magic to validate a blob-type */
#define PL_BLOB_VERSION 1
#define PL_BLOB_MAGIC (PL_BLOB_MAGIC_B | PL_BLOB_VERSION)

#define PL_BLOB_UNIQUE 0x01 /* Blob content is unique */
#define PL_BLOB_TEXT 0x02   /* blob contains text */
#define PL_BLOB_NOCOPY 0x04 /* do not copy the data */
#define PL_BLOB_WCHAR 0x08  /* wide character string */

typedef struct _PL_blob_t {
  uintptr_t magic; /* YAP_BLOB_MAGIC */
  uintptr_t flags; /* YAP_BLOB_* */
  char *name;      /* name of the type */
  int (*release)(YAP_Atom a);
  int (*compare)(YAP_Atom a, YAP_Atom b);
#ifdef SIO_MAGIC
  int (*write)(FILE *s, YAP_Atom a, int flags);
#else
  int (*write)(void *s, YAP_Atom a, int flags);
#endif
  void (*acquire)(YAP_Atom a);
#ifdef SIO_MAGIC
  int (*save)(YAP_Atom a, FILE *s);
  YAP_Atom (*load)(FILE *s);
#else
  int (*save)(YAP_Atom a, void *);
  YAP_Atom (*load)(void *s);
#endif
  /* private */
  void *reserved[10];      /* for future extension */
  int registered;          /* Already registered? */
  int rank;                /* Rank for ordering atoms */
  struct _PL_blob_t *next; /* next in registered type-chain */
  YAP_Atom atom_name;          /* Name as atom */
} blob_type_t;

// typedef struct _PL_blob_t PL_blob_t;
#define YAP_BLOB_MAGIC_B PL_BLOB_MAGIC_B
#define YAP_blob_t blob_type_t
#define PL_blob_t blob_type_t

#ifdef _FLI_H_INCLUDED

PL_EXPORT(int) PL_is_blob(term_t t, PL_blob_t **type);
PL_EXPORT(int) PL_unify_blob(term_t t, void *blob, size_t len, PL_blob_t *type);
PL_EXPORT(int) PL_put_blob(term_t t, void *blob, size_t len, PL_blob_t *type);
PL_EXPORT(int)
PL_get_blob(term_t t, void **blob, size_t *len, PL_blob_t **type);

PL_EXPORT(void *) PL_blob_data(atom_t a, size_t *len, PL_blob_t **type);

PL_EXPORT(void) PL_register_blob_type(PL_blob_t *type);
PL_EXPORT(PL_blob_t *) PL_find_blob_type(const char *name);
PL_EXPORT(int) PL_unregister_blob_type(PL_blob_t *type);
PL_EXPORT(int) PL_raise(int sig);

#endif

#ifdef YATOM_H
extern int Yap_write_blob(AtomEntry *ref, FILE *stream);
extern char *Yap_blob_to_string(AtomEntry *ref, const char *s, size_t sz);
#endif
extern X_API bool YAP_is_blob(YAP_Term t, YAP_blob_t **type);
extern X_API bool YAP_unify_blob(YAP_Term *t, void *blob, size_t len,
                                 YAP_blob_t *type);
extern X_API bool YAP_put_blob(YAP_Term *t, void *blob, size_t len,
                               YAP_blob_t *type);
extern X_API bool YAP_get_blob(YAP_Term t, void **blob, size_t *len,
                               YAP_blob_t **type);

extern X_API void *YAP_blob_data(YAP_Atom a, size_t *len, YAP_blob_t **type);

extern X_API void YAP_register_blob_type(YAP_blob_t *type);
extern X_API YAP_blob_t *YAP_find_blob_type(const char *name);
// extern X_API YAP_blob_t *YAP_find_blob_type(YAP_Atom at);
extern X_API bool YAP_unregister_blob_type(YAP_blob_t *type);

#endif
