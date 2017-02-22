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

#if !defined(X_API) && !defined(SWIGYAP) 
#if (defined(_MSC_VER) || defined(__MINGW32__)) && defined(PL_KERNEL)
#define X_API __declspec(dllexport)
#else
#define X_API
#endif
#endif

/*******************************
 *	       BLOBS		*
 *******************************/

#define YAP_BLOB_MAGIC_B	0x75293a00	/* Magic to validate a blob-type */
#define PL_BLOB_VERSION (YAP_BLOB_MAGIC_B|PL_BLOB_VERSION)

#define PL_BLOB_UNIQUE	0x01		/* Blob content is unique */
#define PL_BLOB_TEXT	  0x02		/* blob contains text */
#define PL_BLOB_NOCOPY	0x04		/* do not copy the data */
#define PL_BLOB_WCHAR	0x08		/* wide character string */

typedef struct YAP_blob_t
{ uintptr_t		magic;		/* YAP_BLOB_MAGIC */
    uintptr_t		flags;		/* YAP_BLOB_* */
    char *		name;		/* name of the type */
    int			(*release)(Atom a);
    int			(*compare)(Atom a, Atom b);
#ifdef SIO_MAGIC
    int			(*write)(FILE *s, Atom a, int flags);
#else
    int			(*write)(void *s, Atom a, int flags);
#endif
    void			(*acquire)(Atom a);
#ifdef SIO_MAGIC
    int			(*save)(Atom a, FILE *s);
    Atom		(*load)(FILE *s);
#else
    int			(*save)(Atom a, void*);
    Atom		(*load)(void *s);
#endif
    /* private */
    void *		reserved[10];	/* for future extension */
    int			registered;	/* Already registered? */
    int			rank;		/* Rank for ordering atoms */
    struct YAP_blob_t *    next;		/* next in registered type-chain */
    Atom		atom_name;	/* Name as atom */
} blob_type_t;

int Yap_write_blob(AtomEntry *ref, FILE *stream);
char * Yap_blob_to_string(AtomEntry *ref, const char *s, size_t sz);
X_API bool	  YAP_is_blob(YAP_Term t, blob_type_t **type);
X_API bool		YAP_unify_blob(YAP_Term *t, void *blob, size_t len,
                                blob_type_t *type);
X_API bool		YAP_put_blob(YAP_Term *t, void *blob, size_t len,
                              blob_type_t *type);
X_API bool		YAP_get_blob(YAP_Term t, void **blob, size_t *len,
                              blob_type_t **type);

X_API void*	YAP_blob_data(Atom a,
                               size_t *len,
                               struct YAP_blob_t **type);

X_API void		YAP_register_blob_type(blob_type_t *type);
X_API blob_type_t*	YAP_find_blob_type(const char* name);
//YAP_blob_type_t*	YAP_find_blob_type(Atom at);
X_API bool		YAP_unregister_blob_type(blob_type_t *type);


#endif
