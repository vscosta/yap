/*************************************************************************
 *									 *
 *	 YAP Prolog 							 *
 *	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
 *									 *
 * Copyright V.Santos Costa and Universidade do Porto 1985--		 *
 *									 *
 **************************************************************************
 *									 *
 * File:		blobs.c *
 * comments:	support blobs in YAP definition 			 *
 *									 *
 * Last rev:	$Date: $,$Author: vsc $					 *
 *									 *
 *									 *
 *************************************************************************/
/**
 *
 * @{
 *   @file swi/fli/blobs.c
 *
 *   @addtogroup swi-c-interface
 *
 */
#define _EXPORT_KERNEL 1

#include <Yap.h>
#include <Yatom.h>
#include <iopreds.h>
#include <stdio.h>

#include <string.h>

#include "YapText.h"

/* for freeBSD9.1 */
#define _WITH_DPRINTF
#include <stdio.h>

//#include	<SWI-Stream.h>
//#include	<pl-shared.h>

#include "swi.h"

static PL_blob_t unregistered_blob_atom = {
    PL_BLOB_MAGIC, PL_BLOB_NOCOPY | PL_BLOB_TEXT, "unregistered"};

X_API int PL_is_blob(term_t t, PL_blob_t **type) {
  CACHE_REGS
  Term yt = Yap_GetFromSlot(t);
  Atom a;
  YAP_BlobPropEntry *b;

  if (IsVarTerm(yt))
    return FALSE;
  if (!IsAtomTerm(yt))
    return FALSE;
  a = AtomOfTerm(yt);
  if (!IsBlob(a))
    return FALSE;
  b = RepBlobProp(a->PropsOfAE);
  *type = (PL_blob_t *)b->blob_type;
  return TRUE;
}

/* void check_chain(void); */

PL_EXPORT(int)
PL_unify_blob(term_t t, void *blob, size_t len, PL_blob_t *type) {
  CACHE_REGS
  AtomEntry *ae;

  if (!blob)
    return FALSE;
  ae = Yap_lookupBlob(blob, len, type, NULL);
  if (!ae) {
    return FALSE;
  }
  if (type->acquire) {
    type->acquire((AbsAtom(ae)));
  }
  return Yap_unify(Yap_GetFromSlot(t), MkAtomTerm(AbsAtom(ae)));
}

PL_EXPORT(int)
PL_put_blob(term_t t, void *blob, size_t len, PL_blob_t *type) {
  CACHE_REGS
  AtomEntry *ae;
  int ret;

  if (!blob)
    return FALSE;
  ae = Yap_lookupBlob(blob, len, type, &ret);
  if (!ae) {
    return FALSE;
  }
  if (type->acquire) {
    type->acquire((AbsAtom(ae)));
  }
  Yap_PutInSlot(t, MkAtomTerm(AbsAtom(ae)));
  return ret;
}

PL_EXPORT(int)
PL_get_blob(term_t t, void **blob, size_t *len, PL_blob_t **type) {
  CACHE_REGS
  Atom a;
  Term tt;
  AtomEntry *ae;

  tt = Yap_GetFromSlot(t);
  if (IsVarTerm(tt))
    return FALSE;
  if (!IsAtomTerm(tt))
    return FALSE;
  a = AtomOfTerm(tt);
  if (!IsBlob(a))
    return FALSE;
  ae = RepAtom(a);
  if (type)
    *type = (PL_blob_t *)RepBlobProp(ae->PropsOfAE)->blob_type;
  if (len)
    *len = ae->rep.blob[0].length;
  if (blob)
    *blob = ae->rep.blob[0].data;
  return true;
}

PL_EXPORT(void *)
PL_blob_data(atom_t a, size_t *len, PL_blob_t **type) {
  Atom x = SWIAtomToAtom(a);

  if (!IsBlob(x)) {
    if (len)
      *len = strlen_utf8(x->UStrOfAE);
    if (type)
      *type = &unregistered_blob_atom;
    return x->StrOfAE;
  }
  size_t sz = x->rep.blob[0].length+1;
  if (len)
    *len = sz;
  if (type)
    *type = (PL_blob_t *)RepBlobProp(x->PropsOfAE)->blob_type;
  void *o = malloc(sz+1);

  memcpy(o, x->rep.blob[0].data, sz);
  return o;
}

PL_EXPORT(void)
PL_register_blob_type(PL_blob_t *type) {
  type->next = BlobTypes;
  BlobTypes = type;
}

PL_EXPORT(PL_blob_t *)
PL_find_blob_type(const char *name) {
  Atom at = Yap_LookupAtom((char *)name);

  return YAP_find_blob_type(RepAtom(at)->StrOfAE);
}

PL_EXPORT(int)
PL_unregister_blob_type(PL_blob_t *type) {
  fprintf(stderr, "PL_unregister_blob_type not implemented yet\n");
  return FALSE;
}

/**
 * @}
 */
