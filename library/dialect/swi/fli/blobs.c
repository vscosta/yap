/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright V.Santos Costa and Universidade do Porto 1985--		 *
*									 *
**************************************************************************
*									 *
* File:		blobs.c							 *
* comments:	support blobs in YAP definition 			 *
*									 *
* Last rev:	$Date: $,$Author: vsc $					 *
*									 *
*									 *
*************************************************************************/

#include	<Yap.h>
#include	<Yatom.h>

#include <string.h>

/* for freeBSD9.1 */
#define _WITH_DPRINTF
#include <stdio.h>

#include	<SWI-Prolog.h>

#include "swi.h"

static PL_blob_t unregistered_blob_atom =
{ PL_BLOB_MAGIC,
  PL_BLOB_NOCOPY|PL_BLOB_TEXT,
  "unregistered"
};


PL_EXPORT(int)
PL_is_blob(term_t t, PL_blob_t **type)
{
  CACHE_REGS
  Term yt = Yap_GetFromSlot(t PASS_REGS);
  Atom a;
  BlobPropEntry *b;

  if (IsVarTerm(yt))
    return FALSE;
  if (!IsAtomTerm(yt))
    return FALSE;
  a = AtomOfTerm(yt);
  if (!IsBlob(a))
    return FALSE;
  b = RepBlobProp(a->PropsOfAE);
  *type = b->blob_t;
  return TRUE;
}


/* void check_chain(void); */

/* void check_chain(void) { */
/*   AtomEntry *ae, *old; */
/*     ae = SWI_Blobs; */
/*     old = NULL; */
/*     while (ae) { */
/*       old = ae; */
/*       ae = RepAtom(ae->NextOfAE); */
/*     } */
/* } */

static AtomEntry *
lookupBlob(void *blob, size_t len, PL_blob_t *type)
{
  BlobPropEntry *b;
  AtomEntry *ae;

  LOCK(SWI_Blobs_Lock);
  if (type->flags & PL_BLOB_UNIQUE) {
    /* just keep a linked chain for now */
    ae = SWI_Blobs;
    while (ae) {
      if (ae->PropsOfAE &&
	  RepBlobProp(ae->PropsOfAE)->blob_t == type &&
	  ae->rep.blob->length == len &&
	  !memcmp(ae->rep.blob->data, blob, len)) {
	UNLOCK(SWI_Blobs_Lock);
	return ae;
      }
      ae = RepAtom(ae->NextOfAE);
    }
  }
  b = (BlobPropEntry *)Yap_AllocCodeSpace(sizeof(BlobPropEntry));
  if (!b) {
    UNLOCK(SWI_Blobs_Lock);
    return NULL;
  }
  b->NextOfPE = NIL;
  b->KindOfPE = BlobProperty;
  b->blob_t = type;
  ae = (AtomEntry *)Yap_AllocCodeSpace(sizeof(AtomEntry)+len+sizeof(size_t));
  if (!ae) {
    UNLOCK(SWI_Blobs_Lock);
    return NULL;
  }
  NOfBlobs++;
  INIT_RWLOCK(ae->ARWLock);
  ae->PropsOfAE = AbsBlobProp(b);
  ae->NextOfAE = AbsAtom(SWI_Blobs);
  ae->rep.blob->length = len;
  memcpy(ae->rep.blob->data, blob, len);
  SWI_Blobs = ae;
  UNLOCK(SWI_Blobs_Lock);
  if (NOfBlobs > NOfBlobsMax) {
    Yap_signal(YAP_CDOVF_SIGNAL);
  }
  return ae;
}

PL_EXPORT(int)		
PL_unify_blob(term_t t, void *blob, size_t len, PL_blob_t *type)
{
  CACHE_REGS
  AtomEntry *ae;

  if (!blob)
    return FALSE;
  ae = lookupBlob(blob, len, type);
  if (!ae) {
    return FALSE;
  }
  if (type->acquire) {
    type->acquire(AtomToSWIAtom(AbsAtom(ae)));
  }
  return Yap_unify(Yap_GetFromSlot(t PASS_REGS), MkAtomTerm(AbsAtom(ae)));
}

PL_EXPORT(int)	
PL_put_blob(term_t t, void *blob, size_t len, PL_blob_t *type)
{
  fprintf(stderr,"PL_put_blob not implemented yet\n");
  return FALSE;
}

PL_EXPORT(int)	
PL_get_blob(term_t t, void **blob, size_t *len, PL_blob_t **type)
{
  fprintf(stderr,"PL_get_blob not implemented yet\n");
  return FALSE;
}

PL_EXPORT(void*)	
PL_blob_data(atom_t a, size_t *len, struct PL_blob_t **type)
{
  Atom x = SWIAtomToAtom(a);

  if (!IsBlob(x)) {
    if (IsWideAtom(x)) {
      if ( len )
	*len = wcslen(x->WStrOfAE);
      if ( type )
	*type = &unregistered_blob_atom;
      return x->WStrOfAE;
    }
    if ( len )
      *len = strlen(x->StrOfAE);
      if ( type )
	*type = &unregistered_blob_atom;
      return x->StrOfAE;
  }
  if ( len )
    *len = x->rep.blob[0].length;
  if ( type )
    *type = RepBlobProp(x->PropsOfAE)->blob_t;

  return x->rep.blob[0].data;
}

PL_EXPORT(void)
PL_register_blob_type(PL_blob_t *type)
{
  type->next = SWI_BlobTypes;
  SWI_BlobTypes = type;
}

PL_EXPORT(PL_blob_t*)	
PL_find_blob_type(const char* name)
{
  Atom at = Yap_LookupAtom((char *)name);

  return YAP_find_blob_type((YAP_Atom)at);
}

PL_EXPORT(PL_blob_t*)	
YAP_find_blob_type(YAP_Atom at)
{
  AtomEntry *a = RepAtom((Atom)at);
  if (!IsBlob(a)) {
    return &unregistered_blob_atom;
  }
  return RepBlobProp(a->PropsOfAE)->blob_t;
}

PL_EXPORT(int)		
PL_unregister_blob_type(PL_blob_t *type)
{
  fprintf(stderr,"PL_unregister_blob_type not implemented yet\n");
  return FALSE;
}

void
Yap_install_blobs(void)
{

}
