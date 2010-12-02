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

#include	<SWI-Prolog.h>

#include "swi.h"


PL_EXPORT(int)
PL_is_blob(term_t t, PL_blob_t **type)
{
  Term yt = Yap_GetFromSlot(t);
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

PL_EXPORT(int)		
PL_unify_blob(term_t t, void *blob, size_t len, PL_blob_t *type)
{
  fprintf(stderr,"PL_unify_blob not implemented yet\n");
  return FALSE;
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
	*type = SWI_Blobs;
      return x->WStrOfAE;
    }
    if ( len )
      *len = strlen(x->StrOfAE);
      if ( type )
	*type = SWI_Blobs;
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
  fprintf(stderr,"PL_register_blob_type not implemented yet\n");
}

PL_EXPORT(PL_blob_t*)	
PL_find_blob_type(const char* name)
{
  fprintf(stderr,"PL_find_blob_type not implemented yet\n");
  return NULL;
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
