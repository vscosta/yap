#include <stdio.h>
#include <assert.h>

#include "uthash_udi.h"
#include "uthash_udi_private.h"

static struct udi_control_block UTHashCB;

void udi_uthash_init(void) {
	UdiControlBlock cb = &UTHashCB;

	memset((void *) cb,0, sizeof(*cb));

	cb->decl=YAP_LookupAtom(SPEC);

	cb->init=UTHashUdiInit;
	cb->insert=UTHashUdiInsert;
	cb->search=UTHashUdiSearch;
	cb->destroy=UTHashUdiDestroy;

	Yap_UdiRegister(cb);
}

void *UTHashUdiInit (YAP_Term spec, int arg, int arity) {
	return NULL; /*empty uthash*/
}

void *UTHashUdiInsert (void *control,
		YAP_Term term, int arg, void *data)
{
  uthash_t hash = (uthash_t) control;
  YAP_Term argterm;
  uthash_t element;

//  Yap_DebugPlWrite(term); fprintf(stderr, "\n");
  argterm = YAP_ArgOfTerm(arg,term);

  if (YAP_IsAtomTerm(argterm) || YAP_IsIntTerm(argterm))
    {
      element = (uthash_t) malloc(sizeof(*element));
      element->data = data;
      if (YAP_IsAtomTerm(argterm))
        element->key.atom = YAP_AtomOfTerm(argterm);
      else
        element->key.integer = YAP_IntOfTerm(argterm);

      HASH_ADD_AI(hash, element);
    }

  /*TODO: check how to handle if a different value appears*/
  return (void *) hash;
}

/*ARGS ARE AVAILABLE*/
int UTHashUdiSearch (void *control,
		int arg, Yap_UdiCallback callback, void *args)
{
  YAP_Term argterm;
  YAP_Atom atom;
  uthash_t element;
  uthash_t hash = (uthash_t) control;
  int count = 0;
  union AI ai;

  assert(hash);

  argterm = YAP_A(arg); /*Deref(XREGS[arg]); */

  if (YAP_IsAtomTerm(argterm) || YAP_IsIntTerm(argterm))
    {
	  if (YAP_IsAtomTerm(argterm))
		  ai.atom = YAP_AtomOfTerm(argterm);
	  else
		  ai.integer = YAP_IntOfTerm(argterm);

      HASH_FIND_AI(hash,&ai,element);
      /* HASH_FIND(hh,utcontrol->tree,&atom,sizeof(Atom),element); */
      while (element)
        {
    	  callback((void *) &(element->key), element->data, args);
          count ++;
          HASH_FIND_NEXT_AI(element,&ai);
        }
//      fprintf(stderr,"found %d\n",count);
      return (count);
    }
//  fprintf(stderr,"not found\n");
  return -1; /*YAP FALLBACK*/
}

int UTHashUdiDestroy(void *control)
{
  uthash_t hash = (uthash_t) control;

  assert(hash);

  if (hash)
    HASH_CLEAR(hh,hash); /* TODO: check if this is enough */

  return TRUE;
}
