
#include "pl-incl.h"


		/********************************
		*            STRINGS            *
		*********************************/


#ifdef O_DEBUG
#define CHAR_INUSE 0x42
#define CHAR_FREED 0x41

char *
store_string(const char *s)
{ if ( s )
  { GET_LD
    char *copy = (char *)allocHeap(strlen(s)+2);

    *copy++ = CHAR_INUSE;
    strcpy(copy, s);

    return copy;
  } else
  { return NULL;
  }
}


void
remove_string(char *s)
{ if ( s )
  { GET_LD
    assert(s[-1] == CHAR_INUSE);
    
    s[-1] = CHAR_FREED;
    freeHeap(s-1, strlen(s)+2);
  }
}

#else /*O_DEBUG*/

char *
store_string(const char *s)
{ if ( s )
  { GET_LD
  
    char *copy = (char *)allocHeap(strlen(s)+1);
    
    strcpy(copy, s);
    return copy;
  } else
  { return NULL;
  }
}


void
remove_string(char *s)
{ if ( s )
  { GET_LD
    freeHeap(s, strlen(s)+1);
  }
}

#endif /*O_DEBUG*/

