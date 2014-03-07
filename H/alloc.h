/*************************************************************************
*									 *
*	 YAP Prolog  %W% %G%						 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		alloc.h							 *
* Last rev:								 *
* mods:									 *
* comments:	allocating space					 *
*									 *
*************************************************************************/

/* Space Organization:
	The data areas is divided in the following way:

 Lower Addresses
	HeapBase	\	
				 |	Atom and code space
	HeapTop-1	/
	
	HeapTop		\
				 | 	Free Space
	HeapMax		/

	GlobalBase-1/

	GlobalBase	\	
				 |	Global Stack
	H			/
	
	ASP			\
				 |	Local stack
	LocalBase	/

	TRBase			/
	
	TR		\
				 |  Trail
 Higher Adresses

*/

/* definitions required by saver/restorer and memory manager */

typedef	CELL YAP_SEG_SIZE;

typedef	struct FREEB {
  YAP_SEG_SIZE		b_size;
  struct FREEB		*b_next;
  struct FREEB		*b_next_size;
} BlockHeader;

#define K		((Int) 1024)

#define MinBlockSize	(sizeof(BlockHeader)+sizeof(YAP_SEG_SIZE))
#define MaxBlockSize	0xffffff
#define	InUseFlag	0x80000000

/* the following defines are machine dependant and are used to enforce
   the correct alignment for allocated blocks							*/

#if SIZEOF_INT_P==4
#define YAP_ALIGN		3
#define YAP_ALIGNMASK		((CELL)(-4))
#else
#define YAP_ALIGN		7
#define YAP_ALIGNMASK		((CELL)(-8))
#endif	/* ALIGN_LONGS */

#define AdjustSize(X)	((X+YAP_ALIGN) & YAP_ALIGNMASK)

/* SIZE should be a power of 2 */
#define ALIGN_SIZE(X,SIZE) (((CELL)(X)+((SIZE)-1)) & ~((SIZE)-1))

/* I'll assume page size is always a power of two */
#if defined(_WIN32) || defined(__CYGWIN__)
/* in WIN32 VirtualAlloc works in multiples of 64K */
#define YAP_ALLOC_SIZE (64*1024)
#define LGPAGE_SIZE YAP_ALLOC_SIZE

#else
#define YAP_ALLOC_SIZE Yap_page_size
#define LGPAGE_SIZE (16*Yap_page_size)
#endif

#define AdjustPageSize(X)  (((X)+ (YAP_ALLOC_SIZE-1))/YAP_ALLOC_SIZE)*YAP_ALLOC_SIZE;
#define AdjustLargePageSize(X)  (((X)+ (LGPAGE_SIZE-1))/LGPAGE_SIZE)*LGPAGE_SIZE;

#define BlockTrailer(b)		((YAP_SEG_SIZE *)b)[((BlockHeader *) b)->b_size]

/* Operating system and architecture dependent page size */
extern int Yap_page_size;

void   Yap_InitHeap(void *);
UInt   Yap_ExtendWorkSpaceThroughHole(UInt);
void   Yap_AllocHole(UInt, UInt);

#if USE_SYSTEM_MMAP && ! defined(__CYGWIN__)

#include <sys/types.h>
#include <sys/mman.h>

#elif USE_SYSTEM_SHM



#elif USE_SBRK

#if (defined(__svr4__) || defined(__SVR4))
#include <unistd.h>
#elif sun
#include <sys/types.h>
#include <malloc.h>
void *sbrk(caddr_t);

typedef unsigned size_t;

MALLOC_T malloc(size_t);
void free(MALLOC_T);
MALLOC_T realloc(MALLOC_T,size_t);
MALLOC_T calloc(size_t,size_t);

#endif


#else


#endif

void Yap_add_memory_hole(ADDR, ADDR);

#define SCRATCH_START_SIZE        K64
#define SCRATCH_INC_SIZE          K64

