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
	AuxSp		\
				 |	Auxiliary stack
	AuxTop		/

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

#define MinBlockSize	(sizeof(BlockHeader)+sizeof(YAP_SEG_SIZE))
#define MaxBlockSize	0xffffff
#define	InUseFlag	0x1000000

/* the following defines are machine dependant and are used to enforce
   the correct alignment for allocated blocks							*/

#if SIZEOF_INT_P==4
#define YAP_ALIGN		3
#define YAP_ALIGNMASK		0xfffffffc
#else
#define YAP_ALIGN		7
#define YAP_ALIGNMASK		0xfffffff8L
#endif	/* ALIGN_LONGS */

#define AdjustSize(X)	((X+YAP_ALIGN) & YAP_ALIGNMASK)

#define ALIGN_YAPTYPE(X,TYPE) (((CELL)(X)+(sizeof(TYPE)-1)) & ~(sizeof(TYPE)-1))

/* I'll assume page size is always a power of two */
#define AdjustPageSize(X)  ((X) & (Yap_page_size-1) ? \
      ((X) + Yap_page_size) & (~(Yap_page_size-1)) :      \
      (X) )

#define BlockTrailer(b)		((YAP_SEG_SIZE *)b)[((BlockHeader *) b)->b_size]

#define FreeBlocks heap_regs->free_blocks

/* Operating system and architecture dependent page size */
extern int Yap_page_size;

void   STD_PROTO(Yap_InitHeap, (void *));

#if USE_MMAP

#include <sys/types.h>
#include <sys/mman.h>

#elif USE_SHM



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

#if defined(YAPOR) || defined(THREADS)
#define HEAPTOP_OWNER(worker_id)  ((worker_id) == HeapTopOwner)
#define HEAPTOP_OWN(worker_id)  (HeapTopOwner = (worker_id))
#define HEAPTOP_DISOWN(worker_id)  (HeapTopOwner = -1)
#else
#define HEAPTOP_OWNER(worker_id)   (FALSE)
#define HEAPTOP_OWN(worker_id)  
#define HEAPTOP_DISOWN(worker_id)
#endif
