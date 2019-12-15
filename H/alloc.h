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

#ifndef ALLOC_H
#define ALLOC_H 1

/**
  @defgroup MemAlloc Memory Allocation in YAP
  @ingroup  Imp
  @{
  @brief Memory organization and Auxiliary Memory Access Data-structures and
  routines.

@ section Space Organization:
        The data areas is divided in the following way:

~~~
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
~~~
*/

 /** definitions required by saver/restorer and memory manager */

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

#define MaxTmp(USES_REGS1) (MaxBlockSize+1)

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
extern size_t Yap_page_size;

extern void   Yap_InitHeap(void *);
extern UInt   Yap_ExtendWorkSpaceThroughHole(UInt);
extern void   Yap_AllocHole(UInt, UInt);
extern size_t Yap_HeapUsed(void);
;
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

extern MALLOC_T malloc(size_t);
extern void free(MALLOC_T);
extern MALLOC_T realloc(MALLOC_T,size_t);
extern MALLOC_T calloc(size_t,size_t);

#endif


#else


#endif

void Yap_add_memory_hole(ADDR, ADDR);

#define SCRATCH_START_SIZE        K64
#define SCRATCH_INC_SIZE          K64

/**
 *  @defgroup ScratchBuf Using a scratch buffer
 *  @ingroup MemAlloc
 *  @brief use a scratch buffer (maybe even many), 
 *
 *  YAP now includes internal routines to fetch and release a scratch buffer.  This is thread-scopeD. reentrancy is allowed.
 */

/// thread view of the scratch buffer
typedef struct scratch_buf_struct_t {
  void *data; ///> buffer data
  size_t sz;  ///> buffer size
  bool in_use; ///> buffer is in use, hopefully by a caller.
} scratch_sys_struct_t;

/// user view of the scratch buffer
typedef struct scratch_user_buf_struct_t {
  void *data;  ///> the goods
  bool is_thread_scratch_buf; ///> are we using Local_ScratchBuf for this
  size_t n_of, size_of; /// what we asked/are asking now.
} scratch_struct_t;

extern bool get_scratch(scratch_struct_t *handle);
extern bool Yap_get_scratch_buf( scratch_struct_t *handle, size_t nof, size_t each);
extern bool Yap_realloc_scratch_buf(scratch_struct_t *handle, size_t nof);
extern bool Yap_release_scratch_buf(scratch_struct_t *handle);

/// @}


/** @defgroup StackDisc
 *  @ingroup MemAlloc
 *  @brief memory who lives during a stack activation. In other words,
 *  most often neds no `free()`.

@{
*/

/// allocate a temporary text block
///
extern void *Malloc(size_t sz USES_REGS);
extern void *Realloc(void *buf, size_t sz USES_REGS);
extern void Free(void *buf USES_REGS);

extern void *MallocAtLevel(size_t sz, int atL USES_REGS);
#define BaseMalloc(sz) MallocAtLevel(sz, 1)
extern const void *MallocExportAsRO(const void *blk);


#define MBYTE (1024 * 1024)

/* Character types for tokenizer and write.c */
extern int AllocLevel(void);

#if 0
#define push_text_stack()						\
  ( fprintf(stderr, " + *** %d %s:%s:%d\n", AllocLevel(),		\
         __FILE__,  __FUNCTION__, __LINE__), 	    \
   push_text_stack__(PASS_REGS1))

   #define pop_text_stack(lvl)						\
  ( fprintf(stderr, " - *** %d %s:%s:%d\n", AllocLevel(), __FILE__,	\
     __FUNCTION__, __LINE__),						\
   pop_text_stack__(lvl PASS_REGS))

   #define pop_output_text_stack(lvl,p)					\
  (fprintf(stderr, "-- *** %d %s:%s:%d\n", AllocLevel(), __FILE__,	\
     __FUNCTION__, __LINE__),					\
   pop_output_text_stack__(lvl,p))
#else
#define push_text_stack() push_text_stack__(PASS_REGS1)
#define pop_text_stack(lvl)	 pop_text_stack__(lvl PASS_REGS)
#define pop_output_text_stack(lvl,p) pop_output_text_stack__(lvl,p PASS_REGS)
#endif

extern int push_text_stack__(USES_REGS1);
extern int pop_text_stack__(int lvl USES_REGS);


extern void *pop_output_text_stack__(int lvl, const void *ox USES_REGS);


#endif
/// @}
