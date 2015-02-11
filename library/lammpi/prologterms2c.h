/*
Copyright (C) 2004,2005,2006 (Nuno A. Fonseca) <nuno.fonseca@gmail.com>

This program is free software; you can redistribute it and/or 
modify it under the terms of the GNU General Public License 
as published by the Free Software Foundation; either 
version 2 of the License, or (at your option) any later 
version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


Last rev: $Id: prologterms2c.h,v 1.2 2006-06-04 19:02:07 nunofonseca Exp $
Comments: This file provides a set of functions to convert a prolog term to a C string and back.
*/
#ifndef PROLOGTERMS2C
#define PROLOGTERMS2C 1

#ifndef _yap_c_interface_h
#include <YapInterface.h>
//#include <yap_structs.h>
#endif

#ifndef size_t
#include <unistd.h>
#endif
#include <stdarg.h>
/*
 * Converts a term t into a string.  
 * An internal representation of t is
 * copied to ptr if it occupies less than size. Otherwise the
 * necessary memory is aloccated (dyn_ptr) and the ascii
 * representation of the term is copied to there.
 */
char* term2string(char *const ptr,size_t *size, const YAP_Term t);
/*
 * Converts a string with a ascci representation of a term into a term.
 * The ascii representation of t is
 * copied to ptr if it occupies less than *size. Otherwise the
 * necessary memory is aloccated  and the ascii
 * representation of the term is copied to there.
 */
YAP_Term string2term(char *const ptr,const size_t *size);
/*
 * Read a prolog term from a stream
 * (the prolog term must have been writen by the write_term_to_stream)
 */
YAP_Term read_term_from_stream(const int fd);
/*
 * Writes a term to a stream.
 */
size_t write_term_to_stream(const int fd,const YAP_Term term);
/*
 * Changes the size of the buffer to contain at least newsize bytes.
 * Useful to reduce the number of reallocs,mallocs, and frees
 */
void change_buffer_size(const size_t newsize);

void write_msg(const char *fun,const char *file, int line,const char *format, ...);
/*********************************************************************************************
 * Macros to manipulate the buffer
 *********************************************************************************************/

extern size_t BLOCK_SIZE;

#define buffer (buffers[YAP_ThreadSelf()])

// deletes the buffer (all fields) but does not release the memory of the buffer.ptr
#define DEL_BUFFER()   {buffer.ptr=NULL;buffer.size=0;buffer.len=0;buffer.pos=0;}
//  informs the prologterm2c module that the buffer is now used and should not be messed
#define USED_BUFFER()  DEL_BUFFER()
// initialize buffer
#define RESET_BUFFER() \
{buffer.len=0;change_buffer_size(BLOCK_SIZE);buffer.pos=0;}
#define BUFFER_PTR   buffer.ptr
#define BUFFER_SIZE  buffer.size
#define BUFFER_LEN   buffer.len
#define BUFFER_POS   buffer.pos
// copies two buffers
#define COPY_BUFFER_DS(src,dst) {dst.size=src.size;dst.len=src.len;dst.ptr=src.ptr;dst.pos=src.pos;}

/*********************************************************************************************
 * Buffer
 *********************************************************************************************/
struct buffer_ds {
  size_t size,  // size of the buffer
         len;   // size of the string
  char *ptr;    // pointer to the buffer
  size_t pos;    // position (used while reading)
};
extern struct buffer_ds buffers[1024];


#endif
