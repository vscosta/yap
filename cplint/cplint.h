/*
	LPAD and CP-Logic interpreter

Copyright (c) 2007, Fabrizio Riguzzi

For the use of cudd:
Copyright (c) 1995-2004, Regents of the University of Colorado

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

Neither the name of the University of Colorado nor the names of its
contributors may be used to endorse or promote products derived from
this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

*/

#include "util.h"
#include "cuddInt.h"
#include "array.h"
#include "mtr.h"
#include "avl.h"
#include "Yap/YapInterface.h"
#include <glib.h>
 
typedef struct
  {
    int var,value;
  } factor;

typedef struct
  {
    int nVal,nBit;
    array_t * probabilities;
    array_t * booleanVars;
  } variable;


void createVars(array_t * vars, YAP_Term t,DdManager * mgr, array_t * bVar2mVar, char inames[1000][20]);
void createExpression(array_t * expression, YAP_Term t);
static int compute_prob(void);
void init_my_predicates();
int compare(char *a, char *b);
gint my_equal(gconstpointer v,gconstpointer v2);
guint my_hash(gconstpointer key);
void  dealloc(gpointer key,gpointer value,gpointer user_data);



DdNode * retFunction(DdManager * mgr, array_t * expression,array_t * v);
DdNode * retTerm(DdManager * mgr,array_t *factors,array_t * v);
DdNode * retFactor(DdManager * mgr, factor f, array_t * v);

double Prob(DdNode *node, array_t * vars,array_t * bVar2mVar, GHashTable * nodes);

double ProbBool(DdNode *node, int bits, int nBit,int posBVar,variable v,
array_t * vars,array_t * bVar2mVar, GHashTable * nodes);
