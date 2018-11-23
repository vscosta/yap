/******************************************************************************\
*                                                                              *
*    SimpleCUDD library (www.cs.kuleuven.be/~theo/tools/simplecudd.html)       *
*  SimpleCUDD was developed at Katholieke Universiteit Leuven(www.kuleuven.be) *
*                                                                              *
*  Copyright Katholieke Universiteit Leuven 2008                               *
*                                                                              *
*  Author: Theofrastos Mantadelis                                              *
*  File: simplecudd.h                                                          *
*                                                                              *
********************************************************************************
*                                                                              *
* Artistic License 2.0                                                         *
*                                                                              *
* Copyright (c) 2000-2006, The Perl Foundation.                                *
*                                                                              *
* Everyone is permitted to copy and distribute verbatim copies of this license *
* document, but changing it is not allowed.                                    *
*                                                                              *
* Preamble                                                                     *
*                                                                              *
* This license establishes the terms under which a given free software Package *
* may be copied, modified, distributed, and/or redistributed. The intent is    *
* that the Copyright Holder maintains some artistic control over the           *
* development of that Package while still keeping the Package available as     *
* open source and free software.                                               *
*                                                                              *
* You are always permitted to make arrangements wholly outside of this license *
* directly with the Copyright Holder of a given Package. If the terms of this  *
* license do not permit the full use that you propose to make of the Package,  *
* you should contact the Copyright Holder and seek a different licensing       *
* arrangement.                                                                 *
* Definitions                                                                  *
*                                                                              *
* "Copyright Holder" means the individual(s) or organization(s) named in the   *
* copyright notice for the entire Package.                                     *
*                                                                              *
* "Contributor" means any party that has contributed code or other material to *
* the Package, in accordance with the Copyright Holder's procedures.           *
*                                                                              *
* "You" and "your" means any person who would like to copy, distribute, or     *
* modify the Package.                                                          *
*                                                                              *
* "Package" means the collection of files distributed by the Copyright Holder, *
* and derivatives of that collection and/or of those files. A given Package    *
* may consist of either the Standard Version, or a Modified Version.           *
*                                                                              *
* "Distribute" means providing a copy of the Package or making it accessible   *
* to anyone else, or in the case of a company or organization, to others       *
* outside of your company or organization.                                     *
*                                                                              *
* "Distributor Fee" means any fee that you charge for Distributing this        *
* Package or providing support for this Package to another party. It does not  *
* mean licensing fees.                                                         *
*                                                                              *
* "Standard Version" refers to the Package if it has not been modified, or has *
* been modified only in ways explicitly requested by the Copyright Holder.     *
*                                                                              *
* "Modified Version" means the Package, if it has been changed, and such       *
* changes were not explicitly requested by the Copyright Holder.               *
*                                                                              *
* "Original License" means this Artistic License as Distributed with the       *
* Standard Version of the Package, in its current version or as it may be      *
* modified by The Perl Foundation in the future.                               *
*                                                                              *
* "Source" form means the source code, documentation source, and configuration *
* files for the Package.                                                       *
*                                                                              *
* "Compiled" form means the compiled bytecode, object code, binary, or any     *
* other form resulting from mechanical transformation or translation of the    *
* Source form.                                                                 *
* Permission for Use and Modification Without Distribution                     *
*                                                                              *
* (1) You are permitted to use the Standard Version and create and use         *
* Modified Versions for any purpose without restriction, provided that you do  *
* not Distribute the Modified Version.                                         *
* Permissions for Redistribution of the Standard Version                       *
*                                                                              *
* (2) You may Distribute verbatim copies of the Source form of the Standard    *
* Version of this Package in any medium without restriction, either gratis or  *
* for a Distributor Fee, provided that you duplicate all of the original       *
* copyright notices and associated disclaimers. At your discretion, such       *
* verbatim copies may or may not include a Compiled form of the Package.       *
*                                                                              *
* (3) You may apply any bug fixes, portability changes, and other              *
* modifications made available from the Copyright Holder. The resulting        *
* Package will still be considered the Standard Version, and as such will be   *
* subject to the Original License.                                             *
* Distribution of Modified Versions of the Package as Source                   *
*                                                                              *
* (4) You may Distribute your Modified Version as Source (either gratis or for *
* a Distributor Fee, and with or without a Compiled form of the Modified       *
* Version) provided that you clearly document how it differs from the Standard *
* Version, including, but not limited to, documenting any non-standard         *
* features, executables, or modules, and provided that you do at least ONE of  *
* the following:                                                               *
*                                                                              *
* (a) make the Modified Version available to the Copyright Holder of the       *
* Standard Version, under the Original License, so that the Copyright Holder   *
* may include your modifications in the Standard Version.                      *
* (b) ensure that installation of your Modified Version does not prevent the   *
* user installing or running the Standard Version. In addition, the Modified   *
* Version must bear a name that is different from the name of the Standard     *
* Version.                                                                     *
* (c) allow anyone who receives a copy of the Modified Version to make the     *
* Source form of the Modified Version available to others under                *
* (i) the Original License or                                                  *
* (ii) a license that permits the licensee to freely copy, modify and          *
* redistribute the Modified Version using the same licensing terms that apply  *
* to the copy that the licensee received, and requires that the Source form of *
* the Modified Version, and of any works derived from it, be made freely       *
* available in that license fees are prohibited but Distributor Fees are       *
* allowed.                                                                     *
* Distribution of Compiled Forms of the Standard Version or Modified Versions  *
* without the Source                                                           *
*                                                                              *
* (5) You may Distribute Compiled forms of the Standard Version without the    *
* Source, provided that you include complete instructions on how to get the    *
* Source of the Standard Version. Such instructions must be valid at the time  *
* of your distribution. If these instructions, at any time while you are       *
* carrying out such distribution, become invalid, you must provide new         *
* instructions on demand or cease further distribution. If you provide valid   *
* instructions or cease distribution within thirty days after you become aware *
* that the instructions are invalid, then you do not forfeit any of your       *
* rights under this license.                                                   *
*                                                                              *
* (6) You may Distribute a Modified Version in Compiled form without the       *
* Source, provided that you comply with Section 4 with respect to the Source   *
* of the Modified Version.                                                     *
* Aggregating or Linking the Package                                           *
*                                                                              *
* (7) You may aggregate the Package (either the Standard Version or Modified   *
* Version) with other packages and Distribute the resulting aggregation        *
* provided that you do not charge a licensing fee for the Package. Distributor *
* Fees are permitted, and licensing fees for other components in the           *
* aggregation are permitted. The terms of this license apply to the use and    *
* Distribution of the Standard or Modified Versions as included in the         *
* aggregation.                                                                 *
*                                                                              *
* (8) You are permitted to link Modified and Standard Versions with other      *
* works, to embed the Package in a larger work of your own, or to build        *
* stand-alone binary or bytecode versions of applications that include the     *
* Package, and Distribute the result without restriction, provided the result  *
* does not expose a direct interface to the Package.                           *
* Items That are Not Considered Part of a Modified Version                     *
*                                                                              *
* (9) Works (including, but not limited to, modules and scripts) that merely   *
* extend or make use of the Package, do not, by themselves, cause the Package  *
* to be a Modified Version. In addition, such works are not considered parts   *
* of the Package itself, and are not subject to the terms of this license.     *
* General Provisions                                                           *
*                                                                              *
* (10) Any use, modification, and distribution of the Standard or Modified     *
* Versions is governed by this Artistic License. By using, modifying or        *
* distributing the Package, you accept this license. Do not use, modify, or    *
* distribute the Package, if you do not accept this license.                   *
*                                                                              *
* (11) If your Modified Version has been derived from a Modified Version made  *
* by someone other than you, you are nevertheless required to ensure that your *
* Modified Version complies with the requirements of this license.             *
*                                                                              *
* (12) This license does not grant you the right to use any trademark, service *
* mark, tradename, or logo of the Copyright Holder.                            *
*                                                                              *
* (13) This license includes the non-exclusive, worldwide, free-of-charge      *
* patent license to make, have made, use, offer to sell, sell, import and      *
* otherwise transfer the Package with respect to any patent claims licensable  *
* by the Copyright Holder that are necessarily infringed by the Package. If    *
* you institute patent litigation (including a cross-claim or counterclaim)    *
* against any party alleging that the Package constitutes direct or            *
* contributory patent infringement, then this Artistic License to you shall    *
* terminate on the date that such litigation is filed.                         *
*                                                                              *
* (14) Disclaimer of Warranty: THE PACKAGE IS PROVIDED BY THE COPYRIGHT HOLDER *
* AND CONTRIBUTORS "AS IS' AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES. THE  *
* IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR  *
* NON-INFRINGEMENT ARE DISCLAIMED TO THE EXTENT PERMITTED BY YOUR LOCAL LAW.   *
* UNLESS REQUIRED BY LAW, NO COPYRIGHT HOLDER OR CONTRIBUTOR WILL BE LIABLE    *
* FOR ANY DIRECT, INDIRECT, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING IN    *
* ANY WAY OUT OF THE USE OF THE PACKAGE, EVEN IF ADVISED OF THE POSSIBILITY OF *
* SUCH DAMAGE.                                                                 *
*                                                                              *
*         The End                                                              *
*                                                                              *
\******************************************************************************/

/* modified by Fabrizio Riguzzi in 2009 for dealing with multivalued variables
instead of variables or their negation, the script can contain equations of the
form
variable=value
*/

#include "YapInterface.h"
#include "cudd_config.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#if HAVE_UTIL_H
#include <util.h>
#endif
#if HAVE_CUDD_UTIL_H
#include <cudd/util.h>
#endif
#if HAVE_CUDD_H
#include "cudd.h"
#endif
#if HAVE_CUDD_CUDD_H
#include "cudd/cudd.h"
#endif
#if HAVE_CUDDINT_H
#include "cuddInt.h"
#endif
#if HAVE_CUDD_CUDDINT_H
#include "cudd/cuddInt.h"
#endif
#include "general.h"

#define IsHigh(manager, node) HIGH(manager) == node
#define IsLow(manager, node) LOW(manager) == node
#define HIGH(manager) Cudd_ReadOne(manager)
#define LOW(manager) Cudd_Not(Cudd_ReadOne(manager))
#define NOT(node) Cudd_Not(node)
#define GetIndex(node) Cudd_NodeReadIndex(node)
#define GetMVar(manager, index, value, varmap)                                 \
  equality(manager, index, value, varmap) // Cudd_bddIthVar(manager, index)
#define GetVar(manager, index) Cudd_bddIthVar(manager, index)
#define NewVar(manager) Cudd_bddNewVar(manager)
#define KillBDD(manager) Cudd_Quit(manager)
#define GetVarCount(manager) Cudd_ReadSize(manager)
#define DEBUGON _debug = 1
#define DEBUGOFF _debug = 0
#define RAPIDLOADON _RapidLoad = 1
#define RAPIDLOADOFF _RapidLoad = 0
#define SETMAXBUFSIZE(size) _maxbufsize = size
#define BDDFILE_ERROR -1
#define BDDFILE_OTHER 0
#define BDDFILE_SCRIPT 1
#define BDDFILE_NODEDUMP 2

extern int _RapidLoad;
extern int _debug;
extern int _maxbufsize;

typedef struct _bddfileheader {
  FILE *inputfile;
  int version;
  int varcnt;
  int bvarcnt;
  int varstart;
  int intercnt;
  int filetype;
} bddfileheader;

typedef struct {
  int nVal, nBit, init;
  double *probabilities;
  DdNode **booleanVars;
} variable;

typedef struct _namedvars {
  int varcnt;
  int varstart;
  char **vars;
  int *loaded;
  double *dvalue;
  int *ivalue;
  void **dynvalue;
  variable *mvars;
  int *bVar2mVar;
} namedvars;

typedef struct _hisnode {
  DdNode *key;
  double dvalue;
  int ivalue;
  void *dynvalue;
} hisnode;

typedef struct _hisqueue {
  int cnt;
  hisnode *thenode;
} hisqueue;

typedef struct _nodeline {
  char *varname;
  char *truevar;
  char *falsevar;
  int nodenum;
  int truenode;
  int falsenode;
} nodeline;

/* Initialization */

DdManager *simpleBDDinit(int varcnt);

/* BDD Generation */

DdNode *D_BDDAnd(DdManager *manager, DdNode *bdd1, DdNode *bdd2);
DdNode *D_BDDNand(DdManager *manager, DdNode *bdd1, DdNode *bdd2);
DdNode *D_BDDOr(DdManager *manager, DdNode *bdd1, DdNode *bdd2);
DdNode *D_BDDNor(DdManager *manager, DdNode *bdd1, DdNode *bdd2);
DdNode *D_BDDXor(DdManager *manager, DdNode *bdd1, DdNode *bdd2);
DdNode *D_BDDXnor(DdManager *manager, DdNode *bdd1, DdNode *bdd2);

DdNode *FileGenerateBDD(DdManager *manager, namedvars varmap,
                        bddfileheader fileheader);
DdNode *OnlineGenerateBDD(DdManager *manager, namedvars *varmap);
DdNode *LineParser(DdManager *manager, namedvars varmap, DdNode **inter,
                   int maxinter, char *function, int iline);
DdNode *OnlineLineParser(DdManager *manager, namedvars *varmap, DdNode **inter,
                         int maxinter, char *function, int iline);
DdNode *BDD_Operator(DdManager *manager, DdNode *bdd1, DdNode *bdd2,
                     char Operator, int inegoper);
int getInterBDD(char *function);
char *getFileName(const char *function);
int GetParam(char *inputline, int iParam);
int LoadVariableData(namedvars varmap, char *filename);
int LoadMultiVariableData(DdManager *mgr, namedvars varmap, char *filename);

/* Named variables */

namedvars InitNamedVars(int varcnt, int varstart);
namedvars InitNamedMultiVars(int varcnt, int varstart, int bvarcnt);
void EnlargeNamedVars(namedvars *varmap, int newvarcnt);
int AddNamedVarAt(namedvars varmap, const char *varname, int index);
int AddNamedVar(namedvars varmap, const char *varname);
int AddNamedMultiVar(DdManager *mgr, namedvars varmap, const char *varnamei,
                     int *value);
void SetNamedVarValuesAt(namedvars varmap, int index, double dvalue, int ivalue,
                         void *dynvalue);
int SetNamedVarValues(namedvars varmap, const char *varname, double dvalue,
                      int ivalue, void *dynvalue);
int GetNamedVarIndex(const namedvars varmap, const char *varname);
int RepairVarcnt(namedvars *varmap);
char *GetNodeVarName(DdManager *manager, namedvars varmap, DdNode *node);
char *GetNodeVarNameDisp(DdManager *manager, namedvars varmap, DdNode *node);
int all_loaded(namedvars varmap, int disp);

/* Traversal */

DdNode *HighNodeOf(DdManager *manager, DdNode *node);
DdNode *LowNodeOf(DdManager *manager, DdNode *node);

/* Traversal - History */

hisqueue *InitHistory(int varcnt);
void ReInitHistory(hisqueue *HisQueue, int varcnt);
void AddNode(hisqueue *HisQueue, int varstart, DdNode *node, double dvalue,
             int ivalue, void *dynvalue);
hisnode *GetNode(hisqueue *HisQueue, int varstart, DdNode *node);
int GetNodeIndex(hisqueue *HisQueue, int varstart, DdNode *node);
void onlinetraverse(DdManager *manager, namedvars varmap, hisqueue *HisQueue,
                    DdNode *bdd);

/* Save-load */

bddfileheader ReadFileHeader(const char *filename);
int CheckFileVersion(const char *version);

DdNode *LoadNodeDump(DdManager *manager, namedvars varmap, FILE *inputfile);
DdNode *LoadNodeRec(DdManager *manager, namedvars varmap, hisqueue *Nodes,
                    FILE *inputfile, nodeline current);
DdNode *GetIfExists(DdManager *manager, namedvars varmap, hisqueue *Nodes,
                    char *varname, int nodenum);

int SaveNodeDump(DdManager *manager, namedvars varmap, DdNode *bdd,
                 const char *filename);
void SaveExpand(DdManager *manager, namedvars varmap, hisqueue *Nodes,
                DdNode *Current, FILE *outputfile);
void ExpandNodes(hisqueue *Nodes, int index, int nodenum);

/* Export */

int simpleBDDtoDot(DdManager *manager, DdNode *bdd, const char *filename);
int simpleNamedBDDtoDot(DdManager *manager, namedvars varmap, DdNode *bdd,
                        const char *filename);

DdNode *equality(DdManager *mgr, int varIndex, int value, namedvars varmap);
hisnode *GetNodei1(int *bVar2mVar, hisqueue *HisQueue, int varstart,
                   DdNode *node);
void AddNode1(int *bVar2mVar, hisqueue *HisQueue, int varstart, DdNode *node,
              double dvalue, int ivalue, void *dynvalue);
hisnode *GetNode1(int *bVar2mVar, hisqueue *HisQueue, int varstart,
                  DdNode *node);
