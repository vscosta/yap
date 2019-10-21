/******************************************************************************\
*                                                                              *
*    SimpleCUDD library (www.cs.kuleuven.be/~theo/tools/simplecudd.html)       *
*  SimpleCUDD was developed at Katholieke Universiteit Leuven(www.kuleuven.be) *
*                                                                              *
*  Copyright Katholieke Universiteit Leuven 2008                               *
*                                                                              *
*  Author: Theofrastos Mantadelis                                              *
*  File: simplecudd.c                                                          *
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
#include "simplecudd.h"

/* BDD manager initialization */

int _debug = 0;
int _RapidLoad = 0;
int _maxbufsize = 0;
int boolVars = 0;

DdManager *simpleBDDinit(int varcnt) {
  DdManager *temp;
  temp = Cudd_Init(varcnt, 0, CUDD_UNIQUE_SLOTS, CUDD_CACHE_SLOTS, 0);
  Cudd_AutodynEnable(temp, CUDD_REORDER_GROUP_SIFT);
  Cudd_SetMaxCacheHard(temp, 1024 * 1024 * 1024);
  Cudd_SetLooseUpTo(temp, 1024 * 1024 * 512);
  if (_debug)
    Cudd_EnableReorderingReporting(temp);
  return temp;
}

/* BDD tree travesrsing */

DdNode *HighNodeOf(DdManager *manager, DdNode *node) {
  DdNode *tmp;
  if (IsHigh(manager, node))
    return HIGH(manager);
  if (IsLow(manager, node))
    return LOW(manager);
  tmp = Cudd_Regular(node);
  if (Cudd_IsComplement(node))
    return NOT(tmp->type.kids.T);
  return tmp->type.kids.T;
}

DdNode *LowNodeOf(DdManager *manager, DdNode *node) {
  DdNode *tmp;
  if (IsHigh(manager, node))
    return HIGH(manager);
  if (IsLow(manager, node))
    return LOW(manager);
  tmp = Cudd_Regular(node);
  if (Cudd_IsComplement(node))
    return NOT(tmp->type.kids.E);
  return tmp->type.kids.E;
}

/* BDD tree generation */

DdNode *D_BDDAnd(DdManager *manager, DdNode *bdd1, DdNode *bdd2) {
  DdNode *tmp;
  tmp = Cudd_bddAnd(manager, bdd1, bdd2);
  Cudd_Ref(tmp);
  Cudd_RecursiveDeref(manager, bdd2);
  return tmp;
}

DdNode *D_BDDNand(DdManager *manager, DdNode *bdd1, DdNode *bdd2) {
  DdNode *tmp;
  tmp = Cudd_bddNand(manager, bdd1, bdd2);
  Cudd_Ref(tmp);
  Cudd_RecursiveDeref(manager, bdd2);
  return tmp;
}

DdNode *D_BDDOr(DdManager *manager, DdNode *bdd1, DdNode *bdd2) {
  DdNode *tmp;
  tmp = Cudd_bddOr(manager, bdd1, bdd2);
  Cudd_Ref(tmp);
  Cudd_RecursiveDeref(manager, bdd2);
  return tmp;
}

DdNode *D_BDDNor(DdManager *manager, DdNode *bdd1, DdNode *bdd2) {
  DdNode *tmp;
  tmp = Cudd_bddNor(manager, bdd1, bdd2);
  Cudd_Ref(tmp);
  Cudd_RecursiveDeref(manager, bdd2);
  return tmp;
}

DdNode *D_BDDXor(DdManager *manager, DdNode *bdd1, DdNode *bdd2) {
  DdNode *tmp;
  tmp = Cudd_bddXor(manager, bdd1, bdd2);
  Cudd_Ref(tmp);
  Cudd_RecursiveDeref(manager, bdd2);
  return tmp;
}

DdNode *D_BDDXnor(DdManager *manager, DdNode *bdd1, DdNode *bdd2) {
  DdNode *tmp;
  tmp = Cudd_bddXnor(manager, bdd1, bdd2);
  Cudd_Ref(tmp);
  Cudd_RecursiveDeref(manager, bdd2);
  return tmp;
}

/* file manipulation */

bddfileheader ReadFileHeader(const char *filename) {
  bddfileheader temp;
  char *header;
  temp.inputfile = NULL;
  temp.version = 0;
  temp.varcnt = 0;
  temp.varstart = 0;
  temp.intercnt = 0;
  temp.filetype = BDDFILE_OTHER;
  if ((temp.inputfile = fopen(filename, "r")) == NULL) {
    perror(filename);
    temp.filetype = BDDFILE_ERROR;
    return temp;
  }
  // Read file header
  if (!feof(temp.inputfile)) {
    header = freadline(temp.inputfile);
    temp.version = CheckFileVersion(header);
    if (temp.version > -1)
      temp.filetype = (strlen(header) == 5) * BDDFILE_SCRIPT +
                      (strlen(header) == 7) * BDDFILE_NODEDUMP;
    free(header);
    switch (temp.filetype) {
    case BDDFILE_SCRIPT:
      switch (temp.version) {
      case 1:
        fscanf(temp.inputfile, "%i\n", &temp.varcnt);
        fscanf(temp.inputfile, "%i\n", &temp.bvarcnt);
        fscanf(temp.inputfile, "%i\n", &temp.varstart);
        fscanf(temp.inputfile, "%i\n", &temp.intercnt);
        break;
      default:
        fclose(temp.inputfile);
        temp.inputfile = NULL;
        break;
      }
      break;
    case BDDFILE_NODEDUMP:
      switch (temp.version) {
      case 1:
        fscanf(temp.inputfile, "%i\n", &temp.varcnt);
        fscanf(temp.inputfile, "%i\n", &temp.varstart);
        break;
      default:
        fclose(temp.inputfile);
        temp.inputfile = NULL;
        break;
      }
      break;
    case BDDFILE_OTHER:
      fclose(temp.inputfile);
      temp.inputfile = NULL;
      break;
    default:
      fclose(temp.inputfile);
      temp.inputfile = NULL;
      break;
    }
  }
  return temp;
}

int CheckFileVersion(const char *version) {
  if (strlen(version) < 5)
    return -1;
  if (strlen(version) == 5 && version[0] == '@' && version[1] == 'B' &&
      version[2] == 'D' && version[3] == 'D')
    return atoi(version + 4);
  if (strlen(version) == 7 && version[0] == '@' && version[1] == 'N' &&
      version[2] == 'O' && version[3] == 'D' && version[4] == 'E' &&
      version[5] == 'S')
    return atoi(version + 6);
  return -1;
}

int simpleBDDtoDot(DdManager *manager, DdNode *bdd, const char *filename) {
  DdNode *f[1];
  int ret;
  FILE *fd;
  f[0] = Cudd_BddToAdd(manager, bdd);
  fd = fopen(filename, "w");
  if (fd == NULL) {
    perror(filename);
    return -1;
  }
  ret = Cudd_DumpDot(manager, 1, f, NULL, NULL, fd);
  fclose(fd);
  return ret;
}

int simpleNamedBDDtoDot(DdManager *manager, namedvars varmap, DdNode *bdd,
                        const char *filename) {
  DdNode *f[1];
  int ret;
  FILE *fd;
  f[0] = Cudd_BddToAdd(manager, bdd);
  fd = fopen(filename, "w");
  if (fd == NULL) {
    perror(filename);
    return -1;
  }
  char **vs = varmap.vars;
  ret = Cudd_DumpDot(manager, 1, f, vs, NULL, fd);
  fclose(fd);
  return ret;
}

int SaveNodeDump(DdManager *manager, namedvars varmap, DdNode *bdd,
                 const char *filename) {
  hisqueue *Nodes;
  FILE *outputfile;
  int i;
  if ((outputfile = fopen(filename, "w")) == NULL) {
    perror(filename);
    return -1;
  }
  fprintf(outputfile, "%s\n%i\n%i\n", "@NODES1", varmap.varcnt,
          varmap.varstart);
  Nodes = InitHistory(varmap.varcnt);
  for (i = 0; i < varmap.varcnt; i++)
    fprintf(outputfile, "%s\t%i\n", varmap.vars[i], Cudd_ReadPerm(manager, i));
  if (bdd == HIGH(manager))
    fprintf(outputfile, "TRUE\t0\tTRUE\t0\tTRUE\t0\n");
  else if (bdd == LOW(manager))
    fprintf(outputfile, "FALSE\t0\tFALSE\t0\tFALSE\t0\n");
  else
    SaveExpand(manager, varmap, Nodes, bdd, outputfile);
  ReInitHistory(Nodes, varmap.varcnt);
  free(Nodes);
  fclose(outputfile);
  return 0;
}

void SaveExpand(DdManager *manager, namedvars varmap, hisqueue *Nodes,
                DdNode *Current, FILE *outputfile) {
  DdNode *h, *l;
  hisnode *Found;
  char *curnode;
  int inode;
  if (Current != HIGH(manager) && Current != LOW(manager)) {
    if ((Found = GetNode(Nodes, varmap.varstart, Current)) == NULL) {
      AddNode(Nodes, varmap.varstart, Current, 0.0, 0, NULL);
      Found = GetNode(Nodes, varmap.varstart, Current);
    }
    if (!(Found->ivalue)) {
      Found->ivalue = 1;
      curnode = GetNodeVarNameDisp(manager, varmap, Current);
      inode = GetNodeIndex(Nodes, varmap.varstart, Current);
      fprintf(outputfile, "%s\t%i\t", curnode, inode);
      h = HighNodeOf(manager, Current);
      if (h == HIGH(manager)) {
        fprintf(outputfile, "TRUE\t0\t");
      } else if (h == LOW(manager)) {
        fprintf(outputfile, "FALSE\t0\t");
      } else {
        if (GetNode(Nodes, varmap.varstart, h) == NULL)
          AddNode(Nodes, varmap.varstart, h, 0.0, 0, NULL);
        curnode = GetNodeVarNameDisp(manager, varmap, h);
        inode = GetNodeIndex(Nodes, varmap.varstart, h);
        fprintf(outputfile, "%s\t%i\t", curnode, inode);
      }
      l = LowNodeOf(manager, Current);
      if (l == HIGH(manager)) {
        fprintf(outputfile, "TRUE\t0\n");
      } else if (l == LOW(manager)) {
        fprintf(outputfile, "FALSE\t0\n");
      } else {
        if (GetNode(Nodes, varmap.varstart, l) == NULL)
          AddNode(Nodes, varmap.varstart, l, 0.0, 0, NULL);
        curnode = GetNodeVarNameDisp(manager, varmap, l);
        inode = GetNodeIndex(Nodes, varmap.varstart, l);
        fprintf(outputfile, "%s\t%i\n", curnode, inode);
      }
      SaveExpand(manager, varmap, Nodes, l, outputfile);
      SaveExpand(manager, varmap, Nodes, h, outputfile);
    }
  }
}

DdNode *LoadNodeDump(DdManager *manager, namedvars varmap, FILE *inputfile) {
  hisqueue *Nodes;
  nodeline temp;
  DdNode *ret;
  int i, pos, *perm;
  char *varnam;
  perm = (int *)malloc(sizeof(int) * varmap.varcnt);
  Nodes = InitHistory(varmap.varcnt);
  for (i = 0; i < varmap.varcnt; i++) {
    varnam = freadstr(inputfile, "\t");
    pos = atoi(freadstr(inputfile, "\n"));
    AddNamedVarAt(varmap, varnam, pos);
    perm[pos] = pos;
  }
  temp.varname = freadstr(inputfile, "\t");
  fscanf(inputfile, "%i\t", &temp.nodenum);
  temp.truevar = freadstr(inputfile, "\t");
  fscanf(inputfile, "%i\t", &temp.truenode);
  temp.falsevar = freadstr(inputfile, "\t");
  fscanf(inputfile, "%i\n", &temp.falsenode);
  ret = LoadNodeRec(manager, varmap, Nodes, inputfile, temp);
  free(temp.varname);
  free(temp.truevar);
  free(temp.falsevar);
  fclose(inputfile);
  ReInitHistory(Nodes, varmap.varcnt);
  free(Nodes);
  Cudd_Ref(ret);
  Cudd_ShuffleHeap(manager, perm);
  for (i = 0; i < varmap.varcnt; i++)
    varmap.ivalue[i] = 0;
  return ret;
}

DdNode *LoadNodeRec(DdManager *manager, namedvars varmap, hisqueue *Nodes,
                    FILE *inputfile, nodeline current) {
  nodeline temp;
  DdNode *newnode, *truenode, *falsenode;
  int index;
  newnode =
      GetIfExists(manager, varmap, Nodes, current.varname, current.nodenum);
  if (newnode != NULL)
    return newnode;
  falsenode =
      GetIfExists(manager, varmap, Nodes, current.falsevar, current.falsenode);
  if (falsenode == NULL) {
    temp.varname = freadstr(inputfile, "\t");
    fscanf(inputfile, "%i\t", &temp.nodenum);
    temp.truevar = freadstr(inputfile, "\t");
    fscanf(inputfile, "%i\t", &temp.truenode);
    temp.falsevar = freadstr(inputfile, "\t");
    fscanf(inputfile, "%i\n", &temp.falsenode);
    falsenode = LoadNodeRec(manager, varmap, Nodes, inputfile, temp);
    free(temp.varname);
    free(temp.truevar);
    free(temp.falsevar);
  }
  truenode =
      GetIfExists(manager, varmap, Nodes, current.truevar, current.truenode);
  if (truenode == NULL) {
    temp.varname = freadstr(inputfile, "\t");
    fscanf(inputfile, "%i\t", &temp.nodenum);
    temp.truevar = freadstr(inputfile, "\t");
    fscanf(inputfile, "%i\t", &temp.truenode);
    temp.falsevar = freadstr(inputfile, "\t");
    fscanf(inputfile, "%i\n", &temp.falsenode);
    truenode = LoadNodeRec(manager, varmap, Nodes, inputfile, temp);
    free(temp.varname);
    free(temp.truevar);
    free(temp.falsevar);
  }
  index = GetNamedVarIndex(varmap, current.varname);
  if (!varmap.ivalue[index]) {
    varmap.ivalue[index] = 1;
    newnode = GetVar(manager, varmap.varstart + index);
    // Cudd_RecursiveDeref(manager, newnode->type.kids.T);
    // Cudd_RecursiveDeref(manager, newnode->type.kids.E);
    newnode->type.kids.T = Cudd_NotCond(truenode, Cudd_IsComplement(truenode));
    newnode->type.kids.E = Cudd_NotCond(falsenode, Cudd_IsComplement(truenode));
    Cudd_Ref(newnode->type.kids.T);
    Cudd_Ref(newnode->type.kids.E);
    Cudd_Ref(newnode);
  } else {
    if (_RapidLoad == 1) {
      newnode = cuddAllocNode(manager);
      if (newnode != NULL) {
        newnode->index = varmap.varstart + index;
        newnode->type.kids.T =
            Cudd_NotCond(truenode, Cudd_IsComplement(truenode));
        newnode->type.kids.E =
            Cudd_NotCond(falsenode, Cudd_IsComplement(truenode));
        Cudd_Ref(newnode->type.kids.T);
        Cudd_Ref(newnode->type.kids.E);
        Cudd_Ref(newnode);
      }
    } else {
      newnode =
          cuddUniqueInter(manager, varmap.varstart + index,
                          Cudd_NotCond(truenode, Cudd_IsComplement(truenode)),
                          Cudd_NotCond(falsenode, Cudd_IsComplement(truenode)));
      if (newnode != NULL) {
        Cudd_Ref(newnode);
      } else {
        newnode = cuddAllocNode(manager);
        if (newnode != NULL) {
          newnode->index = varmap.varstart + index;
          newnode->type.kids.T =
              Cudd_NotCond(truenode, Cudd_IsComplement(truenode));
          newnode->type.kids.E =
              Cudd_NotCond(falsenode, Cudd_IsComplement(truenode));
          Cudd_Ref(newnode->type.kids.T);
          Cudd_Ref(newnode->type.kids.E);
          Cudd_Ref(newnode);
        }
      }
    }
  }
  if (newnode != NULL) {
    Nodes[index].thenode[current.nodenum].key =
        Cudd_NotCond(newnode, Cudd_IsComplement(truenode));
    return Cudd_NotCond(newnode, Cudd_IsComplement(truenode));
  }
  return NULL;
}

DdNode *GetIfExists(DdManager *manager, namedvars varmap, hisqueue *Nodes,
                    char *varname, int nodenum) {
  int index;
  if (strcmp(varname, "TRUE") == 0)
    return HIGH(manager);
  if (strcmp(varname, "FALSE") == 0)
    return LOW(manager);
  index = GetNamedVarIndex(varmap, varname);
  if (index == -1 * varmap.varcnt) {
    fprintf(stderr, "Error: more variables requested than initialized.\n");
    exit(-1);
  }
  if ((index < 0) || (index == 0 && varmap.vars[0] == NULL)) {
    index = AddNamedVar(varmap, varname);
  }
  ExpandNodes(Nodes, index, nodenum);
  if (Nodes[index].thenode[nodenum].key != NULL)
    return Nodes[index].thenode[nodenum].key;
  return NULL;
}

void ExpandNodes(hisqueue *Nodes, int index, int nodenum) {
  int i;
  if (Nodes[index].cnt > nodenum)
    return;
  Nodes[index].thenode =
      (hisnode *)realloc(Nodes[index].thenode, (nodenum + 1) * sizeof(hisnode));
  for (i = Nodes[index].cnt; i < nodenum + 1; i++) {
    Nodes[index].thenode[i].key = NULL;
    Nodes[index].thenode[i].ivalue = 0;
    Nodes[index].thenode[i].dvalue = 0.0;
    Nodes[index].thenode[i].dynvalue = NULL;
  }
  Nodes[index].cnt = nodenum + 1;
};

int LoadVariableData(namedvars varmap, char *filename) {
  FILE *data;
  char *dataread, *varname; //, *dynvalue;
  // double dvalue = 0.0;
  int values, maxbufsize = 10, index = -1;
  //dynvalue = NULL;
  if ((data = fopen(filename, "r")) == NULL) {
    perror("fopen");
    return -1;
  }
  printf("filename %s\n", filename);
  dataread = (char *)malloc(sizeof(char) * maxbufsize);
  while (!feof(data)) {
    index++;
    printf("index %d\n", index);
    fscanf(data, "@%s\n", dataread);

    varname = (char *)malloc(sizeof(char) * strlen(dataread));
    strcpy(varname, dataread);
    varmap.vars[index] = varname;
    fscanf(data, "%di\n", &values);
    varmap.mvars[index].nVal = values;
    varmap.mvars[index].nBit = (int)ceil(log(values) / log(2));
    varmap.mvars[index].probabilities =
        (double *)malloc(sizeof(double) * values);
    varmap.mvars[index].booleanVars =
        (int *)malloc(sizeof(int) * varmap.mvars[index].nBit);

    free(varname);
  }
  fclose(data);
  free(dataread);
  return 0;
}

int LoadMultiVariableData(DdManager *mgr, namedvars varmap, char *filename) {
  FILE *data;
  char *dataread, *varname; //, *dynvalue;
  int values, maxbufsize = 10, index = -1, i;
  //dynvalue = NULL;
  if ((data = fopen(filename, "r")) == NULL) {
    perror("fopen");
    return -1;
  }
  dataread = (char *)malloc(sizeof(char) * maxbufsize);
  while (!feof(data)) {
    index++;
    if (fscanf(data, "@%s\n", dataread) < 1)
      break;
    varname = (char *)malloc(sizeof(char) * strlen(dataread));
    strcpy(varname, dataread);
    varmap.vars[index] = varname;

    fscanf(data, "%d\n", &values);
    varmap.loaded[index] = 1;
    varmap.mvars[index].init = 0;
    varmap.mvars[index].nVal = values;
    varmap.mvars[index].nBit = (int)ceil(log(values) / log(2));
    varmap.mvars[index].probabilities =
        (double *)malloc(sizeof(double) * values);
    varmap.mvars[index].booleanVars =
        (DdNode **)malloc(sizeof(DdNode *) * varmap.mvars[index].nBit);
    for (i = 0; i < values - 1; i++) {
      fscanf(data, "%lf", &varmap.mvars[index].probabilities[i]);
    }
    fscanf(data, "%lf\n", &varmap.mvars[index].probabilities[values - 1]);
  }

  fclose(data);
  free(dataread);
  return 0;
}

/* Queue for node storing to avoid loops */

hisqueue *InitHistory(int varcnt) {
  int i;
  hisqueue *HisQueue;
  HisQueue = (hisqueue *)malloc(sizeof(hisqueue) * varcnt);
  for (i = 0; i < varcnt; i++) {
    HisQueue[i].thenode = NULL;
    HisQueue[i].cnt = 0;
  }
  return HisQueue;
}

void ReInitHistory(hisqueue *HisQueue, int varcnt) {
  int i, j;
  for (i = 0; i < varcnt; i++) {
    if (HisQueue[i].thenode != NULL) {
      for (j = 0; j < HisQueue[i].cnt; j++)
        if (HisQueue[i].thenode[j].dynvalue != NULL)
          free(HisQueue[i].thenode[j].dynvalue);
      free(HisQueue[i].thenode);
      HisQueue[i].thenode = NULL;
    }
    HisQueue[i].cnt = 0;
  }
}

void AddNode1(int *bVar2mVar, hisqueue *HisQueue, int varstart, DdNode *node,
              double dvalue, int ivalue, void *dynvalue) {
  int index, mVarIndex;
  index = Cudd_NodeReadIndex(node);
  mVarIndex = bVar2mVar[index];

  HisQueue[mVarIndex].thenode =
      (hisnode *)realloc(HisQueue[mVarIndex].thenode,
                         (HisQueue[mVarIndex].cnt + 1) * sizeof(hisnode));
  HisQueue[mVarIndex].thenode[HisQueue[mVarIndex].cnt].key = node;
  HisQueue[mVarIndex].thenode[HisQueue[mVarIndex].cnt].dvalue = dvalue;
  HisQueue[mVarIndex].thenode[HisQueue[mVarIndex].cnt].ivalue = ivalue;
  HisQueue[mVarIndex].thenode[HisQueue[mVarIndex].cnt].dynvalue = dynvalue;
  HisQueue[mVarIndex].cnt += 1;
}
void AddNode(hisqueue *HisQueue, int varstart, DdNode *node, double dvalue,
             int ivalue, void *dynvalue) {
  int index = GetIndex(node) - varstart;
  HisQueue[index].thenode = (hisnode *)realloc(
      HisQueue[index].thenode, (HisQueue[index].cnt + 1) * sizeof(hisnode));
  HisQueue[index].thenode[HisQueue[index].cnt].key = node;
  HisQueue[index].thenode[HisQueue[index].cnt].dvalue = dvalue;
  HisQueue[index].thenode[HisQueue[index].cnt].ivalue = ivalue;
  HisQueue[index].thenode[HisQueue[index].cnt].dynvalue = dynvalue;
  HisQueue[index].cnt += 1;
}

hisnode *GetNode1(int *bVar2mVar, hisqueue *HisQueue, int varstart,
                  DdNode *node) {
  int i;
  int index, mVarIndex;
  index = Cudd_NodeReadIndex(node);
  mVarIndex = bVar2mVar[index];
  for (i = 0; i < HisQueue[mVarIndex].cnt; i++) {
    if (HisQueue[mVarIndex].thenode[i].key == node)
      return &(HisQueue[mVarIndex].thenode[i]);
  }
  return NULL;
}

hisnode *GetNode(hisqueue *HisQueue, int varstart, DdNode *node) {
  int i;
  int mVarIndex = 0; // ??? VSC: FIXME
  // int index = Cudd_NodeReadIndex(node);
  for (i = 0; i < HisQueue[mVarIndex].cnt; i++) {
    if (HisQueue[mVarIndex].thenode[i].key == node)
      return &(HisQueue[mVarIndex].thenode[i]);
  }
  return NULL;
}

int GetNodeIndex(hisqueue *HisQueue, int varstart, DdNode *node) {
  int i;
  int index = GetIndex(node) - varstart;
  for (i = 0; i < HisQueue[index].cnt; i++) {
    if (HisQueue[index].thenode[i].key == node)
      return i;
  }
  return -1;
}

/* Named variables */

namedvars InitNamedVars(int varcnt, int varstart) {
  namedvars temp;
  int i;

  temp.varcnt = varcnt;
  temp.varstart = varstart;
  temp.vars = (char **)malloc(sizeof(char *) * varcnt);
  temp.loaded = (int *)malloc(sizeof(int) * varcnt);
  temp.dvalue = (double *)malloc(sizeof(double) * varcnt);
  temp.ivalue = (int *)malloc(sizeof(int) * varcnt);
  temp.dynvalue = (void **)malloc(sizeof(int) * varcnt);
  temp.mvars = (variable *)malloc(sizeof(variable) * varcnt);
  for (i = 0; i < varcnt; i++) {
    temp.vars[i] = NULL;
    temp.loaded[i] = 0;
    temp.dvalue[i] = 0.0;
    temp.ivalue[i] = 0;
    temp.dynvalue[i] = NULL;
  }
  return temp;
}

namedvars InitNamedMultiVars(int varcnt, int varstart, int bvarcnt) {
  namedvars temp;
  int i;
  temp.varcnt = varcnt;
  temp.varstart = varstart;
  temp.vars = (char **)malloc(sizeof(char *) * varcnt);
  temp.loaded = (int *)malloc(sizeof(int) * varcnt);
  temp.dvalue = (double *)malloc(sizeof(double) * varcnt);
  temp.ivalue = (int *)malloc(sizeof(int) * varcnt);
  temp.dynvalue = (void **)malloc(sizeof(int) * varcnt);
  temp.mvars = (variable *)malloc(sizeof(variable) * varcnt);
  temp.bVar2mVar = (int *)malloc(sizeof(int) * bvarcnt);
  for (i = 0; i < varcnt; i++) {
    temp.vars[i] = NULL;
    temp.loaded[i] = 0;
    temp.dvalue[i] = 0.0;
    temp.ivalue[i] = 0;
    temp.dynvalue[i] = NULL;
  }
  return temp;
}

void EnlargeNamedVars(namedvars *varmap, int newvarcnt) {
  int i;
  varmap->vars = (char **)realloc(varmap->vars, sizeof(char *) * newvarcnt);
  varmap->loaded = (int *)realloc(varmap->loaded, sizeof(int) * newvarcnt);
  varmap->dvalue =
      (double *)realloc(varmap->dvalue, sizeof(double) * newvarcnt);
  varmap->ivalue = (int *)realloc(varmap->ivalue, sizeof(int) * newvarcnt);
  varmap->dynvalue =
      (void **)realloc(varmap->dynvalue, sizeof(int) * newvarcnt);
  for (i = varmap->varcnt; i < newvarcnt; i++) {
    varmap->vars[i] = NULL;
    varmap->loaded[i] = 0;
    varmap->dvalue[i] = 0.0;
    varmap->ivalue[i] = 0;
    varmap->dynvalue[i] = NULL;
  }
  varmap->varcnt = newvarcnt;
}

int AddNamedVarAt(namedvars varmap, const char *varname, int index) {
  if (varmap.varcnt > index) {
    varmap.vars[index] = (char *)malloc(sizeof(char) * (strlen(varname) + 1));
    strcpy(varmap.vars[index], varname);
    return index;
  }
  return -1;
}

int AddNamedMultiVar(DdManager *mgr, namedvars varmap, const char *varname,
                     int *value) {

  int index, i, l, nBit;
  int *booleanVars;
  char *vname;
  l = strlen(varname);
  i = 0;
  while (varname[i] != '-') {
    i++;
  }
  vname = (char *)malloc(sizeof(char) * (l + 1));
  strncpy(vname, varname, i);
  vname[i] = '\0';
  sscanf(varname + i + 1, "%d", value);
  index = GetNamedVarIndex(varmap, vname);
  if (index == -1 * varmap.varcnt) {
    return -1;
  } else if ((index < 0) || (index == 0 && varmap.vars[0] == NULL)) {
    index *= -1;
    varmap.vars[index] = (char *)malloc(sizeof(char) * (strlen(varname) + 1));
  }
  if (varmap.mvars[index].init == 0) {
    nBit = varmap.mvars[index].nBit;
    booleanVars = varmap.mvars[index].booleanVars;
    for (i = 0; i < nBit; i++) {
      booleanVars[i] = boolVars;
      varmap.bVar2mVar[boolVars] = index;
      boolVars = boolVars + 1;
    }
    varmap.mvars[index].init = 1;
  }
  return index;
}

int AddNamedVar(namedvars varmap, const char *varname) {

  int index, i, l;
  char *vname;
  l = strlen(varname);
  i = 0;
  while (varname[i] != '-') {
    i++;
  }
  vname = (char *)malloc(sizeof(char) * l);
  strncpy(vname, varname, i);
  vname[i] = '\0';
  index = GetNamedVarIndex(varmap, varname);
  if (index == -1 * varmap.varcnt) {
    return -1;
  } else if ((index < 0) || (index == 0 && varmap.vars[0] == NULL)) {
    index *= -1;
    varmap.vars[index] = (char *)malloc(sizeof(char) * (strlen(varname) + 1));
  }
  return index;
}

void SetNamedVarValuesAt(namedvars varmap, int index, double dvalue, int ivalue,
                         void *dynvalue) {
  varmap.dvalue[index] = dvalue;
  varmap.ivalue[index] = ivalue;
  varmap.dynvalue[index] = dynvalue;
}

int SetNamedVarValues(namedvars varmap, const char *varname, double dvalue,
                      int ivalue, void *dynvalue) {
  int index = GetNamedVarIndex(varmap, varname);
  if (index == -1 * varmap.varcnt) {
    return -1;
  } else if ((index < 0) || (index == 0 && varmap.vars[0] == NULL)) {
    index *= -1;
    varmap.vars[index] = (char *)malloc(sizeof(char) * (strlen(varname) + 1));
    strcpy(varmap.vars[index], varname);
    varmap.dvalue[index] = dvalue;
    varmap.ivalue[index] = ivalue;
    varmap.dynvalue[index] = dynvalue;
  } else {
    varmap.dvalue[index] = dvalue;
    varmap.ivalue[index] = ivalue;
    varmap.dynvalue[index] = dynvalue;
  }
  return index;
}

int GetNamedVarIndex(const namedvars varmap, const char *varname) {
  int i;
  for (i = 0; i < varmap.varcnt; i++) {
    if (varmap.vars[i] == NULL)
      return -1 * i;
    if (strcmp(varmap.vars[i], varname) == 0)
      return i;
  }
  return -1 * varmap.varcnt;
}

char *GetNodeVarName(DdManager *manager, namedvars varmap, DdNode *node) {
  if (node == NULL)
    return NULL;
  if (node == HIGH(manager))
    return "true";
  if (node == LOW(manager))
    return "false";
  return varmap.vars[GetIndex(node) - varmap.varstart];
}

char *GetNodeVarNameDisp(DdManager *manager, namedvars varmap, DdNode *node) {
  if (HIGH(manager) == node)
    return "TRUE";
  if (LOW(manager) == node)
    return "FALSE";
  if (NULL == node)
    return "(null)";
  return varmap.vars[GetIndex(node) - varmap.varstart];
}

int RepairVarcnt(namedvars *varmap) {
  while (varmap->vars[varmap->varcnt - 1] == NULL)
    varmap->varcnt--;
  return varmap->varcnt;
}

int all_loaded(namedvars varmap, int disp) {
  int i, res = 1;
  for (i = 0; i < varmap.varcnt; i++) {
    if (varmap.loaded[i] == 0) {
      res = 0;
      if (disp)
        fprintf(stderr, "The variable: %s was not loaded with values.\n",
                varmap.vars[i]);
      else
        return 0;
    }
  }
  return res;
}

/* Parser */

DdNode *FileGenerateBDD(DdManager *manager, namedvars varmap,
                        bddfileheader fileheader) {
  int icomment, maxlinesize, icur, iline, curinter, iequal;
  DdNode *Line, **inter;
  char buf, *inputline, *filename;
  bddfileheader interfileheader;
  //long startAt, endAt;
  //  double secs;

  // Initialization of intermediate steps
  inter = (DdNode **)malloc(sizeof(DdNode *) * fileheader.intercnt);
  for (icur = 0; icur < fileheader.intercnt; icur++)
    inter[icur] = NULL;
  // Read file data
  interfileheader.inputfile = NULL;
  filename = NULL;  // For nested files
  iequal = 0;       // Flag for encountered = sign
  icur = 0;         // Pointer for inputline buffer location
  iline = 5;        // Current file line (first after header)
  icomment = 0;     // Flag for comments
  maxlinesize = 80; // inputline starting buffer size
  inputline = (char *)malloc(sizeof(char) * maxlinesize);
  while (!feof(fileheader.inputfile)) {
    fread(&buf, 1, 1, fileheader.inputfile);
    if (buf == ';' || buf == '%' || buf == '$')
      icomment = 1;
    if (buf == '\n') {
      if (icomment)
        icomment = 0;
      /*   if (iequal > 1) {
           fprintf(stderr, "Error at line: %i. Line contains more than 1
         equal(=) signs.\n", iline);
           fclose(fileheader.inputfile);
           free(inter);
           free(inputline);
           return NULL;
         } else*/
      iequal = 0;
      if (icur > 0) {
        inputline[icur] = '\0';
        if (inputline[0] != 'L') {
          fprintf(
              stderr,
              "Error at line: %i. Intermediate results should start with L.\n",
              iline);
          fclose(fileheader.inputfile);
          free(inter);
          free(inputline);
          return NULL;
        }
        curinter = getInterBDD(inputline);
        if (curinter == -1) {
          if (inputline[0] == 'L' && IsPosNumber(inputline + 1)) {
            curinter = atoi(inputline + 1) - 1;
            if (curinter > -1 && curinter < fileheader.intercnt &&
                inter[curinter] != NULL) {
              if (_debug)
                fprintf(stderr, "Returned: %s\n", inputline);
              fclose(fileheader.inputfile);
              Line = inter[curinter];
              free(inter);
              free(inputline);
              return Line;
            } else {
              fprintf(stderr,
                      "Error at line: %i. Return result asked doesn't exist.\n",
                      iline);
              fclose(fileheader.inputfile);
              free(inter);
              free(inputline);
              return NULL;
            }
          } else {
            fprintf(stderr,
                    "Error at line: %i. Invalid intermediate result format.\n",
                    iline);
            fclose(fileheader.inputfile);
            free(inter);
            free(inputline);
            return NULL;
          }
        } else if (curinter > -1 && curinter < fileheader.intercnt &&
                   inter[curinter] == NULL) {
          if (_debug)
            fprintf(stderr, "%i %s\n", curinter, inputline);
          filename = getFileName(inputline);
          if (filename == NULL) {
	    //            startAt = clock();
            Line = LineParser(manager, varmap, inter, fileheader.intercnt,
                              inputline, iline);
	    //endAt = clock();
	    //            secs =
	    //  ((double)(endAt - startAt)) / ((double)CLOCKS_PER_SEC) * 1000;
          } else {
            interfileheader = ReadFileHeader(filename);
            if (interfileheader.inputfile == NULL) {
              Line = NULL;
            } else {
              Line = FileGenerateBDD(manager, varmap, interfileheader);
            }
            if (Line == NULL)
              fprintf(stderr,
                      "Error at line: %i. Error in nested BDD file: %s.\n",
                      iline, filename);
            free(filename);
            filename = NULL;
            interfileheader.inputfile = NULL;
          }
          if (Line == NULL) {
            fclose(fileheader.inputfile);
            free(inter);
            free(inputline);
            return NULL;
          }
          inter[curinter] = Line;
          icur = 0;
        } else if (curinter > -1 && curinter < fileheader.intercnt &&
                   inter[curinter] != NULL) {
          fprintf(
              stderr,
              "Error at line: %i. Intermediate results can't be overwritten.\n",
              iline);
          fclose(fileheader.inputfile);
          free(inter);
          free(inputline);
          return NULL;
        } else {
          fprintf(
              stderr,
              "Error at line: %i. Intermediate result asked doesn't exist.\n",
              iline);
          fclose(fileheader.inputfile);
          free(inter);
          free(inputline);
          return NULL;
        }
      }
      iline++;
    } else if (buf != ' ' && buf != '\t' && !icomment) {
      if (buf == '=')
        iequal++;
      inputline[icur] = buf;
      icur += 1;
      if (icur == _maxbufsize) {
        fprintf(stderr, "Error: Maximum buffer size(%i) exceeded.\n",
                _maxbufsize);
        fclose(fileheader.inputfile);
        free(inter);
        free(inputline);
        return NULL;
      }
      while (icur > maxlinesize - 1) {
        maxlinesize *= 2;
        inputline = (char *)realloc(inputline, sizeof(char) * maxlinesize);
      }
    }
  }
  fprintf(stderr, "Error, file either doesn't end with a blank line or no "
                  "return result was asked.\n");
  fclose(fileheader.inputfile);
  free(inter);
  free(inputline);
  return NULL;
}

int getInterBDD(char *function) {
  int i, ret;
  char *inter;
  for (i = 0; i < strlen(function); i++) {
    if (function[i] == '=') {
      inter = (char *)malloc(sizeof(char) * i);
      strncpy(inter, function + 1, i - 1);
      inter[i - 1] = '\0';
      if (IsPosNumber(inter)) {
        ret = atoi(inter) - 1;
        free(inter);
        return ret;
      } else {
        free(inter);
        return -1;
      }
    }
  }
  return -1;
}

char *getFileName(const char *function) {
  int i = 0;
  char *filename;
  while (function[i] != '=' && (i + 1) < strlen(function))
    i++;
  if ((i + 1) < strlen(function)) {
    i++;
    if (function[i] == '<' && function[strlen(function) - 1] == '>') {
      filename = (char *)malloc(sizeof(char) * strlen(function) - i);
      strcpy(filename, function + i + 1);
      filename[strlen(function) - i - 2] = '\0';
      return filename;
    }
  }
  return NULL;
}

DdNode *LineParser(DdManager *manager, namedvars varmap, DdNode **inter,
                   int maxinter, char *function, int iline) {
  int istart, iend, ilength, i, symbol, ivar, inegvar, inegoper, iconst, value;
  long startAt, endAt;
  //  double secs;
  DdNode *bdd;
  char *term, curoper;
  bdd = HIGH(manager);
  Cudd_Ref(bdd);
  term = NULL;
  ivar = -1;
  curoper = '*';
  istart = -1;
  iend = strlen(function) - 1;
  ilength = -1;
  symbol = -1;
  inegvar = 0;
  inegoper = 0;
  iconst = 0;
  for (i = strlen(function) - 1; i > -1; i--) {
    if (symbol == -1 && isOperator(function[i])) {
      symbol = i;
      istart = i + 1;
      ilength = iend - i;
      iend = i - 1;
      if (ilength > 0 && !(ilength == 1 && function[istart] == '~')) {
        term = (char *)malloc(sizeof(char) * (ilength + 1));
        strncpy(term, function + istart, ilength);
        term[ilength] = '\0';
      } else {
        fprintf(stderr, "Line Parser Error at line: %i. An operator was "
                        "encounter with no term at its right side.\n",
                iline);
        free(term);
        return NULL;
      }
    }
    if (symbol != -1) {
      if (term[0] == '~')
        inegvar = 1;
      else
        inegvar = 0;
      if (term[0 + inegvar] != 'L') {
        // Term is a variable
        if (strcmp(term + inegvar, "TRUE") == 0) {
          iconst = 1;
        } else if (strcmp(term + inegvar, "FALSE") == 0) {
          iconst = 1;
          inegvar = 1;
        } else {
          iconst = 0;
          ivar = AddNamedMultiVar(manager, varmap, term + inegvar, &value);

          if (ivar == -1) {
            fprintf(stderr, "Line Parser Error at line: %i. More BDD variables "
                            "than the reserved term: %s.\n",
                    iline, term);
            free(term);
            return NULL;
          }
        }
        if (_debug)
          fprintf(stderr, "%s\n", term);
        if (_debug && !iconst)
          fprintf(stderr, "PNZ1:%.0f P1:%.0f S1:%i PNZ2:%.0f P2:%.0f S2:%i\n",
                  Cudd_CountPathsToNonZero(bdd), Cudd_CountPath(bdd),
                  Cudd_DagSize(bdd), Cudd_CountPathsToNonZero(GetVar(
                                         manager, ivar + varmap.varstart)),
                  Cudd_CountPath(GetVar(manager, ivar + varmap.varstart)),
                  Cudd_DagSize(GetVar(manager, ivar + varmap.varstart)));
        startAt = clock();
        if (!iconst) {
          if (inegvar)
            bdd = BDD_Operator(
                manager,
                NOT(GetMVar(manager, ivar + varmap.varstart, value, varmap)),
                bdd, curoper, inegoper);
          else
            bdd = BDD_Operator(manager, GetMVar(manager, ivar + varmap.varstart,
                                                value, varmap),
                               bdd, curoper, inegoper);
        } else {
          switch (curoper) {
          case '+':
            if (inegvar ^ inegoper)
              ;
            else {
              bdd = HIGH(manager);
              Cudd_Ref(bdd);
            }
            break;
          case '*':
            if (inegvar ^ inegoper) {
              bdd = LOW(manager);
              Cudd_Ref(bdd);
            }
            break;
          case '#':
            if (inegvar ^ inegoper)
              ;
            else
              bdd = NOT(bdd);
            break;
          }
        }
        endAt = clock();
	//        secs = ((double)(endAt - startAt)) / ((double)CLOCKS_PER_SEC);
        if (_debug)
          fprintf(stderr, "term: %s of line: %i took: %li\n", term, iline,
                  endAt - startAt);
        // if ((endAt - startAt) > 10000000) Cudd_AutodynDisable(manager);
        if (bdd == NULL) {
          fprintf(stderr, "Line Parser Error at line: %i. Error using operator "
                          "%c on term: %s.\n",
                  iline, curoper, term);
          free(term);
          return NULL;
        }
      } else {
        // Term is an intermediate result
        if (IsPosNumber(term + inegvar + 1)) {
          ivar = atoi(term + inegvar + 1) - 1;
        } else {
          fprintf(stderr, "Line Parser Error at line: %i. Invalid intermediate "
                          "result format term: %s.\n",
                  iline, term);
          free(term);
          return NULL;
        }
        if (ivar < 0 || ivar > maxinter || inter[ivar] == NULL) {
          fprintf(stderr, "Line Parser Error at line: %i. Usage of undeclared "
                          "intermediate result term: %s.\n",
                  iline, term);
          free(term);
          return NULL;
        }
        if (_debug)
          fprintf(stderr, "%s\n", term);
        if (_debug)
          fprintf(stderr, "PNZ1:%.0f P1:%.0f S1:%i PNZ2:%.0f P2:%.0f S2:%i\n",
                  Cudd_CountPathsToNonZero(bdd), Cudd_CountPath(bdd),
                  Cudd_DagSize(bdd), Cudd_CountPathsToNonZero(inter[ivar]),
                  Cudd_CountPath(inter[ivar]), Cudd_DagSize(inter[ivar]));
	//startAt = clock();
        if (inegvar)
          bdd = BDD_Operator(manager, NOT(inter[ivar]), bdd, curoper, inegoper);
        else
          bdd = BDD_Operator(manager, inter[ivar], bdd, curoper, inegoper);
	//endAt = clock();
	//secs = ((double)(endAt - startAt)) / ((double)CLOCKS_PER_SEC);
	//if (_debug)
	//  fprintf(stderr, "term: %s of line: %i took: %li\n", term, iline,
	//          endAt - startAt);
        // if ((endAt - startAt) > 10000000) Cudd_AutodynDisable(manager);
        if (bdd == NULL) {
          fprintf(stderr, "Line Parser Error at line: %i. Error using operator "
                          "%c on term: %s.\n",
                  iline, curoper, term);
          free(term);
          return NULL;
        }
      }
      free(term);
      term = NULL;
      curoper = function[symbol];
      if (curoper == '=')
        return bdd;
      if (function[symbol - 1] == '~') {
        inegoper = 1;
        i--;
        iend--;
      } else {
        inegoper = 0;
      }
      symbol = -1;
    }
  }
  return NULL;
}

DdNode *BDD_Operator(DdManager *manager, DdNode *bdd1, DdNode *bdd2,
                     char Operator, int inegoper) {
  switch (Operator) {
  case '+':
    if (inegoper)
      return D_BDDNor(manager, bdd1, bdd2);
    else
      return D_BDDOr(manager, bdd1, bdd2);
    break;
  case '*':
    if (inegoper)
      return D_BDDNand(manager, bdd1, bdd2);
    else
      return D_BDDAnd(manager, bdd1, bdd2);
    break;
  case '#':
    if (inegoper)
      return D_BDDXnor(manager, bdd1, bdd2);
    else
      return D_BDDXor(manager, bdd1, bdd2);
    break;
  default:
    return NULL;
    break;
  }
}

DdNode *OnlineGenerateBDD(DdManager *manager, namedvars *varmap) {
  int icomment, maxlinesize, icur, iline, curinter, iequal, iinters, itmp, i;
  DdNode *Line, **inter;
  char buf, *inputline, *filename;
  bddfileheader interfileheader;
  // Initialization of intermediate steps
  iinters = 1;
  inter = (DdNode **)malloc(sizeof(DdNode *) * iinters);
  for (icur = 0; icur < iinters; icur++)
    inter[icur] = NULL;
  // Read file data
  interfileheader.inputfile = NULL;
  filename = NULL;  // For nested files
  iequal = 0;       // Flag for encountered = sign
  icur = 0;         // Pointer for inputline buffer location
  iline = 1;        // Current file line (first after header)
  icomment = 0;     // Flag for comments
  maxlinesize = 80; // inputline starting buffer size
  inputline = (char *)malloc(sizeof(char) * maxlinesize);

  do {
    buf = fgetc(stdin);
    if (buf == ';' || buf == '%' || buf == '$')
      icomment = 1;
    if (buf == '\n') {
      if (icomment)
        icomment = 0;
      if (iequal > 1) {
        fprintf(
            stderr,
            "Error at line: %i. Line contains more than 1 equal(=) signs.\n",
            iline);
        free(inter);
        free(inputline);
        return NULL;
      } else
        iequal = 0;
      if (icur > 0) {
        inputline[icur] = '\0';
        if (inputline[0] == '@') {
          if (inputline[1] == 'e') {
            free(inter);
            free(inputline);
            exit(0);
          } else {
            itmp = GetParam(inputline, 1);
            if (itmp > varmap->varcnt)
              EnlargeNamedVars(varmap, itmp);
            itmp = GetParam(inputline, 2);
            if (itmp > iinters) {
              inter = (DdNode **)realloc(inter, sizeof(DdNode *) * itmp);
              for (i = iinters; i < itmp; i++)
                inter[i] = NULL;
              iinters = itmp;
            }
          }
          icur = 0;
        } else {
          if (inputline[0] != 'L') {
            fprintf(stderr, "Error at line: %i. Intermediate results should "
                            "start with L.\n",
                    iline);
            free(inter);
            free(inputline);
            return NULL;
          }
          curinter = getInterBDD(inputline);
          if (curinter == -1) {
            if (inputline[0] == 'L' && IsPosNumber(inputline + 1)) {
              curinter = atoi(inputline + 1) - 1;
              if (curinter > -1 && curinter < iinters &&
                  inter[curinter] != NULL) {
                if (_debug)
                  fprintf(stderr, "Returned: %s\n", inputline);
                Line = inter[curinter];
                free(inter);
                free(inputline);
                return Line;
              } else {
                fprintf(
                    stderr,
                    "Error at line: %i. Return result asked doesn't exist.\n",
                    iline);
                free(inter);
                free(inputline);
                return NULL;
              }
            } else {
              fprintf(
                  stderr,
                  "Error at line: %i. Invalid intermediate result format.\n",
                  iline);
              free(inter);
              free(inputline);
              return NULL;
            }
          } else if (curinter > -1) {
            if (curinter >= iinters) {
              inter =
                  (DdNode **)realloc(inter, sizeof(DdNode *) * (curinter + 1));
              for (i = iinters; i < curinter + 1; i++)
                inter[i] = NULL;
              iinters = curinter + 1;
            }
            if (inter[curinter] == NULL) {
              if (_debug)
                fprintf(stderr, "%i %s\n", curinter, inputline);
              filename = getFileName(inputline);
              if (filename == NULL) {
                Line = OnlineLineParser(manager, varmap, inter, iinters,
                                        inputline, iline);
              } else {
                interfileheader = ReadFileHeader(filename);
                if (interfileheader.inputfile == NULL) {
                  // Line = simpleBDDload(manager, varmap, filename);
                  Line = NULL;
                } else {
                  Line = FileGenerateBDD(manager, *varmap, interfileheader);
                }
                if (Line == NULL)
                  fprintf(stderr,
                          "Error at line: %i. Error in nested BDD file: %s.\n",
                          iline, filename);
                free(filename);
                filename = NULL;
                interfileheader.inputfile = NULL;
              }
              if (Line == NULL) {
                free(inter);
                free(inputline);
                return NULL;
              }
              inter[curinter] = Line;
              icur = 0;
            } else if (inter[curinter] != NULL) {
              fprintf(stderr, "Error at line: %i. Intermediate results can't "
                              "be overwritten.\n",
                      iline);
              free(inter);
              free(inputline);
              return NULL;
            }
          } else {
            fprintf(
                stderr,
                "Error at line: %i. Intermediate result asked doesn't exist.\n",
                iline);
            free(inter);
            free(inputline);
            return NULL;
          }
        }
      }
      iline++;
    } else if (buf != ' ' && buf != '\t' && !icomment) {
      if (buf == '=')
        iequal++;
      inputline[icur] = buf;
      icur += 1;
      if (icur == _maxbufsize) {
        fprintf(stderr, "Error: Maximum buffer size(%i) exceeded.\n",
                _maxbufsize);
        free(inter);
        free(inputline);
        return NULL;
      }
      while (icur > maxlinesize - 1) {
        maxlinesize *= 2;
        inputline = (char *)realloc(inputline, sizeof(char) * maxlinesize);
      }
    }
  } while (1);
  fprintf(stderr, "Error, file either doesn't end with a blank line or no "
                  "return result was asked.\n");
  free(inter);
  free(inputline);
  return NULL;
}

DdNode *OnlineLineParser(DdManager *manager, namedvars *varmap, DdNode **inter,
                         int maxinter, char *function, int iline) {
  int istart, iend, ilength, i, symbol, ivar, inegvar, inegoper, iconst;
  long startAt, endAt;
  //  double secs;
  DdNode *bdd;
  char *term, curoper;
  bdd = HIGH(manager);
  Cudd_Ref(bdd);
  term = NULL;
  ivar = -1;
  curoper = '*';
  istart = -1;
  iend = strlen(function) - 1;
  ilength = -1;
  symbol = -1;
  inegvar = 0;
  inegoper = 0;
  iconst = 0;
  for (i = strlen(function) - 1; i > -1; i--) {
    if (symbol == -1 && isOperator(function[i])) {
      symbol = i;
      istart = i + 1;
      ilength = iend - i;
      iend = i - 1;
      if (ilength > 0 && !(ilength == 1 && function[istart] == '~')) {
        term = (char *)malloc(sizeof(char) * (ilength + 1));
        strncpy(term, function + istart, ilength);
        term[ilength] = '\0';
      } else {
        fprintf(stderr, "Line Parser Error at line: %i. An operator was "
                        "encounter with no term at its right side.\n",
                iline);
        free(term);
        return NULL;
      }
    }
    if (symbol != -1) {
      if (term[0] == '~')
        inegvar = 1;
      else
        inegvar = 0;
      if (term[0 + inegvar] != 'L') {
        // Term is a variable
        if (strcmp(term, "TRUE") == 0) {
          iconst = 1;
        } else if (strcmp(term, "FALSE") == 0) {
          iconst = 1;
          inegvar = 1;
        } else {
          iconst = 0;
          ivar = AddNamedVar(*varmap, term + inegvar);
          if (ivar == -1) {
            EnlargeNamedVars(varmap, varmap->varcnt + 1);
            ivar = AddNamedVar(*varmap, term + inegvar);
          }
          if (ivar == -1) {
            fprintf(stderr, "Line Parser Error at line: %i. More BDD variables "
                            "than the reserved term: %s.\n",
                    iline, term);
            free(term);
            return NULL;
          }
        }
        if (_debug)
          fprintf(stderr, "%s\n", term);
        if (_debug && !iconst)
          fprintf(stderr, "PNZ1:%.0f P1:%.0f S1:%i PNZ2:%.0f P2:%.0f S2:%i\n",
                  Cudd_CountPathsToNonZero(bdd), Cudd_CountPath(bdd),
                  Cudd_DagSize(bdd), Cudd_CountPathsToNonZero(GetVar(
                                         manager, ivar + varmap->varstart)),
                  Cudd_CountPath(GetVar(manager, ivar + varmap->varstart)),
                  Cudd_DagSize(GetVar(manager, ivar + varmap->varstart)));
        startAt = clock();
        if (!iconst) {
          if (inegvar)
            bdd = BDD_Operator(manager,
                               NOT(GetVar(manager, ivar + varmap->varstart)),
                               bdd, curoper, inegoper);
          else
            bdd =
                BDD_Operator(manager, GetVar(manager, ivar + varmap->varstart),
                             bdd, curoper, inegoper);
        } else {
          switch (curoper) {
          case '+':
            if (inegvar ^ inegoper)
              ;
            else {
              bdd = HIGH(manager);
              Cudd_Ref(bdd);
            }
            break;
          case '*':
            if (inegvar ^ inegoper) {
              bdd = LOW(manager);
              Cudd_Ref(bdd);
            }
            break;
          case '#':
            if (inegvar ^ inegoper)
              ;
            else
              bdd = NOT(bdd);
            break;
          }
        }
        endAt = clock();
	//        secs = ((double)(endAt - startAt)) / ((double)CLOCKS_PER_SEC);
        if (_debug)
          fprintf(stderr, "term: %s of line: %i took: %li\n", term, iline,
                  endAt - startAt);
        // if ((endAt - startAt) > 10000000) Cudd_AutodynDisable(manager);
        if (bdd == NULL) {
          fprintf(stderr, "Line Parser Error at line: %i. Error using operator "
                          "%c on term: %s.\n",
                  iline, curoper, term);
          free(term);
          return NULL;
        }
      } else {
        // Term is an intermediate result
        if (IsPosNumber(term + inegvar + 1))
          ivar = atoi(term + inegvar + 1) - 1;
        else {
          fprintf(stderr, "Line Parser Error at line: %i. Invalid intermediate "
                          "result format term: %s.\n",
                  iline, term);
          free(term);
          return NULL;
        }
        if (ivar < 0 || ivar > maxinter || inter[ivar] == NULL) {
          fprintf(stderr, "Line Parser Error at line: %i. Usage of undeclared "
                          "intermediate result term: %s.\n",
                  iline, term);
          free(term);
          return NULL;
        }
        if (_debug)
          fprintf(stderr, "%s\n", term);
        if (_debug)
          fprintf(stderr, "PNZ1:%.0f P1:%.0f S1:%i PNZ2:%.0f P2:%.0f S2:%i\n",
                  Cudd_CountPathsToNonZero(bdd), Cudd_CountPath(bdd),
                  Cudd_DagSize(bdd), Cudd_CountPathsToNonZero(inter[ivar]),
                  Cudd_CountPath(inter[ivar]), Cudd_DagSize(inter[ivar]));
	//startAt = clock();
        if (inegvar)
          bdd = BDD_Operator(manager, NOT(inter[ivar]), bdd, curoper, inegoper);
        else
          bdd = BDD_Operator(manager, inter[ivar], bdd, curoper, inegoper);
	//endAt = clock();
	//secs = ((double)(endAt - startAt)) / ((double)CLOCKS_PER_SEC);
	//if (_debug)
	//  fprintf(stderr, "term: %s of line: %i took: %li\n", term, iline,
	//          endAt - startAt);
        // if ((endAt - startAt) > 10000000) Cudd_AutodynDisable(manager);
        if (bdd == NULL) {
          fprintf(stderr, "Line Parser Error at line: %i. Error using operator "
                          "%c on term: %s.\n",
                  iline, curoper, term);
          free(term);
          return NULL;
        }
      }
      free(term);
      term = NULL;
      curoper = function[symbol];
      if (curoper == '=')
        return bdd;
      if (function[symbol - 1] == '~') {
        inegoper = 1;
        i--;
        iend--;
      } else {
        inegoper = 0;
      }
      symbol = -1;
    }
  }
  return NULL;
}

int GetParam(char *inputline, int iParam) {
  int icoma, istart, iend, ret;
  char *numb;
  istart = 1;
  icoma = istart;
  iend = strlen(inputline);
  while ((inputline[icoma] != ',') && (icoma < iend))
    icoma++;
  if (iParam == 1) {
    numb = (char *)malloc(sizeof(char) * icoma);
    strncpy(numb, inputline + 1, icoma - 1);
    numb[icoma - 1] = '\0';
    if (IsPosNumber(numb)) {
      ret = atoi(numb);
      free(numb);
      return ret;
    }
  } else if (iParam == 2) {
    numb = (char *)malloc(sizeof(char) * (iend - icoma + 1));
    strncpy(numb, inputline + icoma + 1, iend - icoma);
    numb[iend - icoma] = '\0';
    if (IsPosNumber(numb)) {
      ret = atoi(numb);
      free(numb);
      return ret;
    }
  }
  return 0;
}

void onlinetraverse(DdManager *manager, namedvars varmap, hisqueue *HisQueue,
                    DdNode *bdd) {
  char buf, *inputline;
  int icur, maxlinesize, iline, index, iloop, iQsize, i, inQ;
  DdNode **Q, **Q2, *h_node, *l_node, *curnode;
  hisqueue *his;
  hisnode *hnode;
  iloop = 1;
  icur = 0;         // Pointer for inputline buffer location
  iline = 1;        // Current file line (first after header)
  maxlinesize = 80; // inputline starting buffer size
  inputline = (char *)malloc(sizeof(char) * maxlinesize);
  curnode = bdd;
  iQsize = 0;
  Q = (DdNode **)malloc(sizeof(DdNode *) * iQsize);
  Q2 = NULL;
  his = InitHistory(varmap.varcnt);
  do {
    buf = fgetc(stdin);
    if (buf == '\n') {
      inputline[icur] = '\0';
      if ((icur > 0) && (inputline[0] == '@') &&
          (inputline[2] == ',' || inputline[2] == '\0')) {
        switch (inputline[1]) {
        case 'c':
          printf("bdd_temp_value('%s', %i).\n",
                 GetNodeVarNameDisp(manager, varmap, curnode), iQsize);
          break;
        case 'n':
          if (curnode != HIGH(manager) && curnode != LOW(manager) &&
              (hnode = GetNode(his, varmap.varstart, curnode)) == NULL) {
            AddNode(his, varmap.varstart, curnode, 0.0, 0, NULL);
            l_node = LowNodeOf(manager, curnode);
            h_node = HighNodeOf(manager, curnode);
            inQ = 0;
            for (i = 0; (i < iQsize / 2) && (inQ < 3); i++)
              inQ = (Q[i] == l_node) ||
                    (Q[iQsize - i] == l_node) + 2 * (Q[i] == h_node) ||
                    (Q[iQsize - i] == h_node);
            if ((inQ & 1) == 0)
              inQ = inQ + (GetNode(his, varmap.varstart, l_node) != NULL);
            if ((inQ & 2) == 0)
              inQ = inQ + 2 * (GetNode(his, varmap.varstart, h_node) != NULL);
            if ((inQ & 1) == 1)
              inQ = inQ - (l_node == HIGH(manager) || l_node == LOW(manager));
            if ((inQ & 2) == 2)
              inQ =
                  inQ - 2 * (h_node == HIGH(manager) || h_node == LOW(manager));
            switch (inQ) {
            case 0:
              iQsize += 2;
              Q = (DdNode **)realloc(Q, sizeof(DdNode *) * iQsize);
              Q[iQsize - 2] = l_node;
              Q[iQsize - 1] = h_node;
              break;
            case 1:
              iQsize++;
              Q = (DdNode **)realloc(Q, sizeof(DdNode *) * iQsize);
              Q[iQsize - 1] = h_node;
              break;
            case 2:
              iQsize++;
              Q = (DdNode **)realloc(Q, sizeof(DdNode *) * iQsize);
              Q[iQsize - 1] = l_node;
              break;
            case 3:
              break;
            default:
              break;
            }
          }
          if (inputline[2] == '\0' || strcmp(inputline + 3, "DFS") == 0) {
            if (iQsize > 0) {
              iQsize--;
              curnode = Q[iQsize];
              Q = (DdNode **)realloc(Q, sizeof(DdNode *) * iQsize);
            }
          } else if (strcmp(inputline + 3, "BFS") == 0) {
            if (iQsize > 0) {
              iQsize--;
              curnode = Q[0];
              Q2 = (DdNode **)malloc(sizeof(DdNode *) * iQsize);
              for (i = 0; i < iQsize; i++)
                Q2[i] = Q[i + 1];
              free(Q);
              Q = Q2;
            }
          } else {
            fprintf(stderr, "Error: Could not find method: %s, Correct syntax "
                            "@n,[DFS, BFS].\n",
                    inputline + 3);
            free(Q);
            free(inputline);
            exit(-1);
          }
          break;
        case 'h':
          printf("bdd_temp_value('%s').\n",
                 GetNodeVarNameDisp(manager, varmap,
                                    HighNodeOf(manager, curnode)));
          break;
        case 'l':
          printf(
              "bdd_temp_value('%s').\n",
              GetNodeVarNameDisp(manager, varmap, LowNodeOf(manager, curnode)));
          break;
        case 'v':
          index = GetNamedVarIndex(varmap, inputline + 3);
          if (index >= 0) {
            fprintf(stdout, "bdd_temp_value([%f,%i,%s]).\n",
                    varmap.dvalue[index], varmap.ivalue[index],
                    (char *)varmap.dynvalue[index]);
          } else {
            fprintf(stderr, "Error: Could not find variable: %s, Correct "
                            "syntax @v,[variable name].\n",
                    inputline + 3);
            free(Q);
            free(inputline);
            exit(-1);
          }
          break;
        case 'e':
          iloop = 0;
          break;
        default:
          fprintf(stderr, "Error: Not recognizable instruction: %s.\n",
                  inputline);
          free(Q);
          free(inputline);
          exit(-1);
          break;
        }
        icur = 0;
      } else {
        fprintf(stderr, "Error: Not recognizable instruction: %s.\n",
                inputline);
        free(Q);
        free(inputline);
        exit(-1);
      }
      iline++;
    } else if (buf != ' ' && buf != '\t') {
      inputline[icur] = buf;
      icur += 1;
      if (icur == _maxbufsize) {
        fprintf(stderr, "Error: Maximum buffer size(%i) exceeded.\n",
                _maxbufsize);
        free(Q);
        free(inputline);
        exit(-1);
      }
      while (icur > maxlinesize - 1) {
        maxlinesize *= 2;
        inputline = (char *)realloc(inputline, sizeof(char) * maxlinesize);
      }
    }
  } while (iloop);
  free(Q);
  free(inputline);
}

DdNode *equality(DdManager *mgr, int varIndex, int value, namedvars varmap) {

  int i;
  int bit, *booleanVars, booleanVar;
  variable v;
  DdNode *node, *tmp;

  v = varmap.mvars[varIndex];
  booleanVars = v.booleanVars;
  i = v.nBit - 1;
  booleanVar = booleanVars[i];
  bit = value & 01;
  if (bit) {
    node = Cudd_bddIthVar(mgr, booleanVar);
  } else {
    node = Cudd_Not(Cudd_bddIthVar(mgr, booleanVar));
  }
  value = value >> 1;
  i--;

  while (i >= 0) {
    booleanVar = booleanVars[i];
    bit = value & 01;
    if (bit) {
      tmp = Cudd_bddAnd(mgr, node, Cudd_bddIthVar(mgr, booleanVar));
      Cudd_Ref(tmp);
    } else {
      tmp = Cudd_bddAnd(mgr, node, Cudd_Not(Cudd_bddIthVar(mgr, booleanVar)));
      Cudd_Ref(tmp);
    }
    value = value >> 1;
    i--;
    Cudd_RecursiveDeref(mgr, node);
    node = tmp;
  }
  return node;
}
