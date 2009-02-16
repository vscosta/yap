/******************************************************************************\
*                                                                              *
*    SimpleCUDD library (www.cs.kuleuven.be/~theo/tools/simplecudd.html)       *
*  SimpleCUDD was developed at Katholieke Universiteit Leuven(www.kuleuven.be) *
*                                                                              *
*  Copyright T. Mantadelis and Katholieke Universiteit Leuven 2008             *
*                                                                              *
*  Author: Theofrastos Mantadelis                                              *
*  File: Example.c                                                             *
*                                                                              *
********************************************************************************
*                                                                              *
*        The "Artistic License"                                                *
*                                                                              *
*         Preamble                                                             *
*                                                                              *
* The intent of this document is to state the conditions under which a         *
* Package may be copied, such that the Copyright Holder maintains some         *
* semblance of artistic control over the development of the package,           *
* while giving the users of the package the right to use and distribute        *
* the Package in a more-or-less customary fashion, plus the right to make      *
* reasonable modifications.                                                    *
*                                                                              *
* Definitions:                                                                 *
*                                                                              *
*   "Package" refers to the collection of files distributed by the             *
*   Copyright Holder, and derivatives of that collection of files              *
*   created through textual modification.                                      *
*                                                                              *
*   "Standard Version" refers to such a Package if it has not been             *
*   modified, or has been modified in accordance with the wishes               *
*   of the Copyright Holder as specified below.                                *
*                                                                              *
*   "Copyright Holder" is whoever is named in the copyright or                 *
*   copyrights for the package.                                                *
*                                                                              *
*   "You" is you, if you're thinking about copying or distributing             *
*   this Package.                                                              *
*                                                                              *
*   "Reasonable copying fee" is whatever you can justify on the                *
*   basis of media cost, duplication charges, time of people involved,         *
*   and so on.  (You will not be required to justify it to the                 *
*   Copyright Holder, but only to the computing community at large             *
*   as a market that must bear the fee.)                                       *
*                                                                              *
*   "Freely Available" means that no fee is charged for the item               *
*   itself, though there may be fees involved in handling the item.            *
*   It also means that recipients of the item may redistribute it              *
*   under the same conditions they received it.                                *
*                                                                              *
* 1. You may make and give away verbatim copies of the source form of the      *
* Standard Version of this Package without restriction, provided that you      *
* duplicate all of the original copyright notices and associated disclaimers.  *
*                                                                              *
* 2. You may apply bug fixes, portability fixes and other modifications        *
* derived from the Public Domain or from the Copyright Holder.  A Package      *
* modified in such a way shall still be considered the Standard Version.       *
*                                                                              *
* 3. You may otherwise modify your copy of this Package in any way, provided   *
* that you insert a prominent notice in each changed file stating how and      *
* when you changed that file, and provided that you do at least ONE of the     *
* following:                                                                   *
*                                                                              *
*    a) place your modifications in the Public Domain or otherwise make them   *
*    Freely Available, such as by posting said modifications to Usenet or      *
*    an equivalent medium, or placing the modifications on a major archive     *
*    site such as uunet.uu.net, or by allowing the Copyright Holder to include *
*    your modifications in the Standard Version of the Package.                *
*                                                                              *
*    b) use the modified Package only within your corporation or organization. *
*                                                                              *
*    c) rename any non-standard executables so the names do not conflict       *
*    with standard executables, which must also be provided, and provide       *
*    a separate manual page for each non-standard executable that clearly      *
*    documents how it differs from the Standard Version.                       *
*                                                                              *
*    d) make other distribution arrangements with the Copyright Holder.        *
*                                                                              *
* 4. You may distribute the programs of this Package in object code or         *
* executable form, provided that you do at least ONE of the following:         *
*                                                                              *
*    a) distribute a Standard Version of the executables and library files,    *
*    together with instructions (in the manual page or equivalent) on where    *
*    to get the Standard Version.                                              *
*                                                                              *
*    b) accompany the distribution with the machine-readable source of         *
*    the Package with your modifications.                                      *
*                                                                              *
*    c) give non-standard executables non-standard names, and clearly          *
*    document the differences in manual pages (or equivalent), together        *
*    with instructions on where to get the Standard Version.                   *
*                                                                              *
*    d) make other distribution arrangements with the Copyright Holder.        *
*                                                                              *
* 5. You may charge a reasonable copying fee for any distribution of this      *
* Package.  You may charge any fee you choose for support of this              *
* Package.  You may not charge a fee for this Package itself.  However,        *
* you may distribute this Package in aggregate with other (possibly            *
* commercial) programs as part of a larger (possibly commercial) software      *
* distribution provided that you do not advertise this Package as a            *
* product of your own.  You may embed this Package's interpreter within        *
* an executable of yours (by linking); this shall be construed as a mere       *
* form of aggregation, provided that the complete Standard Version of the      *
* interpreter is so embedded.                                                  *
*                                                                              *
* 6. The scripts and library files supplied as input to or produced as         *
* output from the programs of this Package do not automatically fall           *
* under the copyright of this Package, but belong to whoever generated         *
* them, and may be sold commercially, and may be aggregated with this          *
* Package.  If such scripts or library files are aggregated with this          *
* Package via the so-called "undump" or "unexec" methods of producing a        *
* binary executable image, then distribution of such an image shall            *
* neither be construed as a distribution of this Package nor shall it          *
* fall under the restrictions of Paragraphs 3 and 4, provided that you do      *
* not represent such an executable image as a Standard Version of this         *
* Package.                                                                     *
*                                                                              *
* 7. C subroutines (or comparably compiled subroutines in other                *
* languages) supplied by you and linked into this Package in order to          *
* emulate subroutines and variables of the language defined by this            *
* Package shall not be considered part of this Package, but are the            *
* equivalent of input as in Paragraph 6, provided these subroutines do         *
* not change the language in any way that would cause it to fail the           *
* regression tests for the language.                                           *
*                                                                              *
* 8. Aggregation of this Package with a commercial distribution is always      *
* permitted provided that the use of this Package is embedded; that is,        *
* when no overt attempt is made to make this Package's interfaces visible      *
* to the end user of the commercial distribution.  Such use shall not be       *
* construed as a distribution of this Package.                                 *
*                                                                              *
* 9. The name of the Copyright Holder may not be used to endorse or promote    *
* products derived from this software without specific prior written           *
* permission.                                                                  *
*                                                                              *
* 10. THIS PACKAGE IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR              *
* IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED               *
* WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.          *
*                                                                              *
*         The End                                                              *
*                                                                              *
\******************************************************************************/


#include "simplecudd.h"

typedef struct _extmanager {
  DdManager *manager;
  DdNode *t, *f;
  hisqueue *his;
  namedvars varmap;
} extmanager;

void DFS(extmanager MyManager, DdNode *Current);
int compexpand(extmanager MyManager, DdNode *Current, extmanager MyManager2, DdNode *Current2);
int bufstrcat(char *targetstr, int targetmem, const char *srcstr);
void getalltruepaths(extmanager MyManager, DdNode *Current, const char *startpath, const char *prevvar);

int main(int argc, char **arg) {
  extmanager MyManager;
  DdNode *bdd;
  bddfileheader fileheader;
  int code;
  char yn;
  code = -1;
  if (argc != 2) {
    fprintf(stderr, "\nUsage: %s [filename]\nGenerates and traverses a BDD from file\n", arg[0]);
    fprintf(stderr, "\nUsage: %s -online\nGenerates and traverses a BDD online mode\n", arg[0]);
    return code;
  }
  RAPIDLOADON;
  if (strcmp("-online", arg[1]) == 0) {
    MyManager.manager = simpleBDDinit(0);
    MyManager.t = HIGH(MyManager.manager);
    MyManager.f = LOW(MyManager.manager);
    MyManager.varmap = InitNamedVars(1, 0);
    bdd = OnlineGenerateBDD(MyManager.manager, &MyManager.varmap);
  } else {
    fileheader = ReadFileHeader(arg[1]);
    switch(fileheader.filetype) {
      case BDDFILE_SCRIPT:
        MyManager.manager = simpleBDDinit(fileheader.varcnt);
        MyManager.t = HIGH(MyManager.manager);
        MyManager.f = LOW(MyManager.manager);
        MyManager.varmap = InitNamedVars(fileheader.varcnt, fileheader.varstart);
        bdd = FileGenerateBDD(MyManager.manager, MyManager.varmap, fileheader);
        break;
      case BDDFILE_NODEDUMP:
        MyManager.manager = simpleBDDinit(fileheader.varcnt);
        MyManager.t = HIGH(MyManager.manager);
        MyManager.f = LOW(MyManager.manager);
        MyManager.varmap = InitNamedVars(fileheader.varcnt, fileheader.varstart);
        bdd = LoadNodeDump(MyManager.manager, MyManager.varmap, fileheader.inputfile);
        break;
      default:
        fprintf(stderr, "Error: not a valid file format to load.\n");
        return code;
        break;
    }
  }
  if (bdd != NULL) {
    printf("Do you want to load parameter values from testdata.txt [y]? "); yn = getchar(); getchar();
    if (yn == 'y') LoadVariableData(MyManager.varmap, "testdata.txt");
    code = 0;
    MyManager.his = InitHistory(GetVarCount(MyManager.manager));
    if (strcmp("-online", arg[1]) != 0) {
      DFS(MyManager, bdd);
      printf("Do you need an export [y]? "); yn = getchar(); getchar();
      if (yn == 'y') simpleNamedBDDtoDot(MyManager.manager, MyManager.varmap, bdd, "SimpleCUDDExport.dot");
      printf("Do you want a save [y]? "); yn = getchar(); getchar();
      if (yn == 'y') SaveNodeDump(MyManager.manager, MyManager.varmap, bdd, "SimpleCUDDSave.sav");
      printf("Do you want to see all true paths [y]? "); yn = getchar(); getchar();
      if (yn == 'y') {
        ReInitHistory(MyManager.his, GetVarCount(MyManager.manager));
        getalltruepaths(MyManager, bdd, "", "");
      }
    } else {
      onlinetraverse(MyManager.manager, MyManager.varmap, MyManager.his, bdd);
    }
  }
  if (MyManager.manager != NULL) KillBDD(MyManager.manager);
  return code;
}

void DFS(extmanager MyManager, DdNode *Current) {
  DdNode *h, *l;
  hisnode *Found;
  char *curnode;
  curnode = GetNodeVarNameDisp(MyManager.manager, MyManager.varmap, Current);
  if (GetIndex(Current) < MyManager.varmap.varcnt) {
    printf("%s(%f,%i,%s)\n", curnode, MyManager.varmap.dvalue[GetIndex(Current)], MyManager.varmap.ivalue[GetIndex(Current)], MyManager.varmap.dynvalue[GetIndex(Current)]);
  } else {
    printf("%s\n", curnode);
  }
  if ((Current != MyManager.t) && (Current != MyManager.f) &&
      ((Found = GetNode(MyManager.his, MyManager.varmap.varstart, Current)) == NULL)) {
    l = LowNodeOf(MyManager.manager, Current);
    h = HighNodeOf(MyManager.manager, Current);
    printf("l(%s)->", curnode);
    DFS(MyManager, l);
    printf("h(%s)->", curnode);
    DFS(MyManager, h);
    AddNode(MyManager.his, MyManager.varmap.varstart, Current, 0.0, 0, NULL);
  }
}

void getalltruepaths(extmanager MyManager, DdNode *Current, const char *startpath, const char *prevvar) {
  DdNode *h, *l;
  char *curnode, *curpath;
  int pathmaxsize = 1024;
  curpath = (char *) malloc(sizeof(char) * pathmaxsize);
  curpath[0] = '\0';
  pathmaxsize = bufstrcat(curpath, pathmaxsize, startpath);
  pathmaxsize = bufstrcat(curpath, pathmaxsize, prevvar);
  pathmaxsize = bufstrcat(curpath, pathmaxsize, "*");
  curnode = GetNodeVarNameDisp(MyManager.manager, MyManager.varmap, Current);
  if (Current == MyManager.t) {
    printf("%s\n", curpath);
  } else if (Current != MyManager.f) {
    h = HighNodeOf(MyManager.manager, Current);
    if (h != MyManager.f) {
      getalltruepaths(MyManager, h, curpath, curnode);
    }
    l = LowNodeOf(MyManager.manager, Current);
    if (l != MyManager.f) {
      pathmaxsize = bufstrcat(curpath, pathmaxsize, "~");
      getalltruepaths(MyManager, l, curpath, curnode);
    }
  }
  free(curpath);
}

int bufstrcat(char *targetstr, int targetmem, const char *srcstr) {
  int strinc = strlen(srcstr), strsize = strlen(targetstr);
  while ((strsize + strinc) > (targetmem - 1)) {
    targetmem *= 2;
    targetstr = (char *) realloc(targetstr, sizeof(char) * targetmem);
  }
  strcat(targetstr, srcstr);
  return targetmem;
}
