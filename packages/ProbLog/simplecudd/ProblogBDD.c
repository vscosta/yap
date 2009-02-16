/******************************************************************************\
*                                                                              *
*    SimpleCUDD library (www.cs.kuleuven.be/~theo/tools/simplecudd.html)       *
*  SimpleCUDD was developed at Katholieke Universiteit Leuven(www.kuleuven.be) *
*                                                                              *
*  Copyright T. Mantadelis, A. Kimmig, B. Gutmann                              *
*            and Katholieke Universiteit Leuven 2008                           *
*                                                                              *
*  Author: Theofrastos Mantadelis, Angelika Kimmig, Bernd Gutmann              *
*  File: ProblogBDD.c                                                          *
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
#include <signal.h>

typedef struct _parameters {
  int loadfile;
  int savedfile;
  int exportfile;
  int inputfile;
  int debug;
  int errorcnt;
  int *error;
  int method;
  int queryid;
  int timeout;
  double sigmoid_slope;
  int online;
  int maxbufsize;
  char *ppid;
} parameters;

typedef struct _gradientpair {
  double probability;
  double gradient;
} gradientpair;

typedef struct _extmanager {
  DdManager *manager;
  DdNode *t, *f;
  hisqueue *his;
  namedvars varmap;
} extmanager;

int argtype(const char *arg);
void printhelp(int argc, char **arg);
parameters loadparam(int argc, char **arg);
parameters params;

void handler(int num);
void pidhandler(int num);
void termhandler(int num);

double sigmoid(double x, double slope);
void myexpand(extmanager MyManager, DdNode *Current);
double CalcProbability(extmanager MyManager, DdNode *Current);
double CalcProbabilitySigmoid(extmanager MyManager, DdNode *Current);
gradientpair CalcGradient(extmanager MyManager, DdNode *Current, int TargetVar, char *TargetPattern);
int patterncalculated(char *pattern, extmanager MyManager, int loc);
char * extractpattern(char *thestr);

int main(int argc, char **arg) {
  extmanager MyManager;
  DdNode *bdd;
  bddfileheader fileheader;
  int i, ivarcnt, code;
  gradientpair tvalue;
  double probability = -1.0;
  char *varpattern;
  varpattern = NULL;
  code = -1;
  params = loadparam(argc, arg);

  if (params.errorcnt > 0) {
    printhelp(argc, arg);
    for (i = 0; i < params.errorcnt; i++) {
      fprintf(stderr, "Error: not known or error at parameter %s.\n", arg[params.error[i]]);
    }
    return -1;
  }

  if (params.online == 0 && params.loadfile == -1) {
    printhelp(argc, arg);
    fprintf(stderr, "Error: you must specify a loading file.\n");
    return -1;
  }

  if (params.method != 0 && arg[params.method][0] != 'g' && arg[params.method][0] != 'p' && arg[params.method][0] != 'o') {
    printhelp(argc, arg);
    fprintf(stderr, "Error: you must choose a calculation method beetween [p]robability, [g]radient, [o]nline.\n");
    return -1;
  }

  if (params.debug) DEBUGON;
  RAPIDLOADON;
  SETMAXBUFSIZE(params.maxbufsize);

  signal(SIGINT, termhandler);
  if (params.ppid != NULL) {
    signal(SIGALRM, pidhandler);
    alarm(5);
  } else {
    signal(SIGALRM, handler);
    alarm(params.timeout);
  }
  if (params.online) {
    MyManager.manager = simpleBDDinit(0);
    MyManager.t = HIGH(MyManager.manager);
    MyManager.f = LOW(MyManager.manager);
    MyManager.varmap = InitNamedVars(1, 0);
    bdd = OnlineGenerateBDD(MyManager.manager, &MyManager.varmap);
    ivarcnt = GetVarCount(MyManager.manager);
  } else {
    fileheader = ReadFileHeader(arg[params.loadfile]);
    switch(fileheader.filetype) {
      case BDDFILE_SCRIPT:
        if (params.inputfile == -1) {
          printhelp(argc, arg);
          fprintf(stderr, "Error: an input file is necessary for this type of loading file.\n");
          return -1;
        }
        MyManager.manager = simpleBDDinit(fileheader.varcnt);
        MyManager.t = HIGH(MyManager.manager);
        MyManager.f = LOW(MyManager.manager);
        MyManager.varmap = InitNamedVars(fileheader.varcnt, fileheader.varstart);
        bdd = FileGenerateBDD(MyManager.manager, MyManager.varmap, fileheader);
        ivarcnt = fileheader.varcnt;
        break;
      case BDDFILE_NODEDUMP:
        if (params.inputfile == -1) {
          printhelp(argc, arg);
          fprintf(stderr, "Error: an input file is necessary for this type of loading file.\n");
          return -1;
        }
        MyManager.manager = simpleBDDinit(fileheader.varcnt);
        MyManager.t = HIGH(MyManager.manager);
        MyManager.f = LOW(MyManager.manager);
        MyManager.varmap = InitNamedVars(fileheader.varcnt, fileheader.varstart);
        bdd = LoadNodeDump(MyManager.manager, MyManager.varmap, fileheader.inputfile);
        ivarcnt = fileheader.varcnt;
        break;
      default:
        fprintf(stderr, "Error: not a valid file format to load.\n");
        return -1;
        break;
    }
  }
  alarm(0);

  // problem specifics

  if (bdd != NULL) {
    ivarcnt = RepairVarcnt(&MyManager.varmap);
    code = 0;
    if (params.inputfile != -1) {
      if (LoadVariableData(MyManager.varmap, arg[params.inputfile]) == -1) return -1;
      if (!all_loaded(MyManager.varmap, 1)) return -1;
    }
    MyManager.his = InitHistory(ivarcnt);
    if (params.method != 0) {
      switch(arg[params.method][0]) {
        case 'g':
          for (i = 0; i < MyManager.varmap.varcnt; i++) {
            if (MyManager.varmap.vars[i] != NULL) {
              varpattern = extractpattern(MyManager.varmap.vars[i]);
              if ((varpattern == NULL) || (!patterncalculated(varpattern, MyManager, i))) {
                tvalue = CalcGradient(MyManager, bdd, i + MyManager.varmap.varstart, varpattern);
                probability = tvalue.probability;
                double factor = sigmoid(MyManager.varmap.dvalue[i], params.sigmoid_slope) * (1 - sigmoid(MyManager.varmap.dvalue[i], params.sigmoid_slope)) * params.sigmoid_slope;
                if (varpattern == NULL) {
                  printf("query_gradient(%s,%s,%1.12f).\n", arg[params.queryid], MyManager.varmap.vars[i], tvalue.gradient * factor);
                } else {
                  varpattern[strlen(varpattern) - 2] = '\0';
                  printf("query_gradient(%s,%s,%1.12f).\n", arg[params.queryid], varpattern, tvalue.gradient * factor);
                }
                ReInitHistory(MyManager.his, MyManager.varmap.varcnt);
              }
              if (varpattern != NULL) free(varpattern);
            } else {
              fprintf(stderr, "Error: no variable name given for parameter.\n");
            }
          }
          if (probability < 0.0) {
            // no nodes, so we have to calculate probability ourself
            tvalue = CalcGradient(MyManager, bdd, 0 + MyManager.varmap.varstart, NULL);
            probability = tvalue.probability;
          }
          printf("query_probability(%s,%1.12f).\n", arg[params.queryid], probability);
          break;
        case 'p':
          printf("probability(%1.12f).\n", CalcProbability(MyManager, bdd));
          break;
        case 'o':
          onlinetraverse(MyManager.manager, MyManager.varmap, MyManager.his, bdd);
          break;
        default:
          myexpand(MyManager, bdd);
          break;
      }
    } else {
      myexpand(MyManager, bdd);
    }
    if (params.savedfile > -1) SaveNodeDump(MyManager.manager, MyManager.varmap, bdd, arg[params.savedfile]);
    if (params.exportfile > -1) simpleNamedBDDtoDot(MyManager.manager, MyManager.varmap, bdd, arg[params.exportfile]);
    ReInitHistory(MyManager.his, MyManager.varmap.varcnt);
    free(MyManager.his);
  }
  if (MyManager.manager != NULL) {
    KillBDD(MyManager.manager);
    free(MyManager.varmap.dvalue);
    free(MyManager.varmap.ivalue);
    free(MyManager.varmap.dynvalue);
    for (i = 0; i < MyManager.varmap.varcnt; i++)
      free(MyManager.varmap.vars[i]);
    free(MyManager.varmap.vars);
  }
  if (params.error != NULL) free(params.error);

  return code;

}

/* Shell Parameters handling */

int argtype(const char *arg) {
  if (strcmp(arg, "-l") == 0 || strcmp(arg, "--load") == 0) return 0;
  if (strcmp(arg, "-e") == 0 || strcmp(arg, "--export") == 0) return 2;
  if (strcmp(arg, "-m") == 0 || strcmp(arg, "--method") == 0) return 3;
  if (strcmp(arg, "-i") == 0 || strcmp(arg, "--input") == 0) return 4;
  if (strcmp(arg, "-h") == 0 || strcmp(arg, "--help") == 0) return 5;
  if (strcmp(arg, "-d") == 0 || strcmp(arg, "--debug") == 0) return 6;
  if (strcmp(arg, "-id") == 0 || strcmp(arg, "--queryid") == 0) return 7;
  if (strcmp(arg, "-t") == 0 || strcmp(arg, "--timeout") == 0) return 8;
  if (strcmp(arg, "-sd") == 0 || strcmp(arg, "--savedump") == 0) return 9;
  if (strcmp(arg, "-sl") == 0 || strcmp(arg, "--slope") == 0) return 10;
  if (strcmp(arg, "-o") == 0 || strcmp(arg, "--online") == 0) return 11;
  if (strcmp(arg, "-bs") == 0 || strcmp(arg, "--bufsize") == 0) return 12;
  if (strcmp(arg, "-pid") == 0 || strcmp(arg, "--pid") == 0) return 13;
  return -1;
}

void printhelp(int argc, char **arg) {
  fprintf(stderr, "\nUsage: %s -l [filename] -i [filename] -o (-s(d) [filename] -e [filename] -m [method] -id [queryid] -sl [double]) (-t [seconds] -d -h)\n", arg[0]);
  fprintf(stderr, "Generates and traverses a BDD\nMandatory parameters:\n");
  fprintf(stderr, "\t-l [filename]\t->\tfilename to load supports two formats:\n\t\t\t\t\t\t1. script with generation instructions\n\t\t\t\t\t\t2. node dump saved file\n");
  fprintf(stderr, "\t-i [filename]\t->\tfilename to input problem specifics (mandatory with file formats 1, 2)\n");
  fprintf(stderr, "\t-o\t\t->\tgenerates the BDD in online mode instead from a file can be used instead of -l\n");
  fprintf(stderr, "Optional parameters:\n");
  fprintf(stderr, "\t-sd [filename]\t->\tfilename to save generated BDD in node dump format (fast loading, traverse valid only)\n");
  fprintf(stderr, "\t-e [filename]\t->\tfilename to export generated BDD in dot format\n");
  fprintf(stderr, "\t-m [method]\t->\tthe calculation method to be used: none(default), [p]robability, [g]radient, [o]nline\n");
  fprintf(stderr, "\t-id [queryid]\t->\tthe queries identity name (used by gradient) default: %s\n", arg[0]);
  fprintf(stderr, "\t-sl [double]\t->\tthe sigmoid slope (used by gradient) default: 1.0\n");
  fprintf(stderr, "Extra parameters:\n");
  fprintf(stderr, "\t-t [seconds]\t->\tthe seconds (int) for BDD generation timeout default 0 = no timeout\n");
  fprintf(stderr, "\t-pid [pid]\t->\ta process id (int) to check for termination default 0 = no process to check works only under POSIX OS\n");
  fprintf(stderr, "\t-bs [bytes]\t->\tthe bytes (int) to use as a maximum buffer size to read files default 0 = no max\n");
  fprintf(stderr, "\t-d\t\t->\tRun in debug mode (gives extra messages in stderr)\n");
  fprintf(stderr, "\t-h\t\t->\tHelp (displays this message)\n\n");
  fprintf(stderr, "Example: %s -l testbdd -i input.txt -m g -id testbdd\n", arg[0]);
}

parameters loadparam(int argc, char **arg) {
  int i;
  parameters params;
  params.loadfile = -1;
  params.savedfile = -1;
  params.exportfile = -1;
  params.method = 0;
  params.inputfile = -1;
  params.debug = 0;
  params.errorcnt = 0;
  params.queryid = 0;
  params.timeout = 0;
  params.sigmoid_slope = 1.0;
  params.online = 0;
  params.maxbufsize = 0;
  params.ppid = NULL;
  params.error = (int *) malloc(argc * sizeof(int));
  for (i = 1; i < argc; i++) {
    switch(argtype(arg[i])) {
      case 0:
        if (argc > i + 1) {
          i++;
          params.loadfile = i;
        } else {
          params.error[params.errorcnt] = i;
          params.errorcnt++;
        }
        break;
      case 2:
        if (argc > i + 1) {
          i++;
          params.exportfile = i;
        } else {
          params.error[params.errorcnt] = i;
          params.errorcnt++;
        }
        break;
      case 3:
        if (argc > i + 1) {
          i++;
          params.method = i;
        } else {
          params.error[params.errorcnt] = i;
          params.errorcnt++;
        }
        break;
      case 4:
        if (argc > i + 1) {
          i++;
          params.inputfile = i;
        } else {
          params.error[params.errorcnt] = i;
          params.errorcnt++;
        }
        break;
      case 5:
        printhelp(argc, arg);
        break;
      case 6:
        params.debug = 1;
        break;
      case 7:
        if (argc > i + 1) {
          i++;
          params.queryid = i;
        } else {
          params.error[params.errorcnt] = i;
          params.errorcnt++;
        }
        break;
      case 8:
        if ((argc > i + 1) && (IsPosNumber(arg[i + 1]))) {
          i++;
          params.timeout = atoi(arg[i]);
        } else {
          params.error[params.errorcnt] = i;
          params.errorcnt++;
        }
        break;
      case 9:
        if (argc > i + 1) {
          i++;
          params.savedfile = i;
        } else {
          params.error[params.errorcnt] = i;
          params.errorcnt++;
        }
        break;
      case 10:
        if ((argc > i + 1) && (IsRealNumber(arg[i + 1]))) {
          i++;
          params.sigmoid_slope = atof(arg[i]);
        } else {
          params.error[params.errorcnt] = i;
          params.errorcnt++;
        }
        break;
      case 11:
        params.online = 1;
        break;
      case 12:
        if ((argc > i + 1) && (IsPosNumber(arg[i + 1]))) {
          i++;
          params.maxbufsize = atoi(arg[i]);
        } else {
          params.error[params.errorcnt] = i;
          params.errorcnt++;
        }
        break;
      case 13:
        if ((argc > i + 1) && (IsPosNumber(arg[i + 1]))) {
          i++;
          params.ppid = (char *) malloc(sizeof(char) * (strlen(arg[i]) + 1));
          strcpy(params.ppid, arg[i]);
        } else {
          params.error[params.errorcnt] = i;
          params.errorcnt++;
        }
        break;
      default:
        params.error[params.errorcnt] = i;
        params.errorcnt++;
        break;
    }
  }
  return params;
}

/* Error Handlers */

void handler(int num) {
  fprintf(stderr, "Error: Timeout %i exceeded.\n", params.timeout);
  exit(-1);
}

void pidhandler(int num) {
  char *s;
  if (params.timeout > 0) {
    params.timeout -= 5;
    if (params.timeout <= 0) {
      fprintf(stderr, "Error: Timeout exceeded.\n");
      exit(-1);
    }
  }
  s = (char *) malloc(sizeof(char) * (19 + strlen(params.ppid)));
  strcpy(s, "ps "); strcat(s, params.ppid); strcat(s, " >/dev/null");
  if (system(s) != 0) exit(4);
  signal(SIGALRM, pidhandler);
  alarm(5);
  free(s);
}

void termhandler(int num) {
  exit(3);
}

/* General Functions */

double sigmoid(double x, double slope) {
  return 1 / (1 + exp(-x * slope));
}

/* Debugging traverse function */

void myexpand(extmanager MyManager, DdNode *Current) {
  DdNode *h, *l;
  hisnode *Found;
  char *curnode;
  curnode = GetNodeVarNameDisp(MyManager.manager, MyManager.varmap, Current);
  printf("%s\n", curnode);
  if ((Current != MyManager.t) && (Current != MyManager.f) &&
      ((Found = GetNode(MyManager.his, MyManager.varmap.varstart, Current)) == NULL)) {
    l = LowNodeOf(MyManager.manager, Current);
    h = HighNodeOf(MyManager.manager, Current);
    printf("l(%s)->", curnode);
    myexpand(MyManager, l);
    printf("h(%s)->", curnode);
    myexpand(MyManager, h);
    AddNode(MyManager.his, MyManager.varmap.varstart, Current, 0.0, 0, NULL);
  }
}

/* Angelikas Algorithm */

double CalcProbability(extmanager MyManager, DdNode *Current) {
  DdNode *h, *l;
  hisnode *Found;
  char *curnode;
  double lvalue, hvalue, tvalue;
  if (params.debug) {
    curnode = GetNodeVarNameDisp(MyManager.manager, MyManager.varmap, Current);
    fprintf(stderr, "%s\n", curnode);
  }
  if (Current == MyManager.t) return 1.0;
  if (Current == MyManager.f) return 0.0;
  if ((Found = GetNode(MyManager.his, MyManager.varmap.varstart, Current)) != NULL) return Found->dvalue;
  l = LowNodeOf(MyManager.manager, Current);
  h = HighNodeOf(MyManager.manager, Current);
  if (params.debug) fprintf(stderr, "l(%s)->", curnode);
  lvalue = CalcProbability(MyManager, l);
  if (params.debug) fprintf(stderr, "h(%s)->", curnode);
  hvalue = CalcProbability(MyManager, h);
  tvalue = MyManager.varmap.dvalue[GetIndex(Current) - MyManager.varmap.varstart];
  tvalue = tvalue * hvalue + lvalue * (1.0 - tvalue);
  AddNode(MyManager.his, MyManager.varmap.varstart, Current, tvalue, 0, NULL);
  return tvalue;
}

/* Bernds Algorithm */

gradientpair CalcGradient(extmanager MyManager, DdNode *Current, int TargetVar, char *TargetPattern) {
  DdNode *h, *l;
  hisnode *Found;
  char *curnode;
  gradientpair lvalue, hvalue, tvalue;
  double this_probability;
  double *gradient;
  if (params.debug) {
    curnode = GetNodeVarNameDisp(MyManager.manager, MyManager.varmap, Current);
    fprintf(stderr, "%s\n", curnode);
  }
  if (Current == MyManager.t) {
    tvalue.probability = 1.0;
    tvalue.gradient = 0.0;
    return tvalue;
  }
  if (Current == MyManager.f) {
    tvalue.probability = 0.0;
    tvalue.gradient = 0.0;
    return tvalue;
  }
  if ((Found = GetNode(MyManager.his, MyManager.varmap.varstart, Current)) != NULL) {
    tvalue.probability = Found->dvalue;
    tvalue.gradient = *((double *) Found->dynvalue);
    return tvalue;
  }
  l = LowNodeOf(MyManager.manager, Current);
  h = HighNodeOf(MyManager.manager, Current);
  if (params.debug) fprintf(stderr, "l(%s)->", curnode);
  lvalue = CalcGradient(MyManager, l, TargetVar, TargetPattern);
  if (params.debug) fprintf(stderr, "h(%s)->", curnode);
  hvalue = CalcGradient(MyManager, h, TargetVar, TargetPattern);
  this_probability = sigmoid(MyManager.varmap.dvalue[GetIndex(Current) - MyManager.varmap.varstart], params.sigmoid_slope);
  tvalue.probability = this_probability * hvalue.probability + (1 - this_probability) * lvalue.probability;
  tvalue.gradient = this_probability * hvalue.gradient + (1 - this_probability) * lvalue.gradient;
  if ((GetIndex(Current) == TargetVar) ||
      ((TargetPattern != NULL) && patternmatch(TargetPattern, MyManager.varmap.vars[GetIndex(Current)]))) {
    tvalue.gradient += hvalue.probability - lvalue.probability;
  }
  gradient = (double *) malloc(sizeof(double));
  *gradient = tvalue.gradient;
  AddNode(MyManager.his, MyManager.varmap.varstart, Current, tvalue.probability, 0, gradient);
  return tvalue;
}

char * extractpattern(char *thestr) {
  char *p;
  int i = 0, sl = strlen(thestr);
  while((thestr[i] != '_') && (i < sl)) i++;
  if (i == sl) return NULL;
  i++;
  p = (char *) malloc(sizeof(char) * (i + 2));
  strncpy(p, thestr, i);
  p[i] = '*';
  p[i + 1] = '\0';
  return p;
}

int patterncalculated(char *pattern, extmanager MyManager, int loc) {
  int i;
  if (pattern == NULL) return 0;
  for (i = loc - 1; i > -1; i--)
    if (patternmatch(pattern, MyManager.varmap.vars[i])) return 1;
  return 0;
}
