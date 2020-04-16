/******************************************************************************\
*                                                                              *
*    SimpleCUDD library (www.cs.kuleuven.be/~theo/tools/simplecudd.html)       *
*  SimpleCUDD was developed at Katholieke Universiteit Leuven(www.kuleuven.be) *
*                                                                              *
*  Copyright Katholieke Universiteit Leuven 2008                               *
*                                                                              *
*  Author: Theofrastos Mantadelis, Angelika Kimmig, Bernd Gutmann              *
*  File: ProblogBDD.c                                                          *
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

#include "simplecudd.h"
#include "problogmath.h"
#include "pqueue.h"
#include "iqueue.h"
#include <signal.h>
#include <stdarg.h>
#define VERSION "2.0.0"

int all_loaded_for_deterministic_variables(namedvars varmap, int disp);

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
  int orderfile;
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

void myexpand(extmanager MyManager, DdNode *Current);
double CalcProbability(extmanager MyManager, DdNode *Current);
double CalcProbabilitySigmoid(extmanager MyManager, DdNode *Current);
gradientpair CalcGradient(extmanager MyManager, DdNode *Current, int TargetVar,
                          char *TargetPattern, int type);
double CalcExpectedCountsUp(extmanager *MyManager, DdNode *Current,
                            char *query_id);
double CalcExpectedCountsDown(extmanager *MyManager, DdNode *Current,
                              char *query_id);
double CalcExpectedCounts(extmanager *MyManager, DdNode *Current,
                          char *query_id, int calcdown_needed);
int patterncalculated(char *pattern, extmanager MyManager, int loc);
char *extractpattern(const char *thestr);

int main(int argc, char **arg) {
  extmanager MyManager;
  DdNode *bdd = NULL, **forest = NULL, *bakbdd = NULL;
  bddfileheader fileheader;
  int i, ivarcnt, code, curbdd;
  gradientpair tvalue;
  double probability = -1.0;
  char *varpattern;
  varpattern = NULL;
  code = -1;
  params = loadparam(argc, arg);

  if (params.errorcnt > 0) {
    printhelp(argc, arg);
    for (i = 0; i < params.errorcnt; i++) {
      fprintf(stderr, "Error: not known or error at parameter %s.\n",
              arg[params.error[i]]);
    }
    return -1;
  }

  if (params.online == 0 && params.loadfile == -1) {
    printhelp(argc, arg);
    fprintf(stderr, "Error: you must specify a loading file.\n");
    return -1;
  }

  if (params.method != 0 && arg[params.method][0] != 'g' &&
      arg[params.method][0] != 'p' && arg[params.method][0] != 'o' &&
      arg[params.method][0] != 'l' && arg[params.method][0] != 'e' &&
      arg[params.method][0] != 'd') {
    printhelp(argc, arg);
    fprintf(stderr, "Error: you must choose a calculation method beetween "
                    "[p]robability, [g]radient, [l]ine search, [o]nline, "
                    "[e]xpected counts, probability with [d]eterministic "
                    "nodes.\n");
    return -1;
  }

  if (params.method != 0 &&
      (arg[params.method][0] == 'g' || arg[params.method][0] == 'p' ||
       arg[params.method][0] == 'l' || arg[params.method][0] == 'e' ||
       arg[params.method][0] == 'd') &&
      params.inputfile == -1) {
    printhelp(argc, arg);
    fprintf(stderr, "Error: an input file is necessary for probability, "
                    "gradient, line search calculation or expected counts "
                    "methods.\n");
    return -1;
  }

  if (params.debug)
    DEBUGON;
  RAPIDLOADON;
  SETMAXBUFSIZE(params.maxbufsize);

  signal(SIGINT, termhandler);
#ifndef __MINGW32__
  if (params.ppid != NULL) {
    signal(SIGALRM, pidhandler);
    alarm(5);
  } else {
    signal(SIGALRM, handler);
    alarm(params.timeout);
  }
#endif

  if (params.online) {
    MyManager.manager = simpleBDDinit(0);
    MyManager.t = HIGH(MyManager.manager);
    MyManager.f = LOW(MyManager.manager);
    MyManager.varmap = InitNamedVars(1, 0);
    bdd = OnlineGenerateBDD(MyManager.manager, &MyManager.varmap);
    ivarcnt = GetVarCount(MyManager.manager);
  } else {
    // fprintf(stderr,"reading file \n");
    fileheader = ReadFileHeader(arg[params.loadfile]);
    switch (fileheader.filetype) {
    case BDDFILE_SCRIPT:
      //	fprintf(stderr," ..... %i \n",fileheader.varcnt);
      MyManager.manager = simpleBDDinit(fileheader.varcnt);
      MyManager.t = HIGH(MyManager.manager);
      MyManager.f = LOW(MyManager.manager);
      MyManager.varmap = InitNamedVars(fileheader.varcnt, fileheader.varstart);
      if (fileheader.version > 1) {
        forest = FileGenerateBDDForest(MyManager.manager, MyManager.varmap,
                                       fileheader);
        bdd = forest[0];
        bakbdd = bdd;
      } else {
        forest = NULL;
        bdd = FileGenerateBDD(MyManager.manager, MyManager.varmap, fileheader);
        bakbdd = bdd;
      }
      ivarcnt = fileheader.varcnt;
      break;
    case BDDFILE_NODEDUMP:
      MyManager.manager = simpleBDDinit(fileheader.varcnt);
      MyManager.t = HIGH(MyManager.manager);
      MyManager.f = LOW(MyManager.manager);
      MyManager.varmap = InitNamedVars(fileheader.varcnt, fileheader.varstart);
      bdd = LoadNodeDump(MyManager.manager, MyManager.varmap,
                         fileheader.inputfile);
      ivarcnt = fileheader.varcnt;
      break;
    default:
      fprintf(stderr, "Error: not a valid file format to load.\n");
      return -1;
      break;
    }
    //    fprintf(stderr,"bdd built\n");
  }

  alarm(0);

  // problem specifics

  if (bdd != NULL) {
    ivarcnt = RepairVarcnt(&MyManager.varmap);
    code = 0;
    if (params.inputfile != -1) {
      if (LoadVariableData(MyManager.varmap, arg[params.inputfile]) == -1)
        return -1;
      //     if (!all_loaded(MyManager.varmap, 1)) return -1;
      all_loaded_for_deterministic_variables(MyManager.varmap, 1);
    }
    // impose a predifined order good for debugging
    // can be used with a partial number of variables to impose ordering at
    // beggining of BDD
    if (params.orderfile != -1) {
      ImposeOrder(
          MyManager.manager, MyManager.varmap,
          GetVariableOrder(arg[params.orderfile], MyManager.varmap.varcnt));
    }
    curbdd = 0;
    do {
      MyManager.his = InitHistory(ivarcnt);
      if (params.method != 0) {
        switch (arg[params.method][0]) {
        case 'g':
          for (i = 0; i < MyManager.varmap.varcnt; i++) {
            if (MyManager.varmap.vars[i] != NULL) {

              // check whether this is a continues fact
              if (MyManager.varmap.dynvalue[i] == NULL) { // nope, regular fact
                varpattern = extractpattern(MyManager.varmap.vars[i]);
                if ((varpattern == NULL) ||
                    (!patterncalculated(varpattern, MyManager, i))) {
                  tvalue = CalcGradient(MyManager, bdd,
                                        i + MyManager.varmap.varstart,
                                        varpattern, 0);
                  probability = tvalue.probability;
                  if (varpattern == NULL) {
                    printf("query_gradient(%s,%s,p,%e).\n", arg[params.queryid],
                           MyManager.varmap.vars[i], tvalue.gradient);
                  } else {
                    varpattern[strlen(varpattern) - 2] = '\0';
                    printf("query_gradient(%s,%s,p,%e).\n", arg[params.queryid],
                           varpattern, tvalue.gradient);
                  }
                  ReInitHistory(MyManager.his, MyManager.varmap.varcnt);
                  if (varpattern != NULL)
                    free(varpattern);
                }
              } else { // it is! let's do the Hybrid Problog Magic
                // first for mu
                varpattern = extractpattern(MyManager.varmap.vars[i]);
                if ((varpattern == NULL) ||
                    (!patterncalculated(varpattern, MyManager, i))) {
                  tvalue = CalcGradient(MyManager, bdd,
                                        i + MyManager.varmap.varstart,
                                        varpattern, 1);
                  probability = tvalue.probability;
                  if (varpattern == NULL) {
                    printf("query_gradient(%s,%s,mu,%e).\n",
                           arg[params.queryid], MyManager.varmap.vars[i],
                           tvalue.gradient);
                  } else {
                    varpattern[strlen(varpattern) - 2] = '\0';
                    printf("query_gradient(%s,%s,mu,%e).\n",
                           arg[params.queryid], varpattern, tvalue.gradient);
                  }
                }
                ReInitHistory(MyManager.his, MyManager.varmap.varcnt);
                if (varpattern != NULL)
                  free(varpattern);

                // then for sigma
                varpattern = extractpattern(MyManager.varmap.vars[i]);
                if ((varpattern == NULL) ||
                    (!patterncalculated(varpattern, MyManager, i))) {
                  tvalue = CalcGradient(MyManager, bdd,
                                        i + MyManager.varmap.varstart,
                                        varpattern, 2);
                  probability = tvalue.probability;
                  if (varpattern == NULL) {
                    printf("query_gradient(%s,%s,sigma,%e).\n",
                           arg[params.queryid], MyManager.varmap.vars[i],
                           tvalue.gradient);
                  } else {
                    varpattern[strlen(varpattern) - 2] = '\0';
                    printf("query_gradient(%s,%s,sigma,%e).\n",
                           arg[params.queryid], varpattern, tvalue.gradient);
                  }
                }
                ReInitHistory(MyManager.his, MyManager.varmap.varcnt);
                if (varpattern != NULL)
                  free(varpattern);
              }

            } else {
              fprintf(stderr, "Error: no variable name given for parameter.\n");
            }
          }
          if (probability < 0.0) {
            // no nodes, so we have to calculate probability ourself
            tvalue = CalcGradient(MyManager, bdd, 0 + MyManager.varmap.varstart,
                                  NULL, 0);
            probability = tvalue.probability;
          }
          printf("query_probability(%s,%e).\n", arg[params.queryid],
                 probability);
          break;
        case 'l':
          tvalue = CalcGradient(MyManager, bdd, 0 + MyManager.varmap.varstart,
                                NULL, 0);
          probability = tvalue.probability;
          printf("query_probability(%s,%e).\n", arg[params.queryid],
                 probability);
          break;
        case 'e':
          // fprintf(stderr,"start calc exp count\n");
          printf("query_probability(%s,%30.30e).\n", arg[params.queryid],
                 CalcExpectedCounts(&MyManager, bdd, arg[params.queryid], 1));
          break;
        case 'd':
          // fprintf(stderr,"start calc exp count\n");
          printf("query_probability(%s,%30.30e).\n", arg[params.queryid],
                 CalcExpectedCounts(&MyManager, bdd, arg[params.queryid], 0));
          break;
        case 'p':
          printf("query_probability(%s,%e).\n", arg[params.queryid],
                 CalcProbability(MyManager, bdd));
          break;
        case 'o':
          onlinetraverse(MyManager.manager, MyManager.varmap, MyManager.his,
                         bdd);
          break;
        default:
          myexpand(MyManager, bdd);
          break;
        }
      } else {
        myexpand(MyManager, bdd);
      }
      if (forest != NULL) {
        curbdd++;
        bdd = forest[curbdd];
      } else {
        bdd = NULL;
      }
      ReInitHistory(MyManager.his, MyManager.varmap.varcnt);
    } while (bdd != NULL);
    bdd = bakbdd;
    if (params.savedfile > -1)
      SaveNodeDump(MyManager.manager, MyManager.varmap, bdd,
                   arg[params.savedfile]);
    if (params.exportfile > -1)
      simpleNamedBDDtoDot(MyManager.manager, MyManager.varmap, bdd,
                          arg[params.exportfile]);
    free(MyManager.his);
  }
  if (MyManager.manager != NULL) {
    KillBDD(MyManager.manager);
    free(MyManager.varmap.dvalue);
    free(MyManager.varmap.ivalue);
    if (MyManager.varmap.dynvalue != NULL) {
      for (i = 0; i < MyManager.varmap.varcnt; i++)
        if (MyManager.varmap.dynvalue[i] != NULL) {
          free(MyManager.varmap.dynvalue[i]);
        }
      free(MyManager.varmap.dynvalue);
    }
    for (i = 0; i < MyManager.varmap.varcnt; i++)
      free((void *)MyManager.varmap.vars[i]);
    free(MyManager.varmap.vars);
  }
  if (params.error != NULL)
    free(params.error);

  return code;
}

/* Shell Parameters handling */

int argtype(const char *arg) {
  if (strcmp(arg, "-l") == 0 || strcmp(arg, "--load") == 0)
    return 0;
  if (strcmp(arg, "-e") == 0 || strcmp(arg, "--export") == 0)
    return 2;
  if (strcmp(arg, "-m") == 0 || strcmp(arg, "--method") == 0)
    return 3;
  if (strcmp(arg, "-i") == 0 || strcmp(arg, "--input") == 0)
    return 4;
  if (strcmp(arg, "-h") == 0 || strcmp(arg, "--help") == 0)
    return 5;
  if (strcmp(arg, "-d") == 0 || strcmp(arg, "--debug") == 0)
    return 6;
  if (strcmp(arg, "-id") == 0 || strcmp(arg, "--queryid") == 0)
    return 7;
  if (strcmp(arg, "-t") == 0 || strcmp(arg, "--timeout") == 0)
    return 8;
  if (strcmp(arg, "-sd") == 0 || strcmp(arg, "--savedump") == 0)
    return 9;
  if (strcmp(arg, "-sl") == 0 || strcmp(arg, "--slope") == 0)
    return 10;
  if (strcmp(arg, "-o") == 0 || strcmp(arg, "--online") == 0)
    return 11;
  if (strcmp(arg, "-bs") == 0 || strcmp(arg, "--bufsize") == 0)
    return 12;
  if (strcmp(arg, "-pid") == 0 || strcmp(arg, "--pid") == 0)
    return 13;
  if (strcmp(arg, "-ord") == 0 || strcmp(arg, "--order") == 0)
    return 14;

  return -1;
}

void printhelp(int argc, char **arg) {
  fprintf(stderr, "\n\nProbLogBDD Tool Version: %s\n\n", VERSION);
  fprintf(
      stderr,
      "SimpleCUDD library (www.cs.kuleuven.be/~theo/tools/simplecudd.html)\n");
  fprintf(stderr, "SimpleCUDD was developed at Katholieke Universiteit "
                  "Leuven(www.kuleuven.be)\n");
  fprintf(stderr, "Copyright Katholieke Universiteit Leuven 2008\n");
  fprintf(stderr,
          "Authors: Theofrastos Mantadelis, Angelika Kimmig, Bernd Gutmann\n");
  fprintf(stderr, "This package falls under the: Artistic License 2.0\n");
  fprintf(stderr, "\nUsage: %s -l [filename] -i [filename] -o (-s(d) "
                  "[filename] -e [filename] -m [method] -id [queryid] -sl "
                  "[double]) (-t [seconds] -d -h)\n",
          arg[0]);
  fprintf(stderr, "Generates and traverses a BDD\nMandatory parameters:\n");
  fprintf(stderr, "\t-l [filename]\t->\tfilename to load supports two "
                  "formats:\n\t\t\t\t\t\t1. script with generation "
                  "instructions\n\t\t\t\t\t\t2. node dump saved file\n");
  fprintf(stderr, "\t-i [filename]\t->\tfilename to input problem specifics "
                  "(mandatory with file formats 1, 2)\n");
  fprintf(stderr, "\t-o\t\t->\tgenerates the BDD in online mode instead from a "
                  "file can be used instead of -l\n");
  fprintf(stderr, "Optional parameters:\n");
  fprintf(stderr, "\t-sd [filename]\t->\tfilename to save generated BDD in "
                  "node dump format (fast loading, traverse valid only)\n");
  fprintf(
      stderr,
      "\t-e [filename]\t->\tfilename to export generated BDD in dot format\n");
  fprintf(stderr, "\t-m [method]\t->\tthe calculation method to be used: "
                  "none(default), [p]robability, [g]radient, [l]ine search, "
                  "[o]nline, [e]xpexted counts, prob. with [d]eterministic "
                  "nodes\n");
  fprintf(stderr, "\t-id [queryid]\t->\tthe queries identity name (used by "
                  "gradient) default: %s\n",
          arg[0]);
  fprintf(stderr, "\t-sl [double]\t->\tthe sigmoid slope (used by gradient) "
                  "default: 1.0\n");
  fprintf(stderr, "Extra parameters:\n");
  fprintf(stderr, "\t-t [seconds]\t->\tthe seconds (int) for BDD generation "
                  "timeout default 0 = no timeout\n");
  fprintf(stderr, "\t-pid [pid]\t->\ta process id (int) to check for "
                  "termination default 0 = no process to check\n");
  fprintf(stderr, "\t-bs [bytes]\t->\tthe bytes (int) to use as a maximum "
                  "buffer size to read files default 0 = no max\n");
  fprintf(stderr, "\t-ord [filename]\t->\tUse the [filename] to define a "
                  "specific BDD variable order\n");
  fprintf(stderr,
          "\t-d\t\t->\tRun in debug mode (gives extra messages in stderr)\n");
  fprintf(stderr, "\t-h\t\t->\tHelp (displays this message)\n");
  fprintf(stderr, "Extra notes:\nSupports a forest of BDDs in one shared "
                  "BDD.\nSelected computational methods will be applied to "
                  "each BDD seperately.\nFile operations will be applied only "
                  "to the first BDD.\n");
  fprintf(stderr, "\nExample: %s -l testbdd -i input.txt -m g -id testbdd\n",
          arg[0]);
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
  params.orderfile = -1;
  params.error = (int *)malloc(argc * sizeof(int));
  for (i = 1; i < argc; i++) {
    switch (argtype(arg[i])) {
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
        params.ppid = (char *)malloc(sizeof(char) * (strlen(arg[i]) + 1));
        strcpy(params.ppid, arg[i]);
      } else {
        params.error[params.errorcnt] = i;
        params.errorcnt++;
      }
      break;
    case 14:
      if (argc > i + 1) {
        i++;
        params.orderfile = i;
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
  s = (char *)malloc(sizeof(char) * (19 + strlen(params.ppid)));
  strcpy(s, "ps ");
  strcat(s, params.ppid);
  strcat(s, " >/dev/null");
  if (system(s) != 0)
    exit(4);
#ifndef __MINGW32__
  signal(SIGALRM, pidhandler);
#endif
  alarm(5);
  free(s);
}

void termhandler(int num) { exit(3); }

/* Debugging traverse function */

void myexpand(extmanager MyManager, DdNode *Current) {
  DdNode *h, *l;
  hisnode *Found;
  const char *curnode;
  curnode = GetNodeVarNameDisp(MyManager.manager, MyManager.varmap, Current);
  printf("%s\n", curnode);
  if ((Current != MyManager.t) && (Current != MyManager.f) &&
      ((Found = GetNode(MyManager.his, MyManager.varmap.varstart, Current)) ==
       NULL)) {
    l = LowNodeOf(MyManager.manager, Current);
    h = HighNodeOf(MyManager.manager, Current);
    printf("l(%s)->", curnode);
    myexpand(MyManager, l);
    printf("h(%s)->", curnode);
    myexpand(MyManager, h);
    AddNode(MyManager.his, MyManager.varmap.varstart, Current, 0.0, 0, NULL);
  }
}

/* Angelika's Algorithm */

double CalcProbability(extmanager MyManager, DdNode *Current) {
  DdNode *h, *l;
  hisnode *Found = NULL;
  const char *curnode; //, *dynvalue;
  double lvalue, hvalue, tvalue;
  // density_integral dynvalue_parsed;

  if (params.debug) {
    curnode = GetNodeVarNameDisp(MyManager.manager, MyManager.varmap, Current);
    fprintf(stderr, "%s\n", curnode);
  } else {
    // gcc stupidly complains.
    curnode = NULL;
  }
  if (Current == MyManager.t)
    return 1.0;
  if (Current == MyManager.f)
    return 0.0;

  if ((Found = GetNode(MyManager.his, MyManager.varmap.varstart, Current)) !=
      NULL)
    return Found->dvalue;
  l = LowNodeOf(MyManager.manager, Current);
  h = HighNodeOf(MyManager.manager, Current);
  if (params.debug)
    fprintf(stderr, "l(%s)->", curnode);
  lvalue = CalcProbability(MyManager, l);
  if (params.debug)
    fprintf(stderr, "h(%s)->", curnode);
  hvalue = CalcProbability(MyManager, h);

  tvalue =
      MyManager.varmap.dvalue[GetIndex(Current) - MyManager.varmap.varstart];

  tvalue = tvalue * hvalue + lvalue * (1.0 - tvalue);
  AddNode(MyManager.his, MyManager.varmap.varstart, Current, tvalue, 0, NULL);
  return tvalue;
}

double CalcExpectedCounts(extmanager *MyManager, DdNode *Current,
                          char *query_id, int calcdown_needed) {

  // fprintf(stderr,"%%calcing up\n");
  double ret = CalcExpectedCountsUp(MyManager, Current, query_id);
  //   fprintf(stderr,"%%result is %e\n",ret);
  // fprintf(stderr,"%%calcing down\n");

  if (calcdown_needed != 0) {
    // double retd=CalcExpectedCountsDown(MyManager,Current, query_id);
  }
  /*   if(1 != retd){ */
  /*     fprintf(stderr,"down %e != up %e/%e\n",ret,retd,ret); */
  /*     exit(1); */
  /*   } */
  return ret;
}

/* ComparisonFunction compare_nodes (  extmanager MyManager) */
/* { */
/*   //fprintf(stderr,"creating comparator for  %p\n",MyManager); */

/*   int comparator(void *av, void *bv){ */
/*     fprintf(stderr,"========================> %p \n",MyManager); */
/*     fprintf(stderr,"xxxxx \n"); */
/*     DdNode* a = (DdNode*)av; */
/*     DdNode* b = (DdNode*)bv; */
/*     extmanager MyManager; */
/*     int aindex,bindex, aperm, bperm; */
/*     aindex=GetIndex(a); */
/*     bindex=GetIndex(b); */
/*     aperm=Cudd_IsConstant(a) ? CUDD_CONST_INDEX :
 * Cudd_ReadPerm(MyManager.manager,aindex); */
/*     Cudd_ReadPerm(MyManager.manager,bindex); */
/*     bperm=Cudd_IsConstant(b) ? CUDD_CONST_INDEX :
 * Cudd_ReadPerm(MyManager.manager,bindex); */
/*     int temp = aperm-bperm; */
/* //-Cudd_ReadPerm(MyManager.manager,(*b).index);//-Cudd_ReadPerm(MyManager,b);//
 * - Cudd_ReadPerm(b); */
/*     //fprintf(stderr,"comparing3 %p %p %p\n",a,b,MyManager); */
/*     //  return -1; */
/*     if (temp < 0) */
/*       return 1; */
/*     else if (temp > 0) */
/*       return -1; */
/*     //    else //never return zero otherwise one is pruned away, or(?) */
/*     //      return 0; */
/*   } */

/*   //  return (a<b) ? 1 : -1; */
/*   return *comparator; */
/* } */

#define NODE_VALUE 1001
#define LOG_EXPECTED 0

static void PrintNodeQueue(Queue q, extmanager MyManager) {

  QueueIterator qiter = QueueIteratorNew(q, 1);
  fprintf(stderr, "Queue %p is [", q);

  while (qiter->currentItem != NULL) {
    DdNode *val = (DdNode *)qiter->currentItem->element;
    QueueIteratorAdvance(qiter);
    fprintf(stderr, " %s %s",
            GetNodeVarNameDisp(MyManager.manager, MyManager.varmap, val),
            (qiter->currentItem != NULL) ? "," : "]\n");
  }
}
/** also nesting in CalcExpected seems to not work (must be here nested only
 * valid within function frame)*/
/* will be changed at later stage */
static extmanager *ineedtostorethatsomehow;

static int comparator(void *av, void *bv) {
  int ret = 0;
  DdNode *a = (DdNode *)av;
  DdNode *b = (DdNode *)bv;
  int aindex, bindex, aperm, bperm;
  aindex = GetIndex(a);
  bindex = GetIndex(b);
  aperm = Cudd_IsConstant(a)
              ? CUDD_CONST_INDEX
              : Cudd_ReadPerm(ineedtostorethatsomehow->manager, aindex);
  Cudd_ReadPerm(ineedtostorethatsomehow->manager, bindex);
  bperm = Cudd_IsConstant(b)
              ? CUDD_CONST_INDEX
              : Cudd_ReadPerm(ineedtostorethatsomehow->manager, bindex);
  int temp = -aperm + bperm;
  if (temp < 0)
    ret = 1;
  else if (temp > 0)
    ret = -1;
  //    else //never return zero otherwise one is pruned away, or(?)
  //      return 0;
  if (LOG_EXPECTED) {
    fprintf(stderr, "perm(%s,%i)=%i perm(%s,%i)=%i => %i\n",
            GetNodeVarNameDisp(ineedtostorethatsomehow->manager,
                               ineedtostorethatsomehow->varmap, a),
            aindex, aperm,
            GetNodeVarNameDisp(ineedtostorethatsomehow->manager,
                               ineedtostorethatsomehow->varmap, b),
            bindex, bperm, ret);
  }

  return ret;
}

static void skip_nodes_cnt(extmanager *MyManager, double (*counts)[],
                           int skipcnt, DdNode *l, double dprob,
                           char *query_id);

/** output information for skipped nodes **/
static void skip_nodes(extmanager *MyManager, double (*counts)[], DdNode *node,
                       DdNode *l, double dprob, char *query_id) {
  int skipcnt;
  skipcnt = Cudd_ReadPerm(MyManager->manager, GetIndex(node)) + 1;
  if (LOG_EXPECTED) {
    fprintf(stderr, ">> skipper >> %s=%i@%i of %i -> %i@%i %i\n",
            (char *)(MyManager->varmap.dynvalue[GetIndex(node) -
                                                MyManager->varmap.varstart]),
            GetIndex(node), Cudd_ReadPerm(MyManager->manager, GetIndex(node)),
            Cudd_ReadSize(MyManager->manager), GetIndex(l),
            Cudd_ReadPerm(MyManager->manager, GetIndex(l)), Cudd_IsConstant(l));
  }
  skip_nodes_cnt(MyManager, counts, skipcnt, l, dprob, query_id);
}

static void skip_nodes_cnt(extmanager *MyManager, double (*counts)[],
                           int skipcnt, DdNode *l, double dprob,
                           char *query_id) {
  if (LOG_EXPECTED)
    fprintf(stderr, "====================\n");
  double p;
  int ivalue;
  //  fprintf(stderr, " skip (:%i) \n",__LINE__);
  while (Cudd_IsConstant(l)
             ? skipcnt < Cudd_ReadSize(MyManager->manager) // the
             // terminals/leafs/constants
             // will be ignored
             : skipcnt < Cudd_ReadPerm(MyManager->manager, GetIndex(l))) {
    skipcnt++;
    if (LOG_EXPECTED) {
      fprintf(stderr, "skipcnt %i\n", skipcnt - 1);
    }
    int idx = Cudd_ReadInvPerm(MyManager->manager, skipcnt - 1);
    if (LOG_EXPECTED) {
      fprintf(stderr, "index %i %i\n", idx, MyManager->varmap.varstart);
    }
    // fprintf(stdout,"%i
    // %s.\n",skipcnt,MyManager->varmap.dynvalue[GetIndex(node) -
    // MyManager->varmap.varstart]);
    if (LOG_EXPECTED) {
      fprintf(stderr,
              "Node skipped level %i index: %i name: %s (dprob is %e)\n",
              skipcnt, idx,
              MyManager->varmap.vars[idx - MyManager->varmap.varstart], dprob);
    }
    // notiz
    ivalue = MyManager->varmap.ivalue[idx - MyManager->varmap.varstart];
    //+ new{
    // double tvalue; // probability of prob fact corresp to node
    // tvalue = MyManager->varmap.dvalue[idx - MyManager->varmap.varstart];
    //}
    if (ivalue == 1) {
      p = dprob * MyManager->varmap.dvalue[idx - MyManager->varmap.varstart];
      //+ new{
      // p=dprob*MyManager->varmap.dvalue[idx - MyManager->varmap.varstart]
      // *tvalue;
      //}
      if (p > 0) { // probability is zero, don't follow this branch
        (*counts)[idx - MyManager->varmap.varstart] += p;
        //	fprintf(stdout,"oec(%s,%s,%e).
        //%%2\n",query_id,MyManager->varmap.vars[idx -
        // MyManager->varmap.varstart],p);
        if (LOG_EXPECTED)
          fprintf(stderr, "ec -> %s,%s,%e . %%2_1\n", query_id,
                  MyManager->varmap.vars[idx - MyManager->varmap.varstart], p);
      } else {
        if (LOG_EXPECTED) {
          fprintf(stdout, "%% ec(%s,%s,%30.30e). %%2_2\n", query_id,
                  MyManager->varmap.vars[idx - MyManager->varmap.varstart], p);
        }
      }
    }
  }
  //  fprintf(stderr, " skip %i \n",__LINE__);
  if (LOG_EXPECTED) {
    fprintf(stderr, "skipped\n");
  }
}

double CalcExpectedCountsDown(extmanager *MyManager, DdNode *Current,
                              char *query_id) {
  ineedtostorethatsomehow = MyManager;
  Queue q = QueueNew();
  // fprintf(stderr", =====> queue is: %p \n",q);
  int i;
  const char *curnode, *curh, *curl, *dynvalue;
  DdNode *h, *l, *node;
  ComparisonFunction fun;
  hisnode *Found = NULL, *lfound, *hfound;
  double dprob;  // downward probability of current node
  double tvalue; // probability of prob fact corresp to node
  int ivalue;
  double retval; // last value of true

  double counts[MyManager->varmap.varcnt];
  double(*pcnt)[MyManager->varmap.varcnt];
  pcnt = &counts;
  for (i = 0; i < MyManager->varmap.varcnt; i++) {
    (*pcnt)[i] = 0;
  }
  // skip everything before the first node:
  skip_nodes_cnt(MyManager, pcnt, 0, Current, 1, query_id);

  fun = *comparator;
  if (LOG_EXPECTED) {
    fprintf(stderr, " ##############################\n");
  }
  if (LOG_EXPECTED) {
    fprintf(stderr, " ##############################\n fun is %p\n", fun);
  }
  if (!Cudd_IsConstant(Current)) {
    QueuePutOnPriority(q, Current, NODE_VALUE, fun);
    Found = GetNode(MyManager->his, MyManager->varmap.varstart, Current);
    (*Found).dvalue2 = 1.0 / ((*Found).dvalue);
    dynvalue = (*Found).dynvalue;
  }
  Current = NULL; // not used anymore or should not be
  retval = 0;

  while (QueueSize(q) > 0) {
    if (LOG_EXPECTED) {
      fprintf(stderr, "\n");
    }
    if (LOG_EXPECTED) {
      PrintNodeQueue(q, *MyManager);
    }
    node = QueueGet(q);
    curnode = GetNodeVarNameDisp(MyManager->manager, MyManager->varmap, node);
    // int level = Cudd_ReadPerm(MyManager->manager,GetIndex(node));
    if (!Cudd_IsConstant(node)) {
      tvalue =
          MyManager->varmap.dvalue[GetIndex(node) - MyManager->varmap.varstart];
      ivalue =
          MyManager->varmap.ivalue[GetIndex(node) - MyManager->varmap.varstart];
      dynvalue =
          MyManager->varmap.vars[GetIndex(node) - MyManager->varmap.varstart];
      Found = GetNode(MyManager->his, MyManager->varmap.varstart, node);
      dprob = (*Found).dvalue2;
      l = LowNodeOf(MyManager->manager, node);
      h = HighNodeOf(MyManager->manager, node);
      lfound = GetNode(MyManager->his, MyManager->varmap.varstart, l);
      hfound = GetNode(MyManager->his, MyManager->varmap.varstart, h);
      curh = GetNodeVarNameDisp(MyManager->manager, MyManager->varmap, h);
      curl = GetNodeVarNameDisp(MyManager->manager, MyManager->varmap, l);

      if (LOG_EXPECTED) {
        fprintf(stderr, "%s (%i)-->  %s %s\n", curnode, (*node).index, curh,
                curl);
      }
      /** low node */
      if ((*lfound).dvalue2 < -0.1) { // only if not seen before == dvalue2=0
                                      // (almost) otherwise requing does not
                                      // harm
        if (LOG_EXPECTED) {
          fprintf(stderr, "queueing l(%s)=%s \n", curnode, curl);
        }
        QueuePutOnPriority(q, l, NODE_VALUE, fun);
        (*lfound).dvalue2 = 0;
      }
      ((*lfound).dvalue2) =
          ((*lfound).dvalue2) + (ivalue == 0 ? dprob : dprob * (1 - tvalue));
      if (LOG_EXPECTED) {
        fprintf(stderr, "l(%s)=%s %e \n", curnode, curl, (*lfound).dvalue2);
      }
      if (LOG_EXPECTED) {
        fprintf(stderr, "l(%s)=%s %e %e %e\n", curnode, curl, (*lfound).dvalue2,
                tvalue, dprob);
      }

      /** high node */
      if ((*hfound).dvalue2 < -0.1) { // only if not seen before == dvalue2=0
                                      // (almost) otherwise requing does not
                                      // harm
        fun = *comparator;
        (*fun)(l, l);
        if (LOG_EXPECTED) {
          PrintNodeQueue(q, *MyManager);
          fprintf(stderr, "-> %p\n", h);
        }
        QueuePutOnPriority(q, h, NODE_VALUE, fun);
        (*hfound).dvalue2 = 0;
      }
      (*hfound).dvalue2 =
          (*hfound).dvalue2 + (ivalue == 0 ? dprob : (dprob * (tvalue)));
      if (LOG_EXPECTED) {
        fprintf(stderr, "h(%s)=%s %e %e %e\n", curnode, curh, (*hfound).dvalue2,
                tvalue, dprob);
      }
      /** output expected counts current node */
      if (ivalue == 1) {
        (*pcnt)[GetIndex(node) - MyManager->varmap.varstart] +=
            dprob * tvalue * (*hfound).dvalue;
        // fprintf(stdout,"oec(%s,%s,%e). %% 1_1\n",query_id,dynvalue,dprob *
        // tvalue * (*hfound).dvalue);
        if (LOG_EXPECTED)
          fprintf(stderr, "ec -> %s,%s,%e . %% 1_1\n", query_id, dynvalue,
                  dprob * tvalue * (*hfound).dvalue);
      } else {
        (*pcnt)[GetIndex(node) - MyManager->varmap.varstart] +=
            dprob * tvalue * (*hfound).dvalue;
        if (LOG_EXPECTED)
          fprintf(stderr, "ec -> %s,%s,%e . %% 1_2\n", query_id, dynvalue,
                  dprob * tvalue * (*hfound).dvalue);
      }
      /** output expected counts of skipped nodes for low branch*/
      skip_nodes(MyManager, pcnt, node, l,
                 dprob * ((ivalue == 0) ? 1 : (1 - tvalue)) * (*lfound).dvalue,
                 query_id);
      skip_nodes(MyManager, pcnt, node, h,
                 dprob * ((ivalue == 0) ? 1 : (tvalue)) * (*hfound).dvalue,
                 query_id);
    } else {
      if (LOG_EXPECTED) {
        fprintf(stderr, "here: retval %s %e=>%e\n", curnode, retval,
                (*Found).dvalue2);
      }
      if (node == (MyManager->t)) {
        if (LOG_EXPECTED) {
          fprintf(stderr, "updating retval %e=>%e\n", retval, (*Found).dvalue2);
        }
        retval = (*Found).dvalue2;
      }
    }
  }
  for (i = 0; i < MyManager->varmap.varcnt; i++) {
    ivalue = MyManager->varmap.ivalue[i];
    /* fprintf(stderr,"Node  level %i index: %i name: %s (dprob is %e)\n", */
    /* 	    i,idx, */
    /* 	    MyManager->varmap.vars[idx - MyManager->varmap.varstart], */
    /* 	    dprob); */
    // fprintf(stderr,"Node idx: %i level: %i
    // \n",i,Cudd_ReadPerm(MyManager->manager,i));
    if (ivalue == 0) {
      fprintf(stdout, "%% det: ec(%s,%s,%30.30e).\n", query_id,
              MyManager->varmap.vars[i], (counts)[i]);
    } else {
      fprintf(stdout, "ec(%s,%s,%30.30e).\n", query_id,
              MyManager->varmap.vars[i], (counts)[i]);
    }
  }
  // free(counts);
  if (LOG_EXPECTED) {
    fprintf(stderr, "retval is %e\n", retval);
  }
  return retval;
}

double CalcExpectedCountsUp(extmanager *MyManager, DdNode *Current,
                            char *query_id) {
  //  fprintf(stderr,"--------------------- the manager 2 %p \n",&MyManager);

  DdNode *h, *l;
  hisnode *Found;
  const char *curnode = NULL;
  double lvalue, hvalue, tvalue;
  //  tvalue=0.0;
  int ivalue;
  if (params.debug) {
    curnode =
        GetNodeVarNameDisp(MyManager->manager, MyManager->varmap, Current);
    fprintf(stderr, "%s\n", curnode);
  }

  if (Current == MyManager->t) {
    //    if ((Found = GetNode(MyManager->his, MyManager->varmap.varstart,
    //    Current)) == NULL) {
    //    fprintf(stderr,"adding true \n");
    AddNode(MyManager->his, MyManager->varmap.varstart, MyManager->t, 1, 0,
            NULL); //}//needed in down
    return 1.0;
  }
  if (Current == MyManager->f) {
    //    fprintf(stderr,"adding false \n");
    //    if ((Found = GetNode(MyManager->his, MyManager->varmap.varstart,
    //    Current)) == NULL) {
    AddNode(MyManager->his, MyManager->varmap.varstart, MyManager->f, 0, 0,
            NULL); //}//needed in down
    return 0.0;
  }

  if ((Found = GetNode(MyManager->his, MyManager->varmap.varstart, Current)) !=
      NULL)
    return Found->dvalue;
  l = LowNodeOf(MyManager->manager, Current);
  h = HighNodeOf(MyManager->manager, Current);
  if (params.debug)
    fprintf(stderr, "l(%s)->", curnode);
  lvalue = CalcExpectedCountsUp(MyManager, l, query_id);
  if (params.debug)
    fprintf(stderr, "h(%s)->", curnode);
  hvalue = CalcExpectedCountsUp(MyManager, h, query_id);

  tvalue =
      MyManager->varmap.dvalue[GetIndex(Current) - MyManager->varmap.varstart];
  // notiz
  ivalue =
      MyManager->varmap.ivalue[GetIndex(Current) - MyManager->varmap.varstart];

  if (ivalue == 1) {
    tvalue = tvalue * hvalue + lvalue * (1.0 - tvalue);
  } else if (ivalue == 0) {
    tvalue = hvalue + lvalue;
  }
  //  fprintf(stderr," ---> %e \n",tvalue);
  AddNode(MyManager->his, MyManager->varmap.varstart, Current, tvalue, 0, NULL);
  return tvalue;
}

/* Bernds Algorithm */
// type=0  regular probabilistic fact
// type=1  derive gradient for mu
// type=2  derive gradient for sigma
gradientpair CalcGradient(extmanager MyManager, DdNode *Current, int TargetVar,
                          char *TargetPattern, int type) {
  DdNode *h, *l;
  hisnode *Found;
  const char *curnode = NULL;
  char *dynvalue;
  gradientpair lowvalue, highvalue, tvalue;
  double this_probability;
  double *gradient;
  density_integral dynvalue_parsed;

  if (params.debug) {
    curnode = GetNodeVarNameDisp(MyManager.manager, MyManager.varmap, Current);
    fprintf(stderr, "%s\n", curnode);
  }
  // base cases
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
  // node is in cache
  if ((Found = GetNode(MyManager.his, MyManager.varmap.varstart, Current)) !=
      NULL) {
    tvalue.probability = Found->dvalue;
    tvalue.gradient = *((double *)Found->dynvalue);
    return tvalue;
  }

  // inductive case
  l = LowNodeOf(MyManager.manager, Current);
  h = HighNodeOf(MyManager.manager, Current);
  if (params.debug)
    fprintf(stderr, "l(%s)->", curnode);
  lowvalue = CalcGradient(MyManager, l, TargetVar, TargetPattern, type);
  if (params.debug)
    fprintf(stderr, "h(%s)->", curnode);
  highvalue = CalcGradient(MyManager, h, TargetVar, TargetPattern, type);
  dynvalue = (char *)MyManager.varmap
                 .dynvalue[GetIndex(Current) - MyManager.varmap.varstart];
  if (dynvalue == NULL) { // no dynvalue, it's a regular probabilistic fact
    memset(&dynvalue_parsed, 0, sizeof(dynvalue_parsed));
    this_probability = sigmoid(
        MyManager.varmap.dvalue[GetIndex(Current) - MyManager.varmap.varstart],
        params.sigmoid_slope);
  } else { // there is a dynvalue, it's a continuous fact! let's do the hybrid
           // ProbLog magic here
    curnode = GetNodeVarNameDisp(MyManager.manager, MyManager.varmap, Current);
    dynvalue_parsed = parse_density_integral_string(dynvalue, curnode);
    this_probability =
        cumulative_normal(dynvalue_parsed.low, dynvalue_parsed.high,
                          dynvalue_parsed.mu, dynvalue_parsed.sigma);
  }

  tvalue.probability = this_probability * highvalue.probability +
                       (1 - this_probability) * lowvalue.probability;
  tvalue.gradient = this_probability * highvalue.gradient +
                    (1 - this_probability) * lowvalue.gradient;
  if ((GetIndex(Current) == TargetVar) ||
      ((TargetPattern != NULL) &&
       patternmatch(TargetPattern, MyManager.varmap.vars[GetIndex(Current)]))) {
    if (type == 0) { // current node is normal probabilistic fact
      tvalue.gradient += (highvalue.probability - lowvalue.probability) *
                         this_probability * (1 - this_probability) *
                         params.sigmoid_slope;
    } else if (type == 1) { // it's a continues fact and we need d/dmu
      tvalue.gradient +=
          cumulative_normal_dmu(dynvalue_parsed.low, dynvalue_parsed.high,
                                dynvalue_parsed.mu, dynvalue_parsed.sigma) *
          (highvalue.probability + lowvalue.probability);
    } else if (type == 2) { // it's a continues fact and we need d/dsigma
      tvalue.gradient +=
          cumulative_normal_dsigma(dynvalue_parsed.low, dynvalue_parsed.high,
                                   dynvalue_parsed.mu, dynvalue_parsed.sigma) *
          (highvalue.probability + lowvalue.probability);
    }
  }
  gradient = (double *)malloc(sizeof(double));
  *gradient = tvalue.gradient;
  AddNode(MyManager.his, MyManager.varmap.varstart, Current, tvalue.probability,
          0, gradient);
  return tvalue;
}

char *extractpattern(const char *thestr) {
  char *p;
  int i = 0, sl = strlen(thestr);
  while ((thestr[i] != '_') && (i < sl))
    i++;
  if (i == sl)
    return NULL;
  i++;
  p = (char *)malloc(sizeof(char) * (i + 2));
  strncpy(p, thestr, i);
  p[i] = '*';
  p[i + 1] = '\0';
  return p;
}

int patterncalculated(char *pattern, extmanager MyManager, int loc) {
  int i;
  if (pattern == NULL)
    return 0;
  for (i = loc - 1; i > -1; i--)
    if (patternmatch(pattern, MyManager.varmap.vars[i]))
      return 1;
  return 0;
}
