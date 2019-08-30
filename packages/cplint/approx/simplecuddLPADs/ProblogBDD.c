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

/* modified by Fabrizio Riguzzi in 2009 for dealing with multivalued variables:
instead of variables or their negation, the script can contain equations of the
form
variable=value
Multivalued variables are translated to binary variables by means of a log
encodimg
*/

#include "simplecudd.h"
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <signal.h>
#include <time.h>

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

double ProbBool(extmanager MyManager, DdNode *node, int bits, int nBit,
                int posBVar, variable v, int comp);
double Prob(extmanager MyManager, DdNode *node, int comp);
int correctPosition(int index, variable v, int posBVar);
double ret_prob(extmanager MyManager, DdNode *bdd);

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
gradientpair CalcGradient(extmanager MyManager, DdNode *Current, int TargetVar,
                          char *TargetPattern);
int patterncalculated(char *pattern, extmanager MyManager, int loc);
char *extractpattern(char *thestr);

int main(int argc, char **arg) {
  clock_t start, endc, endt;
  double elapsedc, elapsedt;

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
      arg[params.method][0] != 'l') {
    printhelp(argc, arg);
    fprintf(stderr, "Error: you must choose a calculation method beetween "
                    "[p]robability, [g]radient, [l]ine search, [o]nline.\n");
    return -1;
  }

  if (params.debug)
    DEBUGON;
  RAPIDLOADON;
  SETMAXBUFSIZE(params.maxbufsize);
#ifndef _WIN32
  signal(SIGINT, termhandler);
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
    fileheader = ReadFileHeader(arg[params.loadfile]);
    switch (fileheader.filetype) {
    case BDDFILE_SCRIPT:
      if (params.inputfile == -1) {
        printhelp(argc, arg);
        fprintf(stderr, "Error: an input file is necessary for this type of "
                        "loading file.\n");
        return -1;
      }
      MyManager.manager = simpleBDDinit(fileheader.varcnt);
      MyManager.t = HIGH(MyManager.manager);
      MyManager.f = LOW(MyManager.manager);
      MyManager.varmap = InitNamedMultiVars(
          fileheader.varcnt, fileheader.varstart, fileheader.bvarcnt);
      if (LoadMultiVariableData(MyManager.manager, MyManager.varmap,
                                arg[params.inputfile]) == -1)
        return -1;
      start = clock();
      bdd = FileGenerateBDD(MyManager.manager, MyManager.varmap, fileheader);
      endc = clock();
      elapsedc = ((double)(endc - start)) / CLOCKS_PER_SEC;
      printf("elapsed_construction(%lf).\n", elapsedc);
      ivarcnt = fileheader.varcnt;
      break;
    case BDDFILE_NODEDUMP:
      if (params.inputfile == -1) {
        printhelp(argc, arg);
        fprintf(stderr, "Error: an input file is necessary for this type of "
                        "loading file.\n");
        return -1;
      }
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
  }
#ifndef _WIN32
  alarm(0);
#endif
  // problem specifics

  if (bdd != NULL) {
    ivarcnt = RepairVarcnt(&MyManager.varmap);
    code = 0;
    /*
    if (params.inputfile != -1) {
      if (LoadVariableData(MyManager.varmap, arg[params.inputfile]) == -1)
    return -1;
      if (!all_loaded(MyManager.varmap, 1)) return -1;
    }*/
    MyManager.his = InitHistory(ivarcnt);
    if (params.method != 0) {
      switch (arg[params.method][0]) {
      case 'g':
        for (i = 0; i < MyManager.varmap.varcnt; i++) {
          if (MyManager.varmap.vars[i] != NULL) {
            varpattern = extractpattern(MyManager.varmap.vars[i]);
            if ((varpattern == NULL) ||
                (!patterncalculated(varpattern, MyManager, i))) {
              tvalue = CalcGradient(MyManager, bdd,
                                    i + MyManager.varmap.varstart, varpattern);
              probability = tvalue.probability;
              double factor =
                  sigmoid(MyManager.varmap.dvalue[i], params.sigmoid_slope) *
                  (1 -
                   sigmoid(MyManager.varmap.dvalue[i], params.sigmoid_slope)) *
                  params.sigmoid_slope;
              if (varpattern == NULL) {
                printf("query_gradient(%s,%s,%1.12f).\n", arg[params.queryid],
                       MyManager.varmap.vars[i], tvalue.gradient * factor);
              } else {
                varpattern[strlen(varpattern) - 2] = '\0';
                printf("query_gradient(%s,%s,%1.12f).\n", arg[params.queryid],
                       varpattern, tvalue.gradient * factor);
              }
              ReInitHistory(MyManager.his, MyManager.varmap.varcnt);
            }
            if (varpattern != NULL)
              free(varpattern);
          } else {
            fprintf(stderr, "Error: no variable name given for parameter.\n");
          }
        }
        if (probability < 0.0) {
          // no nodes, so we have to calculate probability ourself
          tvalue =
              CalcGradient(MyManager, bdd, 0 + MyManager.varmap.varstart, NULL);
          probability = tvalue.probability;
        }
        printf("query_probability(%s,%1.12f).\n", arg[params.queryid],
               probability);
        break;
      case 'l':
        tvalue =
            CalcGradient(MyManager, bdd, 0 + MyManager.varmap.varstart, NULL);
        probability = tvalue.probability;
        printf("query_probability(%s,%1.12f).\n", arg[params.queryid],
               probability);
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
      start = clock();
      //	simpleNamedBDDtoDot(MyManager.manager, MyManager.varmap, bdd,
      //"bdd.dot");
      printf("probability(%1.12f).\n", ret_prob(MyManager, bdd));
      endt = clock();
      elapsedt = ((double)(endt - start)) / CLOCKS_PER_SEC;
      printf("elapsed_traversing(%lf).\n", elapsedt);
      //      myexpand(MyManager, bdd);
    }
    if (params.savedfile > -1)
      SaveNodeDump(MyManager.manager, MyManager.varmap, bdd,
                   arg[params.savedfile]);
    if (params.exportfile > -1)
      simpleNamedBDDtoDot(MyManager.manager, MyManager.varmap, bdd,
                          arg[params.exportfile]);
    ReInitHistory(MyManager.his, MyManager.varmap.varcnt);
    free(MyManager.his);
  }
  if (MyManager.manager != NULL) {
    KillBDD(MyManager.manager);
    exit(code);
    free(MyManager.varmap.dvalue);
    free(MyManager.varmap.ivalue);
    free(MyManager.varmap.dynvalue);
    for (i = 0; i < MyManager.varmap.varcnt; i++) {
      free(MyManager.varmap.vars[i]);
      free(MyManager.varmap.mvars[i].probabilities);
      free(MyManager.varmap.mvars[i].booleanVars);
    }
    free(MyManager.varmap.vars);
    free(MyManager.varmap.mvars);
    free(MyManager.varmap.bVar2mVar);
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
  return -1;
}

void printhelp(int argc, char **arg) {
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
                  "none(default), [p]robability, [g]radient, [o]nline\n");
  fprintf(stderr, "\t-id [queryid]\t->\tthe queries identity name (used by "
                  "gradient) default: %s\n",
          arg[0]);
  fprintf(stderr, "\t-sl [double]\t->\tthe sigmoid slope (used by gradient) "
                  "default: 1.0\n");
  fprintf(stderr, "Extra parameters:\n");
  fprintf(stderr, "\t-t [seconds]\t->\tthe seconds (int) for BDD generation "
                  "timeout default 0 = no timeout\n");
  fprintf(stderr, "\t-pid [pid]\t->\ta process id (int) to check for "
                  "termination default 0 = no process to check works only "
                  "under POSIX OS\n");
  fprintf(stderr, "\t-bs [bytes]\t->\tthe bytes (int) to use as a maximum "
                  "buffer size to read files default 0 = no max\n");
  fprintf(stderr,
          "\t-d\t\t->\tRun in debug mode (gives extra messages in stderr)\n");
  fprintf(stderr, "\t-h\t\t->\tHelp (displays this message)\n\n");
  fprintf(stderr, "Example: %s -l testbdd -i input.txt -m g -id testbdd\n",
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
#ifndef _WIN32
  signal(SIGALRM, pidhandler);
  alarm(5);
#endif
  free(s);
}

void termhandler(int num) { exit(3); }

/* General Functions */

double sigmoid(double x, double slope) { return 1 / (1 + exp(-x * slope)); }

/* Debugging traverse function */

void myexpand(extmanager MyManager, DdNode *Current) {
  DdNode *h, *l;
  hisnode *Found;
  char *curnode;
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

/* Bernds Algorithm */

gradientpair CalcGradient(extmanager MyManager, DdNode *Current, int TargetVar,
                          char *TargetPattern) {
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
  if ((Found = GetNode(MyManager.his, MyManager.varmap.varstart, Current)) !=
      NULL) {
    tvalue.probability = Found->dvalue;
    tvalue.gradient = *((double *)Found->dynvalue);
    return tvalue;
  }
  l = LowNodeOf(MyManager.manager, Current);
  h = HighNodeOf(MyManager.manager, Current);
  if (params.debug)
    fprintf(stderr, "l(%s)->", curnode);
  lvalue = CalcGradient(MyManager, l, TargetVar, TargetPattern);
  if (params.debug)
    fprintf(stderr, "h(%s)->", curnode);
  hvalue = CalcGradient(MyManager, h, TargetVar, TargetPattern);
  this_probability = sigmoid(
      MyManager.varmap.dvalue[GetIndex(Current) - MyManager.varmap.varstart],
      params.sigmoid_slope);
  tvalue.probability = this_probability * hvalue.probability +
                       (1 - this_probability) * lvalue.probability;
  tvalue.gradient = this_probability * hvalue.gradient +
                    (1 - this_probability) * lvalue.gradient;
  if ((GetIndex(Current) == TargetVar) ||
      ((TargetPattern != NULL) &&
       patternmatch(TargetPattern, MyManager.varmap.vars[GetIndex(Current)]))) {
    tvalue.gradient += hvalue.probability - lvalue.probability;
  }
  gradient = (double *)malloc(sizeof(double));
  *gradient = tvalue.gradient;
  AddNode(MyManager.his, MyManager.varmap.varstart, Current, tvalue.probability,
          0, gradient);
  return tvalue;
}

char *extractpattern(char *thestr) {
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

double Prob(extmanager MyManager, DdNode *node, int comp)
/* compute the probability of the expression rooted at node
nodes is used to store nodes for which the probability has alread been computed
so that it is not recomputed
 */
{
  int mVarIndex, nBit, index;
  variable v;
  hisnode *Found;
  double res;
  //double value;

  if (Cudd_IsConstant(node)) {
    //value = Cudd_V(node);
    if (comp) {
      return 0.0;
    } else {
      return 1.0;
    }
  } else {
    Found = GetNode1(MyManager.varmap.bVar2mVar, MyManager.his,
                     MyManager.varmap.varstart, node);

    if (Found != NULL) {
      return Found->dvalue;
    } else {
      index = Cudd_NodeReadIndex(node);
      mVarIndex = MyManager.varmap.bVar2mVar[index];
      v = MyManager.varmap.mvars[mVarIndex];
      nBit = v.nBit;
      res = ProbBool(MyManager, node, 0, nBit, 0, v, comp);
      AddNode1(MyManager.varmap.bVar2mVar, MyManager.his,
               MyManager.varmap.varstart, node, res, 0, NULL);
      return res;
    }
  }
}

double ProbBool(extmanager MyManager, DdNode *node, int bits, int nBit,
                int posBVar, variable v, int comp)
/* explores a group of binary variables making up the multivalued variable v */
{
  DdNode *T, *F;
  double p, res;
  double *probs;
  int index;
  probs = v.probabilities;
  if (nBit == 0) {
    if (bits >= v.nVal) {
      return 0.0;
    } else {
      p = probs[bits];
      res = p * Prob(MyManager, node, comp);
      return res;
    }
  } else {
    index = Cudd_NodeReadIndex(node);
    if (correctPosition(index, v, posBVar)) {
      T = Cudd_T(node);
      F = Cudd_E(node);
      bits = bits << 1;
      res = ProbBool(MyManager, T, bits + 1, nBit - 1, posBVar + 1, v, comp);
      comp = (!comp && Cudd_IsComplement(F)) || (comp && !Cudd_IsComplement(F));
      res = res + ProbBool(MyManager, F, bits, nBit - 1, posBVar + 1, v, comp);
      return res;
    } else {
      bits = bits << 1;
      res = ProbBool(MyManager, node, bits + 1, nBit - 1, posBVar + 1, v, comp);
      res =
          res + ProbBool(MyManager, node, bits, nBit - 1, posBVar + 1, v, comp);
      return res;
    }
  }
}

int correctPosition(int index, variable v, int posBVar)
/* returns 1 is the boolean variable with index posBVar is in the correct
position
currently explored by ProbBool */
{
  DdNode *bvar;
  bvar = v.booleanVars[posBVar];

  return (bvar == v.booleanVars[index]);
}

double ret_prob(extmanager MyManager, DdNode *bdd) {
  double prob;
  /* dividend is a global variable used by my_hash
     it is equal to an unsigned int with binary representation 11..1 */
  prob = Prob(MyManager, bdd, Cudd_IsComplement(bdd));

  return prob;
}
