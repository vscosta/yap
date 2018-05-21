/******************************************************************************\
*                                                                              *
*    SimpleCUDD library (www.cs.kuleuven.be/~theo/tools/simplecudd.html)       *
*  SimpleCUDD was developed at Katholieke Universiteit Leuven(www.kuleuven.be) *
*                                                                              *
*  Copyright Katholieke Universiteit Leuven 2008, 2009, 2010                   *
*                                                                              *
*  Author: Theofrastos Mantadelis, Angelika Kimmig, Bernd Gutmann              *
*  File: problogbdd.c                                                          *
*  $Date:: 2010-10-06 18:06:08 +0200 (Wed, 06 Oct 2010)                      $ *
*  $Revision:: 4883                                                          $ *
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

//#include <stdio.h>
//#include <stdlib.h>
#include <unistd.h>
#include "simplecudd.h"
#include "problogmath.h"
#include <signal.h>
#include <time.h>

#define  PROBLOGBDD_VERSION "2.0.1"


#ifndef max
	#define max( a, b ) ( ((a) > (b)) ? (a) : (b) )
#endif

// INFINITY macro does not work on trantor (64-bit linux of some kind)
const double my_infinity = 1.0/0.0;

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
  int utilfile;
  int independent_forest;
  int local_search;
  int dynreorder;
  int staticorder;
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

typedef struct _bdd_mgr {
  extmanager extmanager;
  DdNode *root;
} bdd_mgr;


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
gradientpair CalcGradient(extmanager MyManager, DdNode *Current, int TargetVar, char *TargetPattern, int type);
int patterncalculated(char *pattern, extmanager MyManager, int loc);
char * extractpattern(char *thestr);

// added by GUY
double* read_util_file(char * filename);
int forestSize(DdNode **forest);
int compare_util_adds(const void* A, const void* B);
void exact_strategy_search(extmanager* MyManager, DdNode **forest, double* utilities);
DdNode* buildADDfromBDD(extmanager* MyManager, DdNode *Current, DdManager* addmgr);
void ReInitAndUnrefHistory(hisqueue *HisQueue, int varcnt, DdManager* mgr);
char* GetAddNodeVarNameDisp(namedvars varmap, DdNode *node);
int extractstrategy(extmanager* MyManager, DdManager * add_mgr, DdNode *Current, DdNode *max_node);
DdNode * setLowerBound(DdManager * dd, DdNode * f, double lowerBound);
DdNode * setLowerBoundRecur(DdManager * dd, DdNode * f, double lowerBound);
void local_strategy_search(extmanager* MyManager, DdNode **forest, double* utilities);
void local_strategy_search_independent(bdd_mgr* bdd_mgrs, double* utilities, int nb_bdds, namedvars globalvars);
double expected_value(extmanager* MyManager, DdNode **forest, double* utilities);
void print_strategy(namedvars varmap);
void newManager(extmanager* MyManager,bddfileheader fileheader, int nbManagers);
bdd_mgr* generateIndependentBDDForest(bddfileheader fileheader);
int LoadVariableDataForForest(namedvars varmap, char *filename);
int printTime(void);

int main(int argc, char **arg) {
  extmanager MyManager;
  DdNode *bdd, **forest, *bakbdd;
  bddfileheader fileheader;
  int i, ivarcnt, code, curbdd;
  gradientpair tvalue;
  double probability = -1.0;
  char *varpattern;
  bdd_mgr* bdd_mgrs;
  varpattern = NULL;
  code = -1;
  params = loadparam(argc, arg);

  //Initializin to NULL to be safe?
  bdd = NULL;
  bakbdd = NULL;
  forest = NULL;
  bdd_mgrs = NULL;

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

  if (params.method != 0 && arg[params.method][0] != 'g' && arg[params.method][0] != 'p' && arg[params.method][0] != 'o' && arg[params.method][0] != 'l' && arg[params.method][0] != 's') {
    printhelp(argc, arg);
    fprintf(stderr, "Error: you must choose a calculation method beetween [p]robability, [g]radient, [l]ine search, [s]earch for strategy, [o]nline.\n");
    return -1;
  }

  if (params.method != 0 && (arg[params.method][0] == 'g' || arg[params.method][0] == 'p' || arg[params.method][0] == 'l') && params.inputfile == -1) {
    printhelp(argc, arg);
    fprintf(stderr, "Error: an input file is necessary for probability, gradient or line search calculation methods.\n");
    return -1;
  }

  if (params.debug) DEBUGON;
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
    if (params.dynreorder == 1)
      MyManager.manager = simpleBDDinit(0);
    else
      MyManager.manager = simpleBDDinitNoReOrder(0);
    MyManager.t = HIGH(MyManager.manager);
    MyManager.f = LOW(MyManager.manager);
    MyManager.varmap = InitNamedVars(1, 0);
    bdd = OnlineGenerateBDD(MyManager.manager, &MyManager.varmap);
    bakbdd = bdd;
    ivarcnt = GetVarCount(MyManager.manager);
  } else if(params.independent_forest>0){
	  // the flag to create a forest of independent bdds is set
	  fileheader = ReadFileHeader(arg[params.loadfile]);
	  if (_debug) fprintf(stderr,"Generating forest of independent BDDs.\n");
	  bdd_mgrs = generateIndependentBDDForest(fileheader);
	  ivarcnt = fileheader.varcnt;
	  MyManager.varmap = InitNamedVars(fileheader.varcnt, fileheader.varstart);
  } else{
    fileheader = ReadFileHeader(arg[params.loadfile]);
    switch(fileheader.filetype) {
      case BDDFILE_SCRIPT:
        if (params.dynreorder == 1)
          MyManager.manager = simpleBDDinit(fileheader.varcnt);
        else
          MyManager.manager = simpleBDDinitNoReOrder(fileheader.varcnt);
        MyManager.t = HIGH(MyManager.manager);
        MyManager.f = LOW(MyManager.manager);
        MyManager.varmap = InitNamedVars(fileheader.varcnt, fileheader.varstart);
        if (params.staticorder > 0) {
          char **Order = GetVariableOrder(arg[params.staticorder], MyManager.varmap.varcnt);
          for (i = 0; i < MyManager.varmap.varcnt; i++)
            if (Order[i] != NULL) AddNamedVarAt(MyManager.varmap, Order[i], i);
        }
        if (fileheader.version > 1) {
          forest = FileGenerateBDDForest(MyManager.manager, MyManager.varmap, fileheader);
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
        if (params.dynreorder == 1)
          MyManager.manager = simpleBDDinit(fileheader.varcnt);
        else
          MyManager.manager = simpleBDDinitNoReOrder(fileheader.varcnt);
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

  if (params.method == 0 || arg[params.method][0] != 's') {
	  if (bdd != NULL || bdd_mgrs != NULL) {
		  ivarcnt = RepairVarcnt(&MyManager.varmap);
		  code = 0;
		  if (params.inputfile != -1) {
			  if (LoadVariableData(MyManager.varmap, arg[params.inputfile]) == -1) return -1;
			  if (!all_loaded(MyManager.varmap, 1)) return -1;
		  }
		  // impose a predifined order good for debugging
		  // can be used with a partial number of variables to impose ordering at beggining of BDD
		  if (params.orderfile != -1) {
			  ImposeOrder(MyManager.manager, MyManager.varmap, GetVariableOrder(arg[params.orderfile], MyManager.varmap.varcnt));
		  }
		  curbdd = 0;
		  do {
			  MyManager.his = InitHistory(ivarcnt);
			  if (params.method != 0) {
				  switch(arg[params.method][0]) {
				  case 'g':
					  for (i = 0; i < MyManager.varmap.varcnt; i++) {
						  if (MyManager.varmap.vars[i] != NULL) {

							  // check whether this is a continues fact
							  if (MyManager.varmap.dynvalue[i] == NULL) {  // nope, regular fact
								  varpattern = extractpattern(MyManager.varmap.vars[i]);
								  if ((varpattern == NULL) || (!patterncalculated(varpattern, MyManager, i))) {
									  tvalue = CalcGradient(MyManager, bdd, i + MyManager.varmap.varstart, varpattern, 0);
									  probability = tvalue.probability;
									  if (varpattern == NULL) {
										  printf("query_gradient(%s,%s,p,%e).\n", arg[params.queryid], MyManager.varmap.vars[i], tvalue.gradient);
									  } else {
										  varpattern[strlen(varpattern) - 2] = '\0';
										  printf("query_gradient(%s,%s,p,%e).\n", arg[params.queryid], varpattern, tvalue.gradient);
									  }
									  ReInitHistory(MyManager.his, MyManager.varmap.varcnt);
									  if (varpattern != NULL) free(varpattern);
								  }
							  } else { // it is! let's do the Hybrid Problog Magic
								  // first for mu
								  varpattern = extractpattern(MyManager.varmap.vars[i]);
								  if ((varpattern == NULL) || (!patterncalculated(varpattern, MyManager, i))) {
									  tvalue = CalcGradient(MyManager, bdd, i + MyManager.varmap.varstart, varpattern, 1);
									  probability = tvalue.probability;
									  if (varpattern == NULL) {
										  printf("query_gradient(%s,%s,mu,%e).\n", arg[params.queryid], MyManager.varmap.vars[i], tvalue.gradient);
									  } else {
										  varpattern[strlen(varpattern) - 2] = '\0';
										  printf("query_gradient(%s,%s,mu,%e).\n", arg[params.queryid], varpattern, tvalue.gradient);
									  }
								  }
								  ReInitHistory(MyManager.his, MyManager.varmap.varcnt);
								  if (varpattern != NULL) free(varpattern);

								  // then for sigma
								  varpattern = extractpattern(MyManager.varmap.vars[i]);
								  if ((varpattern == NULL) || (!patterncalculated(varpattern, MyManager, i))) {
									  tvalue = CalcGradient(MyManager, bdd, i + MyManager.varmap.varstart, varpattern, 2);
									  probability = tvalue.probability;
									  if (varpattern == NULL) {
										  printf("query_gradient(%s,%s,sigma,%e).\n", arg[params.queryid], MyManager.varmap.vars[i], tvalue.gradient);
									  } else {
										  varpattern[strlen(varpattern) - 2] = '\0';
										  printf("query_gradient(%s,%s,sigma,%e).\n", arg[params.queryid], varpattern, tvalue.gradient);
									  }
								  }
								  ReInitHistory(MyManager.his, MyManager.varmap.varcnt);
								  if (varpattern != NULL) free(varpattern);
							  }

						  } else {
							  fprintf(stderr, "Error: no variable name given for parameter.\n");
						  }
					  }
					  if (probability < 0.0) {
						  // no nodes, so we have to calculate probability ourself
						  tvalue = CalcGradient(MyManager, bdd, 0 + MyManager.varmap.varstart, NULL, 0);
						  probability = tvalue.probability;
					  }
					  printf("query_probability(%s,%e).\n", arg[params.queryid], probability);
					  break;
				  case 'l':
					  tvalue = CalcGradient(MyManager, bdd, 0 + MyManager.varmap.varstart, NULL, 0);
					  probability = tvalue.probability;
					  printf("query_probability(%s,%e).\n", arg[params.queryid], probability);
					  break;
				  case 'p':
					  printf("probability(%e).\n", CalcProbability(MyManager, bdd));
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
			  if (forest != NULL) {
				  curbdd++;
				  bdd = forest[curbdd];
			  } else {
				  bdd = NULL;
			  }
			  // Guy: I removed it, why is it here?
			  // ReInitHistory(MyManager.his, MyManager.varmap.varcnt);
		  } while(bdd != NULL);

		  bdd = bakbdd;
		  if (params.savedfile > -1) SaveNodeDump(MyManager.manager, MyManager.varmap, bdd, arg[params.savedfile]);
		  if (params.exportfile > -1) simpleNamedBDDtoDot(MyManager.manager, MyManager.varmap, bdd, arg[params.exportfile]);
	  }
  }else{
	  // param "s" is set
	  // do strategy search on the forest
	  code = 0;
	  if(params.independent_forest>0){
		  //the forest consists of independent bdds
		  LoadVariableDataForForest(MyManager.varmap,arg[params.inputfile]);
		  if(bdd_mgrs[0].root == NULL){
			  fprintf(stderr, "Error: No BDDs were generated.\n");
			  return -1;
		  }
		  if (_debug) fprintf(stderr,"Initializing histories.\n");
		  if(params.local_search>0){
			  if (_debug) fprintf(stderr,"Independent local search.\n");
			  local_strategy_search_independent(bdd_mgrs, read_util_file(arg[params.utilfile]), fileheader.intercnt, MyManager.varmap);
		  }else{
			  if (_debug) fprintf(stderr,"Independent exact search.\n");
			  fprintf(stderr, "Error: independent exact search not supported yet.\n");
			  return -1;
		  }
	  }else{
		  ivarcnt = RepairVarcnt(&MyManager.varmap);
		  //the forest is a bdd with multiple entry points
		  if (params.inputfile != -1) {
			  if (LoadVariableData(MyManager.varmap, arg[params.inputfile]) == -1) return -1;
			  if (!all_loaded(MyManager.varmap, 1)) return -1;
		  }
		  // impose a predifined order good for debugging
		  // can be used with a partial number of variables to impose ordering at beggining of BDD
		  if (params.orderfile != -1) {
			  ImposeOrder(MyManager.manager, MyManager.varmap, GetVariableOrder(arg[params.orderfile], MyManager.varmap.varcnt));
		  }
		  MyManager.his = InitHistory(ivarcnt);
		  if (_debug) fprintf(stderr,"Initialized shared history with %i variables.\n", ivarcnt);
		  if(params.local_search>0){
			  if (_debug) fprintf(stderr,"Local search.\n");
			  local_strategy_search(&MyManager, forest, read_util_file(arg[params.utilfile]));
		  }else{
			  if (_debug) fprintf(stderr,"Exact search.\n");
			  exact_strategy_search(&MyManager, forest, read_util_file(arg[params.utilfile]));
		  }
		  free(MyManager.his);
	  }
	  print_strategy(MyManager.varmap);
  }
  if (_debug) fprintf(stderr,"Cleaning up.\n");
  if(params.independent_forest>0){
	  // TODO clean up memory - the existing code gives an invalid pointer problem
  }else{
	  if (MyManager.manager != NULL) {
	    KillBDD(MyManager.manager);
	    free(MyManager.varmap.dvalue);
	    free(MyManager.varmap.ivalue);
	    if (MyManager.varmap.dynvalue != NULL) {
	      for(i = 0; i < MyManager.varmap.varcnt; i++)
	        if (MyManager.varmap.dynvalue[i] != NULL) {
	          free(MyManager.varmap.dynvalue[i]);
	        }
	      free(MyManager.varmap.dynvalue);
	    }
	    for (i = 0; i < MyManager.varmap.varcnt; i++)
	      free(MyManager.varmap.vars[i]);
	    free(MyManager.varmap.vars);
	  }
  }
  if (params.error != NULL) free(params.error);

  return code;

}

//////////////////
// Added by Guy //
//////////////////

double* read_util_file(char *filename){
	FILE* file;
	double line;
	int nb_lines;
	double *utils;
	int i=0;
	if ((file = fopen(filename, "r")) == NULL) {
		perror(filename);
		return NULL;
	}
	// Read file
	fscanf(file, "%i\n", &nb_lines);
	utils = (double *) malloc(sizeof(double)*nb_lines);
	//utils = new double[nb_lines];
	while (!feof(file)) {
		fscanf(file, "%lf\n", &line);
		//fprintf(stderr,"read %g.\n", line);
		if(i>nb_lines) {
			fprintf(stderr,"The number of lines field of %i does not match the number of lines in the file.\n",nb_lines);
			fclose(file);
			exit(1);
		}
		utils[i++] = line;
    }
	fclose(file);
	if(i!=nb_lines) {
		fprintf(stderr,"The number of lines field of %i does not match the number of lines in the file %i.\n",nb_lines,i);
				fclose(file);
				exit(1);
	}
	if (params.debug) for(i=0;i<nb_lines;i++){ fprintf(stderr,"utils[%i]=%g\n", i,utils[i]); }
	return utils;
}

int forestSize(DdNode **forest) {
	int i = 0;
	do{
		i++;
	}while(forest[i] != NULL);
	return i;
}


typedef struct _util_add {
  DdNode * root;
  double util_spread;
} util_add;

int compare_util_adds(const void* A, const void* B){
	if(((const util_add*) A)->util_spread < ((const util_add*) B)->util_spread) return 1;
	else if (((const util_add*) A)->util_spread > ((const util_add*) B)->util_spread) return -1;
	else return 0;
}

void exact_strategy_search(extmanager* MyManager, DdNode **forest, double* utilities){
    DdManager* add_mgr;
	DdNode *sum, *temp, *add_ps, *constant;
	DdNode *max_node;
	FILE *outfile; //output file pointer for .dot file
	int i;
	char filename[128];
	const char** names;
	int n = forestSize(forest);
	double utility_to_go = 0.000001;
	util_add * util_adds = (util_add *) malloc(sizeof(util_add)*n);

  names = NULL;
	if (params.debug) {
        fprintf(stderr, "init add\n");
    }
    if (params.dynreorder == 1) {
        add_mgr = simpleBDDinit(MyManager->varmap.varcnt);
    }else{
        add_mgr = simpleBDDinitNoReOrder(MyManager->varmap.varcnt);
	}
    if (params.debug){
        fprintf(stderr, "end init add\n");
    }

	if (params.debug) {
		names= malloc(sizeof(char*)*MyManager->varmap.varcnt);
		for(i = MyManager->varmap.varstart ;i < MyManager->varmap.varcnt; i++){
			names[i- MyManager->varmap.varstart] = MyManager->varmap.vars[i];
		}
	}

	for(i=n-1;i>=0;i--){
		if (params.debug) {
			// write BDD file
			snprintf(filename, sizeof(filename), "bdd-total.dot");
			outfile = fopen(filename,"w");
			Cudd_DumpDot(MyManager->manager, n, forest, names, NULL, outfile);
			fclose(outfile);

			temp = Cudd_BddToAdd(MyManager->manager,forest[i]);
			Cudd_Ref(temp);
			snprintf(filename, sizeof(filename), "bdd-%i.dot", i+1);
			outfile = fopen(filename,"w");
			Cudd_DumpDot(MyManager->manager, 1, &temp,names, NULL, outfile);
			fclose(outfile);
			Cudd_RecursiveDeref(MyManager->manager,temp);
		}

		// create ADD for Ps
		add_ps = buildADDfromBDD(MyManager,forest[i],add_mgr);
		Cudd_Ref(add_ps);
		if (params.debug) fprintf(stderr, "built add_ps\n");
		ReInitAndUnrefHistory(MyManager->his, MyManager->varmap.varcnt,add_mgr);

		Cudd_RecursiveDeref(MyManager->manager,forest[i]);

		if (params.debug) {
			// write ADD-Ps file
			snprintf(filename, sizeof(filename), "add-ps-%i.dot", i+1);
			outfile = fopen(filename,"w");
			Cudd_DumpDot(add_mgr, 1, &add_ps, names, NULL, outfile);
			fclose(outfile);
		}
		//if (1 || _debug) fprintf(stderr,"best terminal of add_ps after %i now %g.\n", i,cuddV(Cudd_addFindMax(add_mgr,add_ps)));
		//create ADD for u
		constant = Cudd_addConst(add_mgr,utilities[i]);
		Cudd_Ref(constant);
		util_adds[i].root = Cudd_addApply(add_mgr,Cudd_addTimes,add_ps,constant);
		Cudd_Ref(util_adds[i].root);
		Cudd_RecursiveDeref(add_mgr,constant);
		Cudd_RecursiveDeref(add_mgr,add_ps);
		//add_ps can only be dereferenced when the history is cleared or when referenced twice.

		if (params.debug) {
			// write ADD-U file
			snprintf(filename, sizeof(filename), "add-u-%i.dot", i+1);
			outfile = fopen(filename,"w");
			Cudd_DumpDot(add_mgr, 1, &util_adds[i].root, names, NULL, outfile);
			fclose(outfile);
		}

		// compute the maximum achievable utility to set useless terminals to -inf
		max_node = Cudd_addFindMax(add_mgr,util_adds[i].root);
		util_adds[i].util_spread = cuddV(max_node);
		max_node = Cudd_addFindMin(add_mgr,util_adds[i].root);
		util_adds[i].util_spread += -cuddV(max_node);
		utility_to_go += util_adds[i].util_spread;

		//if (1 || _debug) fprintf(stderr,"best terminal of util_add after %i now %g.\n", i,cuddV(Cudd_addFindMax(add_mgr,util_adds[i].root)));
	}

	qsort(util_adds, (size_t)n, sizeof(util_add), compare_util_adds);

	sum = Cudd_addConst(add_mgr,0);
	Cudd_Ref(sum);
	for(i=0;i<n;i++){ // iterate from high to low spread!
		if (params.debug) fprintf(stderr, "Merging ADD %i/%i\n",i,(n-1));

		temp = Cudd_addApply(add_mgr,Cudd_addPlus,sum,util_adds[i].root);
		Cudd_Ref(temp);
		Cudd_RecursiveDeref(add_mgr,sum);
		Cudd_RecursiveDeref(add_mgr,util_adds[i].root);
		sum = temp;

		//if (1 || _debug) fprintf(stderr,"best terminal of sum after %i now %g.\n", i,cuddV(Cudd_addFindMax(add_mgr,sum)));

		if(i<n-1){ // don't do this for the last one, you risk setting every terminal to -inf
			utility_to_go -= util_adds[i].util_spread;

			if (params.debug) fprintf(stderr, "The next BDD can make a difference of %g\n",util_adds[i].util_spread);
			if (params.debug) fprintf(stderr, "Utility to go is %g\n",utility_to_go);
			max_node = Cudd_addFindMax(add_mgr,sum);
			if (params.debug) fprintf(stderr, "Setting lower bound to %g\n",cuddV(max_node)-utility_to_go);
			if (params.debug) fprintf(stderr, "Reducing node count from %i",Cudd_DagSize(sum));
			temp = setLowerBound(add_mgr,sum,cuddV(max_node)-utility_to_go);
			Cudd_Ref(temp);
			Cudd_RecursiveDeref(add_mgr,sum);
			sum = temp;

			if (params.debug) fprintf(stderr, " to %i\n",Cudd_DagSize(sum));

			if (params.debug) {
				// write ADD-U-inf file
				snprintf(filename, sizeof(filename), "add-u-inf-%i.dot", i+1);
				outfile = fopen(filename,"w");
				Cudd_DumpDot(add_mgr, 1, &sum, names, NULL, outfile);
				fclose(outfile);
			}
		}
	}

	free(util_adds);


	if (params.debug) {
		// write ADD-U-total file
		snprintf(filename, sizeof(filename), "add-u-total.dot");
		outfile = fopen(filename,"w");
		Cudd_DumpDot(add_mgr, 1, &sum, names, NULL, outfile);
		fclose(outfile);
	}

	// find max terminal
	max_node = Cudd_addFindMax(add_mgr,sum);

	//API says Cudd_V, but only cuddV works?!
	if (_debug) fprintf(stderr,"expected_value(%g).\n", cuddV(max_node));
	printf("expected_value(%g).\n", cuddV(max_node));

	if (params.debug) fprintf(stderr, "extracting strategy from ADD\n");
	ReInitHistory(MyManager->his, MyManager->varmap.varcnt);
	extractstrategy(MyManager, add_mgr, sum, max_node);
	Cudd_RecursiveDeref(add_mgr,sum);

}

DdNode* buildADDfromBDD(extmanager* MyManager, DdNode *Current, DdManager* addmgr) {
	// the created adds are not dereferenced
	// must be done based on the dynamic programming table
	DdNode *h, *l;
	hisnode *Found;
	char *curnode;
	double fact_prob;
	int isDecision;
	DdNode *lowvalue, *highvalue, *thisvalue;
	DdNode *lowAdd, *highAdd;
	DdNode* var;
	DdNode *posprob, *negprob;

  curnode = NULL;

	//if (_debug && Cudd_DebugCheck(addmgr)!=0) exit(-1);

	if (_debug) {
		fprintf(stderr, "(%p) ", Current);
		curnode = GetNodeVarNameDisp(MyManager->manager, MyManager->varmap, Current);
		fprintf(stderr, " aka %s\n", curnode);
	}

	// base cases
	if (Current == MyManager->t){
		thisvalue = Cudd_ReadOne(addmgr);
		Cudd_Ref(thisvalue);
		//if(_debug && Cudd_DebugCheck(addmgr)!=0) exit(-1);
		return thisvalue;
	}
	if (Current == MyManager->f){
		thisvalue = Cudd_ReadZero(addmgr);
		Cudd_Ref(thisvalue);
		//if(_debug && Cudd_DebugCheck(addmgr)!=0) exit(-1);
		return thisvalue;
	}

	//node is in cache
	if ((Found = GetNode(MyManager->his, MyManager->varmap.varstart, Current)) != NULL){
		if (_debug) fprintf(stderr, "found node %p (%s) in history\n", Current,curnode);
		return (DdNode*)(Found->dynvalue);
	}

	//inductive case
	l = LowNodeOf(MyManager->manager, Current);
	if (_debug) fprintf(stderr, "l(%s)->%p", curnode,l);
	lowvalue = buildADDfromBDD(MyManager,l,addmgr);

	h = HighNodeOf(MyManager->manager, Current);
	if (_debug) fprintf(stderr, "h(%s)->%p", curnode,h);
	highvalue = buildADDfromBDD(MyManager,h,addmgr);

	//if(params.debug && Cudd_DebugCheck(addmgr)!=0) exit(-1);

	isDecision = MyManager->varmap.ivalue[GetIndex(Current) - MyManager->varmap.varstart];
	if(isDecision){
		//decision
		if (_debug) fprintf(stderr,"%p (%s) is a decision\n",Current,curnode);
		var = Cudd_addIthVar(addmgr,(int)GetIndex(Current));
		Cudd_Ref(var);
		thisvalue=Cudd_addIte(addmgr,var,highvalue,lowvalue);
		Cudd_Ref(thisvalue);
		Cudd_RecursiveDeref(addmgr,var);
	} else {
		//probabilistic node
		if (_debug) fprintf(stderr,"%p (%s) is a probabilistic fact",Current,curnode);
		fact_prob = MyManager->varmap.dvalue[GetIndex(Current) - MyManager->varmap.varstart];
		if (_debug) fprintf(stderr, " with probability %lf \n", fact_prob);
		posprob = Cudd_addConst(addmgr,fact_prob);
		Cudd_Ref(posprob);
		highAdd=Cudd_addApply(addmgr,Cudd_addTimes,posprob,highvalue);
		Cudd_Ref(highAdd);
		Cudd_RecursiveDeref(addmgr,posprob);

		negprob=Cudd_addConst(addmgr,1-fact_prob);
		Cudd_Ref(negprob);
		lowAdd= Cudd_addApply(addmgr,Cudd_addTimes,negprob,lowvalue);
		Cudd_Ref(lowAdd);
		Cudd_RecursiveDeref(addmgr,negprob);

		thisvalue = Cudd_addApply(addmgr,Cudd_addPlus,highAdd,lowAdd);
		Cudd_Ref(thisvalue);

		Cudd_RecursiveDeref(addmgr,lowAdd);
		Cudd_RecursiveDeref(addmgr,highAdd);
	}
	AddNode(MyManager->his, MyManager->varmap.varstart, Current, 0, 0, thisvalue);
	return thisvalue;
}


void ReInitAndUnrefHistory(hisqueue *HisQueue, int varcnt, DdManager* mgr) {
  int i, j;
  for (i = 0; i < varcnt; i++) {
    if (HisQueue[i].thenode != NULL) {
      for (j = 0; j < HisQueue[i].cnt; j++){
    	  if(HisQueue[i].thenode[j].ivalue != 0){
    		  //if (_debug) fprintf(stderr,"At (%i,%i), unreffing node %i",i,j,HisQueue[i].thenode[j].ivalue);
    		  Cudd_RecursiveDeref(mgr,(DdNode*)(HisQueue[i].thenode[j].dynvalue));
    	  }
      }
      free(HisQueue[i].thenode);
      HisQueue[i].thenode = NULL;
    }
    HisQueue[i].cnt = 0;
  }
}

char* GetAddNodeVarNameDisp(namedvars varmap, DdNode *node) {
  unsigned int index;
  char *buffer = malloc(sizeof(char)*128);
  if (Cudd_IsConstant(node)) {
	  snprintf(buffer, 128, "%lf", cuddV(node));
	  return buffer;
  }
  if (NULL == node) return "(null)";
  index = GetIndex(node);
  return varmap.vars[index - varmap.varstart];
}

int extractstrategy(extmanager* MyManager, DdManager * add_mgr, DdNode *Current, DdNode *max_node) {
  char *curnode;
  int result;
  hisnode *Found;

  if (params.debug) {
		fprintf(stderr, "handling node %p", Current);
		curnode = GetAddNodeVarNameDisp(MyManager->varmap, Current);
		fprintf(stderr, " aka %s\n", curnode);
  }

  if(max_node == Current) return 1;
  else if (Cudd_IsConstant(Current)) return 0;
  else{
	  if ((Found = GetNode(MyManager->his, MyManager->varmap.varstart, Current)) != NULL) {
		  return Found->ivalue;
	  }
	  if(extractstrategy(MyManager,add_mgr,LowNodeOf(add_mgr, Current),max_node)){
		  // set strategy to 0
		  MyManager->varmap.dvalue[GetIndex(Current) - MyManager->varmap.varstart] = 0;
		  result = 1;
	  }else if(extractstrategy(MyManager,add_mgr,HighNodeOf(add_mgr, Current),max_node)){
		  // set strategy to 1
		  MyManager->varmap.dvalue[GetIndex(Current) - MyManager->varmap.varstart] = 1;
		  result = 1;
	  }else result = 0;
	  AddNode(MyManager->his, MyManager->varmap.varstart, Current, 0, result, NULL);
	  return result;
  }
}

DdNode * setLowerBound(DdManager * dd, DdNode * f, double lowerBound) {
    DdNode *res;
    do {
    	res = setLowerBoundRecur(dd,f,lowerBound);
    } while (dd->reordered == 1);
    return(res);
}

DdNode * setLowerBoundRecur(DdManager * dd, DdNode * f, double lowerBound) {
	DdNode *res, *fv, *fvn, *T, *E;
	DD_CTFP1 cacheOp;

	statLine(dd);
	if (cuddIsConstant(f)) {
		if(cuddV(f)<lowerBound){
			return cuddUniqueConst(dd,-my_infinity);
		}else return f;
	}
	cacheOp = (DD_CTFP1) setLowerBound;
	res = cuddCacheLookup1(dd,cacheOp,f);
	if (res != NULL) {
		return(res);
	}
	/* Recursive Step */
	fv = cuddT(f);
	fvn = cuddE(f);
	T = setLowerBoundRecur(dd,fv,lowerBound);
	if (T == NULL) {
	   return(NULL);
	}
	cuddRef(T);
	E = setLowerBoundRecur(dd,fvn,lowerBound);
	if (E == NULL) {
		Cudd_RecursiveDeref(dd,T);
		return(NULL);
	}
	cuddRef(E);
	res = (T == E) ? T : cuddUniqueInter(dd,(int)f->index,T,E);
	if (res == NULL) {
		Cudd_RecursiveDeref(dd,T);
		Cudd_RecursiveDeref(dd,E);
		return(NULL);
	}
	cuddDeref(T);
	cuddDeref(E);

	/* Store result. */
	cuddCacheInsert1(dd,cacheOp,f,res);
	return(res);
}

// TODO extmanager* or extmanager ???????
// Is copying the varmap not too inefficient?
void local_strategy_search(extmanager* MyManager, DdNode **forest, double* utilities){
	double tempev;
	int i;
	int j = 0;
	int changed = 1;
	double bestev = expected_value(MyManager, forest, utilities);
	if (_debug) fprintf(stderr,"Initial strategy has reward %g.\n", bestev);
	while(changed){
		j++;
		if (_debug) fprintf(stderr,"starting iteration %i.\n", j);
		changed = 0;
		for(i = 0; i < MyManager->varmap.varcnt; i++){
			if (MyManager->varmap.ivalue[i] == 1) {
				//it's a decision, flip it'
				MyManager->varmap.dvalue[i] = 1-MyManager->varmap.dvalue[i];
				tempev = expected_value(MyManager, forest, utilities);
				if(tempev > bestev){
					if (_debug) fprintf(stderr,"found new best strategy (%g > %g).\n", tempev, bestev);
					bestev = tempev;
					changed = 1;
				}else{
					if (_debug) fprintf(stderr,"keeping old strategy (%g < %g).\n", tempev, bestev);
					MyManager->varmap.dvalue[i] = 1-MyManager->varmap.dvalue[i];
				}
			}
		}
	}
	if (_debug) fprintf(stderr,"expected_value(%g).\n", bestev);
	printf("expected_value(%g).\n", bestev);
}

typedef struct _decision{
	int var;
	int nb_rel_bdds;
	int alloc_rel_bdds;
	int* rel_bdds;
	int* rel_bdds_var;
} decision;

void local_strategy_search_independent(bdd_mgr* bdd_mgrs, double* utilities, int nb_bdds, namedvars globalvars){
	int i, j, index;
	int changed;
	double* bdd_ev = malloc(sizeof(double)*nb_bdds);
	double* bdd_ev_temp = malloc(sizeof(double)*nb_bdds);
	double difference;
	double new_strategy;
	int nb_dec_vars = 0;
	decision* decs = (decision*) malloc(sizeof(decision)*globalvars.varcnt);
	decision* decision;

	// Initialize all BDDs and compute their utility
	if (_debug) fprintf(stderr,"Initializing BDDs and computing the starting utility\n");
	for(i=0;i<nb_bdds;i++){
		bdd_mgrs[i].extmanager.his = InitHistory(bdd_mgrs[i].extmanager.varmap.varcnt);
		bdd_ev[i] = utilities[i]*CalcProbability(bdd_mgrs[i].extmanager, bdd_mgrs[i].root);
		if (_debug) fprintf(stderr,"Utility for BDD %i is %g.\n", i, bdd_ev[i]);
		ReInitHistory(bdd_mgrs[i].extmanager.his, bdd_mgrs[i].extmanager.varmap.varcnt);
	}

	// Make a list of all decisions with the BDDs they appear in
	if (_debug) fprintf(stderr,"Making list of decisions with related BDDs\n");
	if (_debug) fprintf(stderr,"There are %i vars.\n",globalvars.varcnt);
	for(i=0;i<globalvars.varcnt;i++){
		if (_debug) fprintf(stderr,"Variable %i", i);
		if (globalvars.ivalue[i] == 1) {
			if (_debug) fprintf(stderr," is a decision, affecting bdds");
			decision = &decs[nb_dec_vars++];
			decision->var = i;
			decision->nb_rel_bdds = 0;
			decision->alloc_rel_bdds = 8;
			decision->rel_bdds = (int*) malloc(sizeof(int)*8);
			decision->rel_bdds_var = (int*)malloc(sizeof(int)*8);
			for(j=0;j<nb_bdds;j++){
				index = GetNamedVarIndex(bdd_mgrs[j].extmanager.varmap,globalvars.vars[i]);
				if(index>=0){
					if (_debug) fprintf(stderr,"  %i", j);
					bdd_mgrs[j].extmanager.varmap.dvalue[index] = globalvars.dvalue[i];
					if(decision->nb_rel_bdds == decision->alloc_rel_bdds){
						// increase array size
						decision->alloc_rel_bdds = 2*decision->alloc_rel_bdds;
						decision->rel_bdds = (int*) realloc(decision->rel_bdds,sizeof(int)*decision->alloc_rel_bdds);
						decision->rel_bdds_var = (int*) realloc(decision->rel_bdds_var,sizeof(int)*decision->alloc_rel_bdds);
					}
					decision->rel_bdds[decision->nb_rel_bdds] = j;
					decision->rel_bdds_var[decision->nb_rel_bdds] = index;
					decision->nb_rel_bdds++;
				}
			}
		}else{
			if (_debug) fprintf(stderr," is not a decision, affecting bdds");
			for(j=0;j<nb_bdds;j++){
				index = GetNamedVarIndex(bdd_mgrs[j].extmanager.varmap,globalvars.vars[i]);
				if(index>=0){
					if (_debug) fprintf(stderr,"  %i", j);
					bdd_mgrs[j].extmanager.varmap.dvalue[index] = globalvars.dvalue[i];
				}
			}
		}
		if (_debug) fprintf(stderr,".\n");
	}

	if (_debug) fprintf(stderr,"Starting Search\n");
	if (_debug) fprintf(stderr,"There are %i decisions.\n",nb_dec_vars);
	do{
		changed = 0;
		if (_debug) fprintf(stderr,"New Iteration\n");
		for(i=0;i<nb_dec_vars;i++){
			new_strategy = 1-globalvars.dvalue[decs[i].var];
			difference = 0;
			for(j=0;j<decs[i].nb_rel_bdds;j++){
				index = decs[i].rel_bdds[j];
				bdd_mgrs[index].extmanager.varmap.dvalue[decs[i].rel_bdds_var[j]] = new_strategy;
				bdd_ev_temp[index] = utilities[index]*CalcProbability(bdd_mgrs[index].extmanager, bdd_mgrs[index].root);
				ReInitHistory(bdd_mgrs[index].extmanager.his, bdd_mgrs[index].extmanager.varmap.varcnt);
				if (_debug) fprintf(stderr,"Utility for BDD %i changed from %g to %g.\n", index, bdd_ev[index],bdd_ev_temp[index]);
				difference += bdd_ev_temp[index] - bdd_ev[index];
			}
			if(difference>0){
				// it's an improvement
				globalvars.dvalue[decs[i].var] = new_strategy;
				changed = 1;
				for(j=0;j<decs[i].nb_rel_bdds;j++){
					bdd_ev[decs[i].rel_bdds[j]] = bdd_ev_temp[decs[i].rel_bdds[j]];
				}
				if (_debug) fprintf(stderr,"Changing decision %i to improve %g.\n",decs[i].var,difference);
			}else{
				// it's not an improvement, reset
				for(j=0;j<decs[i].nb_rel_bdds;j++){
					bdd_mgrs[decs[i].rel_bdds[j]].extmanager.varmap.dvalue[decs[i].rel_bdds_var[j]] = 1-new_strategy;
				}
			}
		}
	}while(changed);

	free(decs);
	free(bdd_ev_temp);
	difference = 0;
	for(i=0;i<nb_bdds;i++){
		difference += bdd_ev[i];
	}
	free(bdd_ev);
	printf("expected_value(%g).\n", difference);
}


double expected_value(extmanager* MyManager, DdNode **forest, double* utilities){
	int curbdd = 0;
	DdNode *bdd = forest[0];
	double sum = 0;
	double prob = 0;
	do {
		prob = CalcProbability(*MyManager, bdd);
		//printf("probability(%e).\n", prob);
		sum += (*utilities) * prob;
		//printf("sum is %e.\n", sum);
		curbdd++;
		bdd = forest[curbdd];
		utilities++;
	} while(bdd != NULL);
	ReInitHistory(MyManager->his, MyManager->varmap.varcnt);
	//printf("final sum is %e.\n", sum);
	return sum;
}

void print_strategy(namedvars varmap){
	int i;
	for(i = 0; i < varmap.varcnt; i++){
		if (varmap.ivalue[i] == 1) {
			// it's a decision, print it
			// if it contains an '_', it must be quoted,
			// otherwise don't quote because it must parsed as an integer in prolog
			if(strchr(varmap.vars[i]+1,'_')==NULL){
				if (_debug) fprintf(stderr,"strategy(%s,%g).\n",varmap.vars[i]+1, varmap.dvalue[i]);
				printf("strategy(%s,%g).\n",varmap.vars[i]+1, varmap.dvalue[i]);
			}else{
				if (_debug) fprintf(stderr,"strategy('%s',%g).\n",varmap.vars[i]+1, varmap.dvalue[i]);
				printf("strategy('%s',%g).\n",varmap.vars[i]+1, varmap.dvalue[i]);
			}
		}
	}
}

// new manager for bdd forest needs very low memory requirements! -l
void newManager(extmanager* MyManager, bddfileheader fileheader, int nbManagers){
// 	MyManager->manager;
	if (_debug) fprintf(stderr,"Creating new BDD manager.\n\n");
	if (_debug) fprintf(stderr,"Setting BDD manager memory consumption to %i.\n", max(1024,(512*1024*1024)/nbManagers));
	MyManager->manager = Cudd_Init((unsigned int)fileheader.varcnt, 0,
			(unsigned int)max(32,CUDD_UNIQUE_SLOTS/nbManagers),
			(unsigned int)max(512,CUDD_CACHE_SLOTS/nbManagers),
			(unsigned int)max(5000,(1024*1024*1024)/nbManagers));
	Cudd_AutodynEnable(MyManager->manager, CUDD_REORDER_GROUP_SIFT);
	//Cudd_SetMaxCacheHard(MyManager->manager, 1024*1024*1024);
	//Cudd_SetLooseUpTo(MyManager->manager, 1024*1024*512);
	if (_debug) Cudd_EnableReorderingReporting(MyManager->manager);
	MyManager->t = HIGH(MyManager->manager);
	MyManager->f = LOW(MyManager->manager);
	MyManager->varmap = InitNamedVars(fileheader.varcnt, fileheader.varstart);
	if (_debug) Cudd_PrintInfo(MyManager->manager,stderr);
	MyManager->his = InitHistory(fileheader.varcnt);
}


int printTime(void){
	struct tm *current;
	time_t now;
	time(&now);
	current = localtime(&now);
	fprintf(stderr, "%i:%i:%i: ", current->tm_hour, current->tm_min, current->tm_sec);
  return 1;
}

bdd_mgr* generateIndependentBDDForest(bddfileheader fileheader) {
  int icomment, maxlinesize, icur, iline, curinter, iequal;
  DdNode *Line;
  bdd_mgr * bdd_mgrs;
  char buf, *inputline, *filename, *subl;
  bddfileheader interfileheader;
  subl = NULL; // This addition might hide a real bug GUY you need to check your free(subl) instructions
  // Initialization of intermediate steps
  //Guy: +1 to delimit array????
  bdd_mgrs = (bdd_mgr *) malloc(sizeof(bdd_mgr) * (fileheader.intercnt+1));
  for (icur = 0; icur < fileheader.intercnt+1; icur++) {
	  bdd_mgrs[icur].extmanager.manager = NULL;
	  bdd_mgrs[icur].root = NULL;
  }
  // Read file data
  interfileheader.inputfile = NULL;
  filename = NULL;  // For nested files
  iequal = 0;       // Flag for encountered = sign
  icur = 0;         // Pointer for inputline buffer location
  iline = 5;        // Current file line (first after header)
  icomment = 0;     // Flag for comments
  maxlinesize = 80; // inputline starting buffer size
  inputline = (char *) malloc(sizeof(char) * maxlinesize);
  while(!feof(fileheader.inputfile)) {
    fread(&buf, 1, 1, fileheader.inputfile);
    if (buf == ';' || buf == '%' || buf == '$') icomment = 1;
    if (buf == '\n') {
      if (icomment) icomment = 0;
      if (iequal > 1) {
        fprintf(stderr, "Error at line: %i. Line contains more than 1 equal(=) signs.\n", iline);
        fclose(fileheader.inputfile);
        free(bdd_mgrs);
        free(inputline);
        return NULL;
      } else iequal = 0;
      if (icur > 0) {
        inputline[icur] = '\0';
        if (inputline[0] != 'L') {
          fprintf(stderr, "Error at line: %i. Intermediate results should start with L.\n", iline);
          fclose(fileheader.inputfile);
          free(bdd_mgrs);
          free(inputline);
          return NULL;
        }
        curinter = getInterBDD(inputline);
        if (curinter == -1) {
          if (fileheader.version < 2) {
            if (inputline[0] == 'L' && IsPosNumber(inputline + 1)) {
              curinter = atoi(inputline + 1) - 1;
              if (curinter > -1 && curinter < fileheader.intercnt && bdd_mgrs[curinter].extmanager.manager != NULL) {
                if (_debug) fprintf(stderr, "Returned: %s\n", inputline);
                fclose(fileheader.inputfile);
                free(inputline);
                //changed: just return every intermediate BDD
                return bdd_mgrs;
              } else {
                fprintf(stderr, "Error at line: %i. Return result asked doesn't exist.\n", iline);
                fclose(fileheader.inputfile);
                free(bdd_mgrs);
                free(inputline);
                return NULL;
              }
            } else {
              fprintf(stderr, "Error at line: %i. Invalid intermediate result format.\n", iline);
              fclose(fileheader.inputfile);
              free(bdd_mgrs);
              free(inputline);
              return NULL;
            }
          } else {
            // Support for forest
            maxlinesize = 10;
            iline = -1;
            for (subl = strtok(inputline, ","); subl != NULL; subl = strtok(NULL, ",")) {
              if (subl[0] == 'L' && IsPosNumber(subl + 1)) {
                curinter = atoi(subl + 1) - 1;
                if (curinter > -1 && curinter < fileheader.intercnt && bdd_mgrs[curinter].extmanager.manager != NULL) {
                  iline++;
                  if (iline >= (maxlinesize - 1)) {
                    maxlinesize *= 2;
                  }
                } else {
                  fprintf(stderr, "Error at line: %i. Return result asked(%s) doesn't exist.\n", iline, subl);
                  fclose(fileheader.inputfile);
                  free(bdd_mgrs);
                  free(inputline);
                  free(subl);
                  return NULL;
                }
              } else {
                fprintf(stderr, "Error at line: %i. Invalid intermediate result format.\n", iline);
                fclose(fileheader.inputfile);
                free(bdd_mgrs);
                free(inputline);
                free(subl);
                return NULL;
              }
            }
            if (_debug) fprintf(stderr, "Returned: %s\n", inputline);
            fclose(fileheader.inputfile);
            free(inputline);
            free(subl);
            iline++;
            //changed: just return every intermediate BDD
            return bdd_mgrs;
          }
        } else if (curinter > -1 && curinter < fileheader.intercnt && bdd_mgrs[curinter].extmanager.manager == NULL) {
          if (_debug) fprintf(stderr, "%i %s\n", curinter, inputline);
          if (_debug) printTime();
          if (_debug)  fprintf(stderr, "At line %i reading %s\n", (curinter+1), inputline);
          filename = getFileName(inputline);
          if (filename == NULL) {
        	  fprintf(stderr, "Error at line: %i. A forest of independent BDDs cannot have formulas.\n", iline);
        	  fclose(fileheader.inputfile);
        	  free(bdd_mgrs);
        	  free(inputline);
        	  free(subl);
        	  return NULL;
          } else {
            interfileheader = ReadFileHeader(filename);
            if (interfileheader.inputfile == NULL) {
              //Line = simpleBDDload(manager, &varmap, filename);
              Line = NULL;
            } else {
              newManager(&(bdd_mgrs[curinter].extmanager),interfileheader,fileheader.intercnt);
              Line = FileGenerateBDD(bdd_mgrs[curinter].extmanager.manager, bdd_mgrs[curinter].extmanager.varmap, interfileheader);
//              for(i = 0; i<bdd_mgrs[curinter].extmanager.varmap.varcnt;i++){
//            	  AddNamedVar(globalvars, bdd_mgrs[curinter].extmanager.varmap.vars[i]);
//              }
              //Cudd_EnableGarbageCollection(bdd_mgrs[curinter].manager->manager);
              if (_debug) Cudd_PrintInfo(bdd_mgrs[curinter].extmanager.manager,stderr);
            }
            if (Line == NULL) fprintf(stderr, "Error at line: %i. Error in nested BDD file: %s.\n", iline, filename);
            free(filename);
            filename = NULL;
            interfileheader.inputfile = NULL;
          }
          if (Line == NULL) {
            fclose(fileheader.inputfile);
            free(bdd_mgrs);
            free(inputline);
            return NULL;
          }
          bdd_mgrs[curinter].root = Line;
          icur = 0;
        } else if (curinter > -1 && curinter < fileheader.intercnt && bdd_mgrs[curinter].extmanager.manager != NULL) {
          fprintf(stderr, "Error at line: %i. Intermediate results can't be overwritten.\n", iline);
          fclose(fileheader.inputfile);
          free(bdd_mgrs);
          free(inputline);
          return NULL;
        } else {
          fprintf(stderr, "Error at line: %i. Intermediate result asked doesn't exist.\n", iline);
          fclose(fileheader.inputfile);
          free(bdd_mgrs);
          free(inputline);
          return NULL;
        }
      }
      iline++;
    } else if (buf != ' ' && buf != '\t' && !icomment) {
      if (buf == '=') iequal++;
      inputline[icur] = buf;
      icur += 1;
      if (icur == _maxbufsize) {
        fprintf(stderr, "Error: Maximum buffer size(%i) exceeded.\n", _maxbufsize);
        fclose(fileheader.inputfile);
        free(bdd_mgrs);
        free(inputline);
        return NULL;
      }
      while (icur > maxlinesize - 1) {
        maxlinesize *= 2;
        inputline = (char *) realloc(inputline, sizeof(char) * maxlinesize);
      }
    }
  }
  fprintf(stderr, "Error, file either doesn't end with a blank line or no return result was asked.\n");
  fclose(fileheader.inputfile);
  free(bdd_mgrs);
  free(inputline);
  return NULL;
}


int LoadVariableDataForForest(namedvars varmap, char *filename) {
  FILE *data;
  char *dataread, buf, *varname, *dynvalue;
  double dvalue = 0.0;
  int icur = 0, maxbufsize = 10, hasvar = 0, index = 0, idat = 0, ivalue = 0;
  dynvalue = NULL;
  varname = NULL;
  if ((data = fopen(filename, "r")) == NULL) {
    perror(filename);
    return -1;
  }
  dataread = (char *) malloc(sizeof(char) * maxbufsize);
  while(!feof(data)) {
    fread(&buf, 1, 1, data);
    if ((buf == '\n') && icur == 0) {
      // ignore empty lines
    } else if (buf == '\n') {
      dataread[icur] = '\0';
      icur = 0;
      buf = ' ';
      if (dataread[0] == '@') {
    	  if (hasvar) {
    		  AddNamedVarAt(varmap,varname,index);
    		  varmap.loaded[index] = 1;
    		  varmap.dvalue[index] = dvalue;
    		  varmap.ivalue[index] = ivalue;
    		  if (varmap.dynvalue[index] != NULL) {
    			  free(varmap.dynvalue[index]);
    			  varmap.dynvalue[index] = NULL;
    		  }
    		  if (dynvalue != NULL) {
    			  varmap.dynvalue[index] = (void *) malloc(sizeof(char) * (strlen(dynvalue) + 1));
    			  strcpy(varmap.dynvalue[index], dynvalue);
    			  free(dynvalue);
    			  dynvalue = NULL;
    		  }
    		  index++;
    		  dvalue = 0.0;
    		  ivalue = 0;
    		  free(varname);
    	  }
    	  varname = (char *) malloc(sizeof(char) * strlen(dataread));
    	  strcpy(varname, dataread + 1);
    	  hasvar = 1;
    	  idat = 0;
      } else {
        if (hasvar >= 0) {
          switch(idat) {
            case 0:
              if (!getRealNumber(dataread, &dvalue)) {
                fprintf(stderr, "Error at file: %s. Variable: %s can't have non real value: %s.\n", filename, varname, dataread);
                fclose(data);
                free(varname);
                free(dataread);
                return -2;
              }
              idat++;
              break;
            case 1:
              if (!getIntNumber(dataread, &ivalue)) {
                fprintf(stderr, "Error at file: %s. Variable: %s can't have non integer value: %s.\n", filename, varname, dataread);
                fclose(data);
                free(varname);
                free(dataread);
                return -2;
              }
              idat++;
              break;
            case 2:
              dynvalue = malloc(sizeof(char) * (strlen(dataread) + 1));
              strcpy(dynvalue, dataread);
              break;
          }
        }
      }
    } else {
      dataread[icur] = buf;
      icur++;
      if (icur == _maxbufsize) {
        fprintf(stderr, "Error: Maximum buffer size(%i) exceeded.\n", _maxbufsize);
        fclose(data);
        free(varname);
        free(dataread);
        return -2;
      }
      while (icur > maxbufsize - 1) {
        maxbufsize *= 2;
        dataread = (char *) realloc(dataread, sizeof(char) * maxbufsize);
      }
    }
  }
  if (hasvar) {
	  AddNamedVarAt(varmap,varname,index);
	  varmap.loaded[index] = 1;
	  varmap.dvalue[index] = dvalue;
	  varmap.ivalue[index] = ivalue;
	  if (varmap.dynvalue[index] != NULL) {
		  free(varmap.dynvalue[index]);
		  varmap.dynvalue[index] = NULL;
	  }
	  if (dynvalue != NULL) {
		  varmap.dynvalue[index] = (void *) malloc(sizeof(char) * (strlen(dynvalue) + 1));
		  strcpy(varmap.dynvalue[index], dynvalue);
		  free(dynvalue);
	  }
	  index++;
	  free(varname);
  }
  fclose(data);
  free(dataread);
  return 0;
}

///////////////////////
// Stop Added by Guy //
///////////////////////

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
  if (strcmp(arg, "-ord") == 0 || strcmp(arg, "--order") == 0) return 14;
  if (strcmp(arg, "-u") == 0 || strcmp(arg, "--utilities") == 0) return 15;
  if (strcmp(arg, "-if") == 0 || strcmp(arg, "--independent") == 0) return 16;
  if (strcmp(arg, "-lo") == 0 || strcmp(arg, "--local") == 0) return 17;
  if (strcmp(arg, "-dreorder") == 0 || strcmp(arg, "--disable-reorder") == 0) return 18;
  if (strcmp(arg, "-sord") == 0 || strcmp(arg, "--static-order") == 0) return 19;
  return -1;
}

void printhelp(int argc, char **arg) {
  fprintf(stderr, "\n\nProbLogBDD Tool Version: %s\n\n", PROBLOGBDD_VERSION);
  fprintf(stderr, "SimpleCUDD library (www.cs.kuleuven.be/~theo/tools/simplecudd.html)\n");
  fprintf(stderr, "SimpleCUDD was developed at Katholieke Universiteit Leuven(www.kuleuven.be)\n");
  fprintf(stderr, "Copyright Katholieke Universiteit Leuven 2008\n");
  fprintf(stderr, "Authors: Theofrastos Mantadelis, Angelika Kimmig, Bernd Gutmann\n");
  fprintf(stderr, "This package falls under the: Artistic License 2.0\n");
  fprintf(stderr, "\nUsage: %s -l [filename] -i [filename] -o (-s(d) [filename] -e [filename] -m [method] -id [queryid] -sl [double]) (-t [seconds] -d -h)\n", arg[0]);
  fprintf(stderr, "Generates and traverses a BDD\nMandatory parameters:\n");
  fprintf(stderr, "\t-l [filename]\t->\tfilename to load supports two formats:\n\t\t\t\t\t\t1. script with generation instructions\n\t\t\t\t\t\t2. node dump saved file\n");
  fprintf(stderr, "\t-i [filename]\t->\tfilename to input problem specifics (mandatory with file formats 1, 2)\n");
  fprintf(stderr, "\t-o\t\t->\tgenerates the BDD in online mode instead from a file can be used instead of -l\n");
  fprintf(stderr, "Optional parameters:\n");
  fprintf(stderr, "\t-sd [filename]\t->\tfilename to save generated BDD in node dump format (fast loading, traverse valid only)\n");
  fprintf(stderr, "\t-e [filename]\t->\tfilename to export generated BDD in dot format\n");
  fprintf(stderr, "\t-m [method]\t->\tthe calculation method to be used: none(default), [p]robability, [g]radient, [l]ine search, [o]nline\n");
  fprintf(stderr, "\t-id [queryid]\t->\tthe queries identity name (used by gradient) default: %s\n", arg[0]);
  fprintf(stderr, "\t-sl [double]\t->\tthe sigmoid slope (used by gradient) default: 1.0\n");
  fprintf(stderr, "\t-if \t\t->\tbuild a forest of -independent- BDDs where each BDD is in a different manager. \n");
  fprintf(stderr, "\t-u [filename]\t->\tfilename where a list of utilities can be found. \n");
  fprintf(stderr, "\t-lo \t\t->\t do local strategy search. \n");
  fprintf(stderr, "Extra parameters:\n");
  fprintf(stderr, "\t-t [seconds]\t->\tthe seconds (int) for BDD generation timeout default 0 = no timeout\n");
  fprintf(stderr, "\t-pid [pid]\t->\ta process id (int) to check for termination default 0 = no process to check\n");
  fprintf(stderr, "\t-bs [bytes]\t->\tthe bytes (int) to use as a maximum buffer size to read files default 0 = no max\n");
  fprintf(stderr, "\t-ord [filename]\t->\tUse the [filename] to define a specific final BDD variable order\n");
  fprintf(stderr, "\t-dreorder\t->\tDiseable BDD dynamic variable ordering\n");
  fprintf(stderr, "\t-sord [filename]\t->\tDefine a static ordering within [filename]\n");
  fprintf(stderr, "\t-d\t\t->\tRun in debug mode (gives extra messages in stderr)\n");
  fprintf(stderr, "\t-h\t\t->\tHelp (displays this message)\n");
  fprintf(stderr, "Extra notes:\nSupports a forest of BDDs in one shared BDD.\nSelected computational methods will be applied to each BDD seperately.\nFile operations will be applied only to the first BDD.\n");
  fprintf(stderr, "\nExample: %s -l testbdd -i input.txt -m g -id testbdd\n", arg[0]);
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
  params.utilfile = -1;
  params.independent_forest = -1;
  params.local_search = -1;
  params.error = (int *) malloc(argc * sizeof(int));
  params.dynreorder = 1;
  params.staticorder = -1;
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
        if ((argc > i + 1) && (getRealNumber(arg[i + 1], & params.sigmoid_slope))) {
          i++;
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
      case 14:
        if (argc > i + 1) {
          i++;
          params.orderfile = i;
        } else {
          params.error[params.errorcnt] = i;
          params.errorcnt++;
        }
        break;
      case 15:
    	  if (argc > i + 1) {
    		  i++;
    		  params.utilfile = i;
    	  } else {
    		  params.error[params.errorcnt] = i;
    		  params.errorcnt++;
    	  }
    	  break;
      case 16:
    	  params.independent_forest = i;
    	  break;
      case 17:
    	  params.local_search = i;
    	  break;
      case 18:
        params.dynreorder = -1;
        break;
      case 19:
        if (argc > i + 1) {
          i++;
          params.staticorder = i;
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
#ifndef __MINGW32__
  signal(SIGALRM, pidhandler);
#endif
  alarm(5);
  free(s);
}

void termhandler(int num) {
  exit(3);
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

/* Angelicas Algorithm */

double CalcProbability(extmanager MyManager, DdNode *Current) {
  DdNode *h, *l;
  hisnode *Found;
  char *curnode, *dynvalue;
  double lvalue, hvalue, tvalue;
  density_integral dynvalue_parsed;

  dynvalue_parsed.low = 0.0;
  dynvalue_parsed.high = 0.0;
  dynvalue_parsed.mu = 0.0;
  dynvalue_parsed.log_sigma = 0.0;

  curnode = NULL;

  if (params.debug) {
    curnode = GetNodeVarNameDisp(MyManager.manager, MyManager.varmap, Current);
    fprintf(stderr, "%s\n", curnode);
  }

  // base cases: 0 and 1 terminal
  if (Current == MyManager.t) return 1.0;
  if (Current == MyManager.f) return 0.0;

  // case: node is in cache
  if ((Found = GetNode(MyManager.his, MyManager.varmap.varstart, Current)) != NULL) {
    return Found->dvalue;
  }

  // case: node is not in cache
  l = LowNodeOf(MyManager.manager, Current);
  h = HighNodeOf(MyManager.manager, Current);
  if (params.debug) fprintf(stderr, "l(%s)->", curnode);
  lvalue = CalcProbability(MyManager, l);
  if (params.debug) fprintf(stderr, "h(%s)->", curnode);
  hvalue = CalcProbability(MyManager, h);

  dynvalue = (char*) MyManager.varmap.dynvalue[GetIndex(Current) - MyManager.varmap.varstart];
  if (dynvalue == NULL) {
    // no dynvalue, node is regular probabilistic fact
    tvalue = MyManager.varmap.dvalue[GetIndex(Current) - MyManager.varmap.varstart];
  } else {
    // there is a dynvalue, node is continuous fact
    curnode = GetNodeVarNameDisp(MyManager.manager, MyManager.varmap, Current);
    dynvalue_parsed = parse_density_integral_string(dynvalue, curnode);
    if (params.debug) fprintf(stderr, "  cont low=%f  high=%f  mu=%f  sigma=%f\n->", dynvalue_parsed.low, dynvalue_parsed.high, dynvalue_parsed.mu,exp(dynvalue_parsed.log_sigma) );
    tvalue = cumulative_normal(dynvalue_parsed.low, dynvalue_parsed.high, dynvalue_parsed.mu, exp(dynvalue_parsed.log_sigma))/
      (1-cumulative_normal_upper(dynvalue_parsed.low, dynvalue_parsed.mu, exp(dynvalue_parsed.log_sigma)));
  }

  tvalue = tvalue * hvalue + lvalue * (1.0 - tvalue);
  AddNode(MyManager.his, MyManager.varmap.varstart, Current, tvalue, 0, NULL);
  return tvalue;
}


/* Bernds Algorithm */
// type=0  regular probabilistic fact
// type=1  derive gradient for mu
// type=2  derive gradient for sigma
gradientpair CalcGradient(extmanager MyManager, DdNode *Current, int TargetVar, char *TargetPattern, int type) {
  DdNode *h, *l;
  hisnode *Found;
  char *curnode, *dynvalue;
  gradientpair lowvalue, highvalue, tvalue;
  double this_probability = 0.0;
  double this_gradient = 0.0;
  double continuous_denominator = 0.0, continuous_numerator = 0.0;
  double *gradient;
  density_integral dynvalue_parsed;

  dynvalue_parsed.low = 0.0;
  dynvalue_parsed.high = 0.0;
  dynvalue_parsed.mu = 0.0;
  dynvalue_parsed.log_sigma = 0.0;

  curnode = NULL;
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
  //node is in cache
  if ((Found = GetNode(MyManager.his, MyManager.varmap.varstart, Current)) != NULL) {
    tvalue.probability = Found->dvalue;
    tvalue.gradient = *((double *) Found->dynvalue);
    return tvalue;
  }

  //inductive case
  l = LowNodeOf(MyManager.manager, Current);
  h = HighNodeOf(MyManager.manager, Current);
  if (params.debug) fprintf(stderr, "l(%s)->", curnode);
  lowvalue = CalcGradient(MyManager, l, TargetVar, TargetPattern,type);
  if (params.debug) fprintf(stderr, "h(%s)->", curnode);
  highvalue = CalcGradient(MyManager, h, TargetVar, TargetPattern,type);
  dynvalue = (char*) MyManager.varmap.dynvalue[GetIndex(Current) - MyManager.varmap.varstart];
  if (dynvalue == NULL) { // no dynvalue, it's a regular probabilistic fact
    this_probability = sigmoid(MyManager.varmap.dvalue[GetIndex(Current) - MyManager.varmap.varstart], params.sigmoid_slope);
  } else { // there is a dynvalue, it's a continuous fact! let's do the hybrid ProbLog magic here
    curnode = GetNodeVarNameDisp(MyManager.manager, MyManager.varmap, Current);
    dynvalue_parsed = parse_density_integral_string(dynvalue, curnode);
    continuous_denominator = 1-cumulative_normal_upper(dynvalue_parsed.low, dynvalue_parsed.mu, exp(dynvalue_parsed.log_sigma));
    continuous_numerator = cumulative_normal(dynvalue_parsed.low, dynvalue_parsed.high, dynvalue_parsed.mu, exp(dynvalue_parsed.log_sigma));
    this_probability= continuous_numerator/continuous_denominator;
  }

  tvalue.probability = this_probability * highvalue.probability + (1 - this_probability) * lowvalue.probability;
  tvalue.gradient = this_probability * highvalue.gradient + (1 - this_probability) * lowvalue.gradient;


  // is this node, the one we want to calculcate the gradient for?

  if ((GetIndex(Current) == TargetVar) ||
      ((TargetPattern != NULL) && patternmatch(TargetPattern, MyManager.varmap.vars[GetIndex(Current)]))) {

    if (type == 0) {
      // current node is normal probabilistic fact
      this_gradient =  this_probability * (1 - this_probability) * params.sigmoid_slope;
    } else if (type == 1) {
      // it's a continues fact and we need d/dmu
      this_gradient = (cumulative_normal_dmu(dynvalue_parsed.low, dynvalue_parsed.high, dynvalue_parsed.mu, exp(dynvalue_parsed.log_sigma))*continuous_denominator+
		       continuous_numerator*cumulative_normal_upper_dmu(dynvalue_parsed.low, dynvalue_parsed.mu, exp(dynvalue_parsed.log_sigma))) /
	               (continuous_denominator*continuous_denominator);
    } else if (type == 2) {
      // it's a continues fact and we need d/dsigma


    this_gradient = exp(dynvalue_parsed.log_sigma)*

                     (cumulative_normal_dsigma(dynvalue_parsed.low, dynvalue_parsed.high, dynvalue_parsed.mu, exp(dynvalue_parsed.log_sigma))*continuous_denominator +
		       continuous_numerator*cumulative_normal_upper_dsigma(dynvalue_parsed.low, dynvalue_parsed.mu, exp(dynvalue_parsed.log_sigma))) /
	               (continuous_denominator*continuous_denominator);
    }

    tvalue.gradient += (highvalue.probability - lowvalue.probability) * this_gradient;
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
