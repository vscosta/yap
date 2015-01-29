/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  $Date: 2010-06-30 17:05:24 +0200 (Wed, 30 Jun 2010) $
%  $Revision: 1 $
%
%  This file is part of YAP & ProbLog
%    http://www.dcc.fc.up.pt/~vsc/Yap/index.html
%    http://dtai.cs.kuleuven.be/problog
%
%  ProbLog was developed at Katholieke Universiteit Leuven &
%  University of Porto
%
%  Copyright 2010 Katholieke Universiteit Leuven & University of Porto
%
%  Main authors of this file: Mantadelis Theofrastos, Ricardo Rocha
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Artistic License 2.0
%
% Copyright (c) 2000-2006, The Perl Foundation.
%
% Everyone is permitted to copy and distribute verbatim copies of this
% license document, but changing it is not allowed.  Preamble
%
% This license establishes the terms under which a given free software
% Package may be copied, modified, distributed, and/or
% redistributed. The intent is that the Copyright Holder maintains some
% artistic control over the development of that Package while still
% keeping the Package available as open source and free software.
%
% You are always permitted to make arrangements wholly outside of this
% license directly with the Copyright Holder of a given Package. If the
% terms of this license do not permit the full use that you propose to
% make of the Package, you should contact the Copyright Holder and seek
% a different licensing arrangement.  Definitions
%
% "Copyright Holder" means the individual(s) or organization(s) named in
% the copyright notice for the entire Package.
%
% "Contributor" means any party that has contributed code or other
% material to the Package, in accordance with the Copyright Holder's
% procedures.
%
% "You" and "your" means any person who would like to copy, distribute,
% or modify the Package.
%
% "Package" means the collection of files distributed by the Copyright
% Holder, and derivatives of that collection and/or of those files. A
% given Package may consist of either the Standard Version, or a
% Modified Version.
%
% "Distribute" means providing a copy of the Package or making it
% accessible to anyone else, or in the case of a company or
% organization, to others outside of your company or organization.
%
% "Distributor Fee" means any fee that you charge for Distributing this
% Package or providing support for this Package to another party. It
% does not mean licensing fees.
%
% "Standard Version" refers to the Package if it has not been modified,
% or has been modified only in ways explicitly requested by the
% Copyright Holder.
%
% "Modified Version" means the Package, if it has been changed, and such
% changes were not explicitly requested by the Copyright Holder.
%
% "Original License" means this Artistic License as Distributed with the
% Standard Version of the Package, in its current version or as it may
% be modified by The Perl Foundation in the future.
%
% "Source" form means the source code, documentation source, and
% configuration files for the Package.
%
% "Compiled" form means the compiled bytecode, object code, binary, or
% any other form resulting from mechanical transformation or translation
% of the Source form.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Permission for Use and Modification Without Distribution
%
% (1) You are permitted to use the Standard Version and create and use
% Modified Versions for any purpose without restriction, provided that
% you do not Distribute the Modified Version.
%
% Permissions for Redistribution of the Standard Version
%
% (2) You may Distribute verbatim copies of the Source form of the
% Standard Version of this Package in any medium without restriction,
% either gratis or for a Distributor Fee, provided that you duplicate
% all of the original copyright notices and associated disclaimers. At
% your discretion, such verbatim copies may or may not include a
% Compiled form of the Package.
%
% (3) You may apply any bug fixes, portability changes, and other
% modifications made available from the Copyright Holder. The resulting
% Package will still be considered the Standard Version, and as such
% will be subject to the Original License.
%
% Distribution of Modified Versions of the Package as Source
%
% (4) You may Distribute your Modified Version as Source (either gratis
% or for a Distributor Fee, and with or without a Compiled form of the
% Modified Version) provided that you clearly document how it differs
% from the Standard Version, including, but not limited to, documenting
% any non-standard features, executables, or modules, and provided that
% you do at least ONE of the following:
%
% (a) make the Modified Version available to the Copyright Holder of the
% Standard Version, under the Original License, so that the Copyright
% Holder may include your modifications in the Standard Version.  (b)
% ensure that installation of your Modified Version does not prevent the
% user installing or running the Standard Version. In addition, the
% modified Version must bear a name that is different from the name of
% the Standard Version.  (c) allow anyone who receives a copy of the
% Modified Version to make the Source form of the Modified Version
% available to others under (i) the Original License or (ii) a license
% that permits the licensee to freely copy, modify and redistribute the
% Modified Version using the same licensing terms that apply to the copy
% that the licensee received, and requires that the Source form of the
% Modified Version, and of any works derived from it, be made freely
% available in that license fees are prohibited but Distributor Fees are
% allowed.
%
% Distribution of Compiled Forms of the Standard Version or
% Modified Versions without the Source
%
% (5) You may Distribute Compiled forms of the Standard Version without
% the Source, provided that you include complete instructions on how to
% get the Source of the Standard Version. Such instructions must be
% valid at the time of your distribution. If these instructions, at any
% time while you are carrying out such distribution, become invalid, you
% must provide new instructions on demand or cease further
% distribution. If you provide valid instructions or cease distribution
% within thirty days after you become aware that the instructions are
% invalid, then you do not forfeit any of your rights under this
% license.
%
% (6) You may Distribute a Modified Version in Compiled form without the
% Source, provided that you comply with Section 4 with respect to the
% Source of the Modified Version.
%
% Aggregating or Linking the Package
%
% (7) You may aggregate the Package (either the Standard Version or
% Modified Version) with other packages and Distribute the resulting
% aggregation provided that you do not charge a licensing fee for the
% Package. Distributor Fees are permitted, and licensing fees for other
% components in the aggregation are permitted. The terms of this license
% apply to the use and Distribution of the Standard or Modified Versions
% as included in the aggregation.
%
% (8) You are permitted to link Modified and Standard Versions with
% other works, to embed the Package in a larger work of your own, or to
% build stand-alone binary or bytecode versions of applications that
% include the Package, and Distribute the result without restriction,
% provided the result does not expose a direct interface to the Package.
%
% Items That are Not Considered Part of a Modified Version
%
% (9) Works (including, but not limited to, modules and scripts) that
% merely extend or make use of the Package, do not, by themselves, cause
% the Package to be a Modified Version. In addition, such works are not
% considered parts of the Package itself, and are not subject to the
% terms of this license.
%
% General Provisions
%
% (10) Any use, modification, and distribution of the Standard or
% Modified Versions is governed by this Artistic License. By using,
% modifying or distributing the Package, you accept this license. Do not
% use, modify, or distribute the Package, if you do not accept this
% license.
%
% (11) If your Modified Version has been derived from a Modified Version
% made by someone other than you, you are nevertheless required to
% ensure that your Modified Version complies with the requirements of
% this license.
%
% (12) This license does not grant you the right to use any trademark,
% service mark, tradename, or logo of the Copyright Holder.
%
% (13) This license includes the non-exclusive, worldwide,
% free-of-charge patent license to make, have made, use, offer to sell,
% sell, import and otherwise transfer the Package with respect to any
% patent claims licensable by the Copyright Holder that are necessarily
% infringed by the Package. If you institute patent litigation
% (including a cross-claim or counterclaim) against any party alleging
% that the Package constitutes direct or contributory patent
% infringement, then this Artistic License to you shall terminate on the
% date that such litigation is filed.
%
% (14) Disclaimer of Warranty: THE PACKAGE IS PROVIDED BY THE COPYRIGHT
% HOLDER AND CONTRIBUTORS "AS IS' AND WITHOUT ANY EXPRESS OR IMPLIED
% WARRANTIES. THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
% PARTICULAR PURPOSE, OR NON-INFRINGEMENT ARE DISCLAIMED TO THE EXTENT
% PERMITTED BY YOUR LOCAL LAW. UNLESS REQUIRED BY LAW, NO COPYRIGHT
% HOLDER OR CONTRIBUTOR WILL BE LIABLE FOR ANY DIRECT, INDIRECT,
% INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING IN ANY WAY OUT OF THE USE
% OF THE PACKAGE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include <stdlib.h>

/* -------------------------- */
/*      Local Procedures      */
/* -------------------------- */

static void          simplification_reduction(TrEntry trie);
static TrNode        depth_reduction(TrEntry trie, TrNode depth_node, YAP_Int opt_level);
static TrNode        breadth_reduction(TrEntry trie, TrNode breadth_node, YAP_Int opt_level);
static inline int    compare_label_nodes(TrData data1, TrData data2);
static inline void   move_after(TrData data_source, TrData data_dest);
static inline void   move_last_data_after(TrData moveto_data);
static inline void   set_depth_breadth_reduction_current_data(TrData data);


/* -------------------------- */
/*       Local Variables      */
/* -------------------------- */

static TrData  CURRENT_DEPTH_BREADTH_DATA;


/* -------------------------- */
/*            API             */     
/* -------------------------- */


YAP_Term trie_depth_breadth(TrEntry trie, TrEntry db_trie, YAP_Int opt_level, YAP_Int start_counter, YAP_Int *end_counter) {
  TrNode depth_node, breadth_node, nested_trie;
  TrEntry otrie = CURRENT_TRIE;
  core_set_label_counter(start_counter);
  CURRENT_TRIE = db_trie;
  core_set_trie_db_return_term(YAP_MkAtomTerm(YAP_LookupAtom("false")));
  core_initialize_depth_breadth_trie(TrEntry_trie(db_trie), &depth_node, &breadth_node);
  set_depth_breadth_reduction_current_data(NULL);
  /* We only need to simplify the trie once! */
  /* This can be a 10% overhead for sld cases :-( */
//  printf("simplification\n"); trie_print(trie);
  if (TrNode_child(TrEntry_trie(trie)))
    simplification_reduction(trie);
  while (TrNode_child(TrEntry_trie(trie))) {
//  printf("depth\n"); trie_print(trie);
    nested_trie = depth_reduction(trie, depth_node, opt_level);
    if (nested_trie) {
      set_depth_breadth_reduction_current_data(get_data_from_trie_node(nested_trie));
      core_finalize_depth_breadth_trie(depth_node, breadth_node);
      *end_counter = core_get_label_counter();
      CURRENT_TRIE = otrie;
      return YAP_MkApplTerm((YAP_Functor)(~ApplTag & TrNode_entry(TrNode_parent(nested_trie))), 1, &TrNode_entry(nested_trie));
    }
//  printf("breadth\n"); trie_print(trie);
    nested_trie = breadth_reduction(trie, breadth_node, opt_level);
    if (nested_trie) {
      set_depth_breadth_reduction_current_data(get_data_from_trie_node(nested_trie));
      core_finalize_depth_breadth_trie(depth_node, breadth_node);
      *end_counter = core_get_label_counter();
      CURRENT_TRIE = otrie;
      return YAP_MkApplTerm((YAP_Functor)(~ApplTag & TrNode_entry(TrNode_parent(nested_trie))), 1, &TrNode_entry(nested_trie));
    }
  }
  core_finalize_depth_breadth_trie(depth_node, breadth_node);
  *end_counter = core_get_label_counter();
  CURRENT_TRIE = otrie;
  return core_get_trie_db_return_term();
}



YAP_Int trie_get_db_opt_level_count(YAP_Int opt_level) {
  return core_db_trie_get_optimization_level_count(opt_level);
}



TrData trie_get_depth_breadth_reduction_current_data(void) {
  return CURRENT_DEPTH_BREADTH_DATA;
}



void trie_replace_nested_trie(TrEntry trie, YAP_Int nested_trie_id, YAP_Term new_term) {
  TrEntry otrie = CURRENT_TRIE;
  CURRENT_TRIE = trie;
  core_depth_breadth_trie_replace_nested_trie(TrNode_child(TrEntry_trie(trie)), nested_trie_id, new_term, &trie_data_construct, &trie_data_destruct);
  CURRENT_TRIE = otrie;
  return;
}



YAP_Int trie_get_db_opt_min_prefix(void) {
  return core_get_trie_db_opt_min_prefix();
}



void trie_set_db_opt_min_prefix(YAP_Int min_prefix) {
  core_set_trie_db_opt_min_prefix(min_prefix);
  return;
}


/* -------------------------- */
/*      Local Procedures      */
/* -------------------------- */


static inline
void set_depth_breadth_reduction_current_data(TrData data) {
  CURRENT_DEPTH_BREADTH_DATA = data;
  return;
}


static
void simplification_reduction(TrEntry trie) {
  TrNode node;
  TrData stop_data, new_data, data = NULL;
  stop_data = TrData_previous(TrEntry_first_data(trie));
  data = TrEntry_traverse_data(trie) = TrEntry_last_data(trie);
  while (data && (data != stop_data) && (TrData_trie(data))) {
    node = core_simplification_reduction(TRIE_ENGINE, TrData_leaf(data), &trie_data_destruct);
    if (node) {
      new_trie_data(new_data, trie, node);
      PUT_DATA_IN_LEAF_TRIE_NODE(node, new_data);
      data = NULL;
    }
    if (data && TrEntry_traverse_data(trie) && TrEntry_traverse_data(trie) != stop_data && !TrData_trie(data)) {
      data = TrData_previous(data);
      TrEntry_traverse_data(trie) = data;
    } else if (data && TrEntry_traverse_data(trie) && TrEntry_traverse_data(trie) != stop_data && data == TrEntry_traverse_data(trie)) {
      data = TrData_previous(data);
      TrEntry_traverse_data(trie) = data;
    } else
      data = TrEntry_traverse_data(trie);
  }
}


static
TrNode depth_reduction(TrEntry trie, TrNode depth_node, YAP_Int opt_level) {
  TrNode node;
  TrData stop_data, new_data, data = NULL;

  stop_data = TrData_previous(TrEntry_first_data(trie));
  data = TrEntry_traverse_data(trie) = TrEntry_last_data(trie);
  while (data && (data != stop_data) && (TrData_trie(data))) {
    node = core_depth_reduction(TRIE_ENGINE, TrData_leaf(data), depth_node, opt_level, &trie_data_construct, &trie_data_destruct, &trie_data_copy, &trie_data_order_correction);
    if (node && IS_FUNCTOR_NODE(TrNode_parent(node)) && (strcmp(YAP_AtomName(YAP_NameOfFunctor((YAP_Functor)(~ApplTag & TrNode_entry(TrNode_parent(node))))), NESTED_TRIE_TERM) == 0)) {
      //nested trie stop procedure return nested trie node
      return node;
    }
    if (node) {
      new_trie_data(new_data, trie, node);
      PUT_DATA_IN_LEAF_TRIE_NODE(node, new_data);
      data = NULL;
    }
    if (data && TrEntry_traverse_data(trie) && TrEntry_traverse_data(trie) != stop_data && !TrData_trie(data)) {
      data = TrData_previous(data);
      TrEntry_traverse_data(trie) = data;
    } else if (data && TrEntry_traverse_data(trie) && TrEntry_traverse_data(trie) != stop_data && data == TrEntry_traverse_data(trie)) {
      data = TrData_previous(data);
      TrEntry_traverse_data(trie) = data;
    } else
      data = TrEntry_traverse_data(trie);
  }
  return NULL;
}


static
TrNode breadth_reduction(TrEntry trie, TrNode breadth_node, YAP_Int opt_level) {
  TrNode node;
  TrData stop_data, new_data, data = NULL;

  stop_data = TrData_previous(TrEntry_first_data(trie));
  data = TrEntry_traverse_data(trie) = TrEntry_last_data(trie);
  while (data && (data != stop_data) && (TrData_trie(data))) {
    node = core_breadth_reduction(TRIE_ENGINE, TrData_leaf(data), breadth_node, opt_level, &trie_data_construct, &trie_data_destruct, &trie_data_copy, &trie_data_order_correction);
    if (node && IS_FUNCTOR_NODE(TrNode_parent(node)) && (strcmp(YAP_AtomName(YAP_NameOfFunctor((YAP_Functor)(~ApplTag & TrNode_entry(TrNode_parent(node))))), NESTED_TRIE_TERM) == 0)) {
      //nested trie stop procedure return nested trie node
      return node;
    }
    if (node) {
      new_trie_data(new_data, trie, node);
      PUT_DATA_IN_LEAF_TRIE_NODE(node, new_data);
      data = NULL;
    }
    if (data && TrEntry_traverse_data(trie) && TrEntry_traverse_data(trie) != stop_data && !TrData_trie(data)) {
      data = TrData_previous(data);
      TrEntry_traverse_data(trie) = data;
    } else if (data && TrEntry_traverse_data(trie) && TrEntry_traverse_data(trie) != stop_data && data == TrEntry_traverse_data(trie)) {
      data = TrData_previous(data);
      TrEntry_traverse_data(trie) = data;
    } else
      data = TrEntry_traverse_data(trie);
  }
  return NULL;
}


static inline
void move_last_data_after(TrData moveto_data) {
  TrEntry trie = CURRENT_TRIE;
  TrData last_data = TrEntry_last_data(trie);
  TrEntry_last_data(trie) = TrData_previous(last_data);
  TrData_next(TrData_previous(last_data)) = TrData_next(last_data);
  if (moveto_data == TrData_previous(TrEntry_first_data(trie))) {
    TrData_next(last_data) = TrEntry_first_data(trie);
    TrEntry_first_data(trie) = last_data;
  } else {
    TrData_next(last_data) = TrData_next(moveto_data);
    TrData_next(moveto_data) = last_data;
  }
  TrData_previous(last_data) = moveto_data;
  TrData_previous(TrData_next(last_data)) = last_data;
  return;
}


static inline
void move_after(TrData data_source, TrData data_dest) {
  TrEntry trie = CURRENT_TRIE;
  if (data_source == TrEntry_first_data(trie))
    TrEntry_first_data(trie) = TrData_next(data_source);
  else
    TrData_next(TrData_previous(data_source)) = TrData_next(data_source);
  if (data_source == TrEntry_last_data(trie))
    TrEntry_last_data(trie) = TrData_previous(data_source);
  else
    TrData_previous(TrData_next(data_source)) = TrData_previous(data_source);
  
  if (data_dest == TrData_previous(TrEntry_first_data(trie))) {
    TrData_next(data_source) = TrEntry_first_data(trie);
    TrData_previous(TrEntry_first_data(trie)) = data_source;
    TrEntry_first_data(trie) = data_source;
  } else {
    TrData_next(data_source) = TrData_next(data_dest);
    if (data_dest == TrEntry_last_data(trie))
      TrEntry_last_data(trie) = data_source;
    else
      TrData_previous(TrData_next(data_dest)) = data_source;
    TrData_next(data_dest) = data_source;
  }
  TrData_previous(data_source) = data_dest;
  return;
}


void trie_data_order_correction(void) {
  TrEntry trie = CURRENT_TRIE;
  TrData inserted_data = TrEntry_last_data(trie);
  TrData moved_data = TrData_previous(inserted_data);
  TrData moveto_data = TrData_previous(moved_data);

  while((moveto_data != TrData_previous(TrEntry_first_data(trie))) && (compare_label_nodes(moveto_data, inserted_data) == 1)) {
    if (compare_label_nodes(moveto_data, moved_data) == 2)
      moved_data = moveto_data;
    moveto_data = TrData_previous(moveto_data);
  }
  move_last_data_after(moveto_data);

  do {
    moveto_data = TrData_next(inserted_data);
    while(compare_label_nodes(moveto_data, moved_data) == 2)
      moveto_data = TrData_next(moveto_data);
    inserted_data = moved_data;
    moved_data = TrData_next(moved_data);
    if (moved_data != moveto_data)
      move_after(inserted_data, TrData_previous(moveto_data));
  } while(moved_data);
  return;
}


static inline
int compare_label_nodes(TrData data1, TrData data2) {
  YAP_Term t1 = TrNode_entry(TrData_leaf(data1)), t2 = TrNode_entry(TrData_leaf(data2));
  YAP_Int i1 = atol(YAP_AtomName(YAP_AtomOfTerm(t1)) + 1), i2 = atol(YAP_AtomName(YAP_AtomOfTerm(t2)) + 1);
  if (i1 == i2) return 0;
  if (i1 > i2) return 1;
  return 2;
}
