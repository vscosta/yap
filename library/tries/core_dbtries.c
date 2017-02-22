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

/* -------------------------- */
/*      Local Procedures      */
/* -------------------------- */

int      traverse_get_counter(TrNode node);
YAP_Term generate_label(YAP_Int Index);
YAP_Term update_depth_breadth_trie(TrEngine engine, TrNode root, YAP_Int opt_level, void (*construct_function)(TrNode), void (*destruct_function)(TrNode), void (*copy_function)(TrNode, TrNode), void (*correct_order_function)(void));
YAP_Term get_return_node_term(TrNode node);
void     traverse_and_replace_nested_trie(TrNode node, YAP_Int nested_trie_id, YAP_Term new_term, void (*construct_function)(TrNode), void (*destruct_function)(TrNode));
TrNode   replace_nested_trie(TrNode node, TrNode child, YAP_Term new_term, void (*construct_function)(TrNode), void (*destruct_function)(TrNode));
void     check_attach_childs(TrNode parent, TrNode search_child, TrNode existing_child, void (*construct_function)(TrNode), void (*destruct_function)(TrNode));
TrNode   get_simplification_sibling(TrNode node);
TrNode   check_parent_first(TrNode node);
TrNode   TrNode_myparent(TrNode node);

/* -------------------------- */
/*       Debug Procedures     */
/* -------------------------- */

void   displaynode(TrNode node);
void   displayentry(TrNode node);
void   displayterm(YAP_Term term);
void   displaytrie(TrNode node);
void   display_trie_inner(TrNode node);
void   trie_display_node(TrNode node);


/* -------------------------- */
/*       Local Variables      */
/* -------------------------- */

static YAP_Int LABEL_COUNTER;
static YAP_Term TRIE_DEPTH_BREADTH_RETURN_TERM;
static YAP_Int TRIE_DEPTH_BREADTH_MIN_PREFIX = 2;
static YAP_Int TRIE_DEPTH_BREADTH_OPT_COUNT[3];


/* -------------------------- */
/*     depth-breadth Trie     */
/* -------------------------- */


YAP_Int core_get_trie_db_opt_min_prefix(void) {
  return TRIE_DEPTH_BREADTH_MIN_PREFIX;
}



void core_set_trie_db_opt_min_prefix(YAP_Int min_prefix) {
  TRIE_DEPTH_BREADTH_MIN_PREFIX = min_prefix;
  return;
}



void core_depth_breadth_trie_replace_nested_trie(TrNode node, YAP_Int nested_trie_id, YAP_Term new_term, void (*construct_function)(TrNode), void (*destruct_function)(TrNode)) {
  traverse_and_replace_nested_trie(node, nested_trie_id, new_term, construct_function, destruct_function);
  return;
}


inline
void traverse_and_replace_nested_trie(TrNode node, YAP_Int nested_trie_id, YAP_Term new_term, void (*construct_function)(TrNode), void (*destruct_function)(TrNode)) {
  TrNode child, temp;
  if (TrNode_entry(node) == PairEndTag) {
    if (TrNode_next(node))
      traverse_and_replace_nested_trie(TrNode_next(node), nested_trie_id, new_term, construct_function, destruct_function);
    return;
  } else if (IS_HASH_NODE(node)) {
    printf("HASH NODE ERROR: db_tries do not support hash nodes.\n");
    abort();
    TrNode *first_bucket, *bucket;
    TrHash hash = (TrHash) node;
    first_bucket = TrHash_buckets(hash);
    bucket = first_bucket + TrHash_num_buckets(hash);
    do {
      if ((node = *--bucket)) {
        do {
          traverse_and_replace_nested_trie(node, nested_trie_id, new_term, construct_function, destruct_function);
          node = TrNode_next(node);
        } while(node);
      }
    } while (bucket != first_bucket);
  } else {
    if (IS_FUNCTOR_NODE(node)) {
      YAP_Functor f = (YAP_Functor) (~ApplTag & TrNode_entry(node));
      YAP_Int arity = YAP_ArityOfFunctor(f);
      if (arity == 1 && strcmp(YAP_AtomName(YAP_NameOfFunctor(f)), NESTED_TRIE_TERM) == 0) {
        child = TrNode_child(node);
        if (IS_HASH_NODE(child)) {
          printf("HASH NODE ERROR: db_tries do not support hash nodes.\n");
          abort();
          TrNode *first_bucket, *bucket;
          TrHash hash = (TrHash) child;
          first_bucket = TrHash_buckets(hash);
          bucket = first_bucket + TrHash_num_buckets(hash);
          do {
            if ((child = *--bucket)) {
              do {
                if (YAP_IntOfTerm(TrNode_entry(child)) == nested_trie_id) {
                  temp = TrNode_previous(node);
                  node = replace_nested_trie(node, child, new_term, construct_function, destruct_function);
                  if (temp) {
                    temp = TrNode_next(node);
                    if (temp)
                      node = temp;
                  } else {
                    traverse_and_replace_nested_trie(TrNode_child(node), nested_trie_id, new_term, construct_function, destruct_function);
                    return;
                  }
                }
                child = TrNode_next(child);
              } while(child);
            }
          } while (bucket != first_bucket);
       
        } else {
          do {
            if (YAP_IntOfTerm(TrNode_entry(child)) == nested_trie_id) {
              temp = TrNode_next(node);
              node = replace_nested_trie(node, child, new_term, construct_function, destruct_function);
              traverse_and_replace_nested_trie(TrNode_child(node), nested_trie_id, new_term, construct_function, destruct_function);
              if(temp)
                traverse_and_replace_nested_trie(temp, nested_trie_id, new_term, construct_function, destruct_function);
              return;
            }
            child = TrNode_next(child);
          } while(child);
        }
      }
    }
    traverse_and_replace_nested_trie(TrNode_child(node), nested_trie_id, new_term, construct_function, destruct_function);
    if (TrNode_next(node))
      traverse_and_replace_nested_trie(TrNode_next(node), nested_trie_id, new_term, construct_function, destruct_function);
  }
  return;
}

/* fixmeeee */
TrNode replace_nested_trie(TrNode node, TrNode child, YAP_Term new_term, void (*construct_function)(TrNode), void (*destruct_function)(TrNode)) {
  TrNode newnode, temp, newnodef = NULL;
  if (YAP_IsApplTerm(new_term)) {
    YAP_Term new_term_functor = ApplTag | ((YAP_Term) YAP_FunctorOfTerm(new_term));
    YAP_Int arity = YAP_ArityOfFunctor(YAP_FunctorOfTerm(new_term));
    if (arity != 1) abort();
    YAP_Term new_term_arg = YAP_ArgOfTerm(1, new_term);
    temp = TrNode_child(TrNode_parent(node));
    while (temp) {
      if (TrNode_entry(temp) == new_term_functor) {
        printf("Warning - non tested code, please report the example to Theo to test it!\n");
        newnodef = temp;
        temp = NULL;
      } else {
        temp = TrNode_next(temp);
      }
    }
    if (newnodef == NULL) {
      new_trie_node(newnodef, new_term_functor, TrNode_parent(node), NULL, TrNode_child(TrNode_parent(node)), NULL);
      TrNode_previous(TrNode_child(TrNode_parent(node))) = newnodef;
      TrNode_child(TrNode_parent(node)) = newnodef;
    }
    new_trie_node(newnode, new_term_arg, newnodef, TrNode_child(child), TrNode_child(newnodef), NULL);
    if (TrNode_child(newnodef))
      TrNode_previous(TrNode_child(newnodef)) = newnode;
    TrNode_child(newnodef) = newnode;
  } else {
    /* Check if one of the node siblings have new_term */
    temp = node;
    while (TrNode_previous(temp))
      temp = TrNode_previous(temp);
    while (temp && TrNode_entry(temp) != new_term)
      temp = TrNode_next(temp);
    if (temp) {
      newnode = temp;
      // Check if the childs of node/child exist already otherwise attach them
      check_attach_childs(newnode, TrNode_child(child), TrNode_child(newnode), construct_function, destruct_function);
      DATA_DESTRUCT_FUNCTION = destruct_function;
      remove_child_nodes(TrNode_child(child));
      TrNode_child(child) = NULL;
      remove_entry(child);
      return newnode;
    } else { // Make a new node
      new_trie_node(newnode, new_term, TrNode_parent(node), TrNode_child(child), TrNode_child(TrNode_parent(node)), NULL);
      TrNode_previous(TrNode_child(TrNode_parent(node))) = newnode;
      TrNode_child(TrNode_parent(node)) = newnode;
    }
  }
  temp = TrNode_child(child);
  if (IS_HASH_NODE(temp)) {
    printf("HASH NODE ERROR: db_tries do not support hash nodes.\n");
    abort();
    TrNode *first_bucket, *bucket;
    TrHash hash = (TrHash) temp;
    first_bucket = TrHash_buckets(hash);
    bucket = first_bucket + TrHash_num_buckets(hash);
    do {
      if ((temp = *--bucket)) {
        do {
          TrNode_parent(temp) = newnode;
          temp = TrNode_next(temp);
        } while(temp);
      }
    } while (bucket != first_bucket);
  } else {
    while (temp) {
      TrNode_parent(temp) = newnode;
      temp = TrNode_next(temp);
    }
  }
  DATA_DESTRUCT_FUNCTION = destruct_function;
  TrNode_child(child) = NULL;
  remove_entry(child);
  return newnode;
}


void check_attach_childs(TrNode parent, TrNode search_child, TrNode existing_child, void (*construct_function)(TrNode), void (*destruct_function)(TrNode)) {
  TrNode newnode;
  // Check if the childs of node/child exist already otherwise attach them
  do {
    while(existing_child && (TrNode_entry(existing_child) != PairEndTag) && (TrNode_entry(existing_child) != TrNode_entry(search_child)))
      existing_child = TrNode_next(existing_child);

    if (existing_child) {
      if (TrNode_entry(existing_child) != PairEndTag)
        check_attach_childs(existing_child, TrNode_child(search_child), TrNode_child(existing_child), construct_function, destruct_function);
      existing_child = TrNode_child(parent);
      search_child = TrNode_next(search_child);
    } else if (TrNode_entry(search_child) == PairEndTag) {
      newnode = parent;
      DATA_DESTRUCT_FUNCTION = destruct_function;
      remove_child_nodes(TrNode_child(newnode));
      TrNode_child(newnode) = NULL;
      newnode = trie_node_check_insert(newnode, PairEndTag);
      INCREMENT_ENTRIES(CURRENT_TRIE_ENGINE);
      (*construct_function)(newnode);
      return;
    } else {
      existing_child = search_child;
      search_child = TrNode_next(search_child);
      if(TrNode_child(TrNode_parent(existing_child)) == existing_child) {
        if(TrNode_next(existing_child)) {
          TrNode_child(TrNode_parent(existing_child)) = TrNode_next(existing_child);
        } else {
          newnode = TrNode_parent(existing_child);
//          DATA_DESTRUCT_FUNCTION = destruct_function;
//          remove_child_nodes(TrNode_child(newnode));
          TrNode_child(newnode) = NULL;
          newnode = trie_node_check_insert(newnode, PairEndTag);
          INCREMENT_ENTRIES(CURRENT_TRIE_ENGINE);
          (*construct_function)(newnode);
        }
      }

      if (TrNode_next(existing_child))
        TrNode_previous(TrNode_next(existing_child)) = TrNode_previous(existing_child);
      if (TrNode_previous(existing_child))
        TrNode_next(TrNode_previous(existing_child)) = TrNode_next(existing_child);

      TrNode_parent(existing_child) = parent;
      TrNode_previous(existing_child) = NULL;
      TrNode_next(existing_child) = TrNode_child(parent);
      TrNode_previous(TrNode_child(parent)) = existing_child;
      TrNode_child(parent) = existing_child;
      existing_child = TrNode_child(parent);
    }
  } while(search_child);
}


YAP_Term  core_get_trie_db_return_term(void) {
  return TRIE_DEPTH_BREADTH_RETURN_TERM;
}



void core_set_trie_db_return_term(YAP_Term return_value){
  TRIE_DEPTH_BREADTH_RETURN_TERM = return_value;
  return;
}



void core_set_label_counter(YAP_Int value) {
  LABEL_COUNTER = value;                            // Initialize the counter
  return;
}


YAP_Int core_get_label_counter(void) {
  return LABEL_COUNTER;
}


void core_initialize_depth_breadth_trie(TrNode node, TrNode *depth_node, TrNode *breadth_node) {
  TrNode root = node;
  YAP_Functor f;
  f = YAP_MkFunctor(YAP_LookupAtom("depth"),2);
  node = trie_node_check_insert(root, ApplTag | ((YAP_Term) f));
  *depth_node = trie_node_check_insert(node, PairInitTag);
  f = YAP_MkFunctor(YAP_LookupAtom("breadth"),2);
  node = trie_node_check_insert(root, ApplTag | ((YAP_Term) f));
  *breadth_node = trie_node_check_insert(node, PairInitTag);
  TRIE_DEPTH_BREADTH_OPT_COUNT[0] = 0;
  TRIE_DEPTH_BREADTH_OPT_COUNT[1] = 0;
  TRIE_DEPTH_BREADTH_OPT_COUNT[2] = 0;
  return;
}



void core_finalize_depth_breadth_trie(TrNode depth_node, TrNode breadth_node) {
  depth_node = trie_node_check_insert(depth_node, YAP_MkIntTerm(1));
  depth_node = trie_node_check_insert(depth_node, PairEndTag);
  depth_node = trie_node_check_insert(depth_node, YAP_MkIntTerm(1));
  remove_entry(depth_node);
  breadth_node = trie_node_check_insert(breadth_node, YAP_MkIntTerm(1));
  breadth_node = trie_node_check_insert(breadth_node, PairEndTag);
  breadth_node = trie_node_check_insert(breadth_node, YAP_MkIntTerm(1));
  remove_entry(breadth_node);
  return;
}


TrNode get_simplification_sibling(TrNode node) {
  TrNode sibling = node;
  while (sibling != NULL && TrNode_entry(sibling) != PairEndTag)
    sibling = TrNode_next(sibling);
  if (sibling != NULL && TrNode_entry(sibling) == PairEndTag) return sibling;
  sibling = node;
  while (sibling != NULL && TrNode_entry(sibling) != PairEndTag)
    sibling = TrNode_previous(sibling);
  return sibling;
}

TrNode check_parent_first(TrNode node) {
  TrNode simplification;
  if (TrNode_entry(TrNode_myparent(node)) != PairInitTag) {
    simplification = check_parent_first(TrNode_myparent(node));
    if (simplification != NULL && TrNode_entry(simplification) == PairEndTag) return simplification;
  }
  simplification = get_simplification_sibling(node);
  return simplification;
}

TrNode TrNode_myparent(TrNode node) {
  TrNode parent = TrNode_parent(node);
  while (parent != NULL && IS_FUNCTOR_NODE(parent))
    parent = TrNode_parent(parent);
  return parent;
}

TrNode core_simplification_reduction(TrEngine engine, TrNode node, void (*destruct_function)(TrNode)) {
  /* Try to find the greatest parent that has a sibling that is a PairEndTag: this indicates a deep simplification */
  node = check_parent_first(TrNode_myparent(node));
  if (node != NULL) {
    /* do breadth reduction simplification */
    node = TrNode_parent(node);
    DATA_DESTRUCT_FUNCTION = destruct_function;
    remove_child_nodes(TrNode_child(node));
    TrNode_child(node) = NULL;
    node = trie_node_check_insert(node, PairEndTag);
    INCREMENT_ENTRIES(CURRENT_TRIE_ENGINE);
  }
  return node;
}


TrNode core_depth_reduction(TrEngine engine, TrNode node, TrNode depth_node, YAP_Int opt_level, void (*construct_function)(TrNode), void (*destruct_function)(TrNode), void (*copy_function)(TrNode, TrNode), void (*correct_order_function)(void)) {
  TrNode leaf = node;
  YAP_Term t, *stack_top;
  int count = -1;
  /* collect depth nodes */
  stack_args_base = stack_args = AUXILIARY_TERM_STACK;
  stack_top = AUXILIARY_TERM_STACK + CURRENT_AUXILIARY_TERM_STACK_SIZE - 1;
  do {
    node = TrNode_parent(node);
    if (TrNode_entry(node) == PairInitTag) {
      node = TrNode_child(node);
      break;
    }
    //Nested Trie code
    if (IS_FUNCTOR_NODE(TrNode_parent(node)) && (strcmp(YAP_AtomName(YAP_NameOfFunctor((YAP_Functor)(~ApplTag & TrNode_entry(TrNode_parent(node))))), NESTED_TRIE_TERM) == 0)) {
      /* nested trie: stop procedure and return nested trie node */
      return node;
    }
    PUSH_DOWN(stack_args, TrNode_entry(node), stack_top);
    if (!IS_FUNCTOR_NODE(node))
      count++;
  } while (TrNode_next(node) == NULL && TrNode_child(TrNode_parent(node)) == node);
  if (!count)
    return NULL;
  while (IS_FUNCTOR_NODE(TrNode_parent(node))) {
    node = TrNode_parent(node);
    PUSH_DOWN(stack_args, TrNode_entry(node), stack_top);
  }
  TrNode temp = TrNode_child(TrNode_parent(node));
  if (IS_HASH_NODE(temp)) {
    printf("HASH NODE ERROR: db_tries do not support hash nodes.\n");
    abort();
    TrNode *first_bucket, *bucket;
    TrHash hash = (TrHash) temp;
    first_bucket = TrHash_buckets(hash);
    bucket = first_bucket + TrHash_num_buckets(hash);
    do {
      if ((temp = *--bucket)) {
        while(TrNode_next(temp) != NULL) {
          if (TrNode_entry(temp) == PairEndTag)
            return NULL;
          temp = TrNode_next(temp);
        }
      }
    } while (bucket != first_bucket);
  } else {
    while(TrNode_next(temp) != NULL) {
      if (TrNode_entry(temp) == PairEndTag)
        return NULL;
      temp = TrNode_next(temp);
    }
  }

  t = update_depth_breadth_trie(engine, depth_node, opt_level, construct_function, destruct_function, copy_function, correct_order_function);

  /* do depth reduction */
  DATA_DESTRUCT_FUNCTION = destruct_function;
  node = trie_node_check_insert(TrNode_parent(node), t);
  node = trie_node_check_insert(node, PairEndTag);
  INCREMENT_ENTRIES(CURRENT_TRIE_ENGINE);
  temp = TrNode_parent(leaf);
  remove_child_nodes(TrNode_child(temp));
  TrNode_child(temp) = NULL;
  remove_entry(temp);
  return node;
}



TrNode core_breadth_reduction(TrEngine engine, TrNode node, TrNode breadth_node, YAP_Int opt_level, void (*construct_function)(TrNode), void (*destruct_function)(TrNode), void (*copy_function)(TrNode, TrNode), void (*correct_order_function)(void)) {
  YAP_Term t, *stack_top;
  int count = -1;
  TrNode child;
  
  /* Simplification with breadth reduction (faster dbtrie execution worse BDD)
  child = core_simplification_reduction(engine, node, destruct_function);
  if (child) return child;
  */

  /* collect breadth nodes */
  stack_args_base = stack_args = AUXILIARY_TERM_STACK;
  stack_top = AUXILIARY_TERM_STACK + CURRENT_AUXILIARY_TERM_STACK_SIZE - 1;
  node = TrNode_parent(TrNode_parent(node));
  // printf("start node: "); displaynode(node);
  if (IS_FUNCTOR_NODE(node)) {
    while(IS_FUNCTOR_NODE(node))
      node = TrNode_parent(node);
    child = TrNode_child(node);
    while((TrNode_next(child) == NULL) && (TrNode_child(TrNode_parent(child)) == child) && (TrNode_entry(TrNode_child(child)) != PairEndTag))
      child = TrNode_child(child);
  } else
    child = TrNode_child(node);
  // printf("Chosen start node: "); displaynode(child);
  if (IS_HASH_NODE(child)) {
    printf("HASH NODE ERROR: db_tries do not support hash nodes.\n");
    abort();
    /* Comment code for HASH NODES - the commented code 100% has a bug
    TrNode *first_bucket, *bucket;
    TrHash hash = (TrHash) child;
    first_bucket = TrHash_buckets(hash);
    bucket = first_bucket + TrHash_num_buckets(hash);
    do {
      if ((child = *--bucket)) {
        do {
          if (TrNode_entry(child) == PairEndTag)
            return core_breadth_reduction(engine, child, breadth_node, opt_level, construct_function, destruct_function, copy_function, correct_order_function);
          while (IS_FUNCTOR_NODE(child)) {
            child = TrNode_child(child);
            if (IS_HASH_NODE(child)) { // gets first child in the hash
              TrNode *first_bucket2, *bucket2;
              TrHash hash2 = (TrHash) child;
              first_bucket2 = TrHash_buckets(hash2);
              bucket2 = first_bucket2 + TrHash_num_buckets(hash2);
              while(!(child = *--bucket2));
            }
          }
          TrNode temp = TrNode_child(child);
          if (temp == NULL)
            return NULL;
          if (IS_HASH_NODE(temp)) {
            TrNode *first_bucket2, *bucket2;
            TrHash hash2 = (TrHash) temp;
            first_bucket2 = TrHash_buckets(hash2);
            bucket2 = first_bucket2 + TrHash_num_buckets(hash2);
            do {
              if ((temp = *--bucket2)) {
                while((temp != NULL) && (TrNode_entry(temp) != PairEndTag))
                temp = TrNode_next(temp);
              }
            } while (bucket2 != first_bucket2 && temp == NULL);
          } else {
            while((temp != NULL) && (TrNode_entry(temp) != PairEndTag))
              temp = TrNode_next(temp);
          }
          //Nested Trie code
          if (IS_FUNCTOR_NODE(TrNode_parent(child)) && (strcmp(YAP_AtomName(YAP_NameOfFunctor((YAP_Functor)(~ApplTag & TrNode_entry(TrNode_parent(child))))), NESTED_TRIE_TERM) == 0)) {
            // nested trie: stop procedure and return nested trie node
            return child;
          }
          PUSH_DOWN(stack_args, TrNode_entry(child), stack_top);
          count++;
          if (IS_FUNCTOR_NODE(TrNode_parent(child))) {
            temp = TrNode_parent(child);
            while (IS_FUNCTOR_NODE(temp)) {
              PUSH_DOWN(stack_args, TrNode_entry(temp), stack_top);
              temp = TrNode_parent(temp);
            }
            while ((TrNode_next(child) == NULL) && IS_FUNCTOR_NODE(TrNode_parent(child)) && (bucket == first_bucket))
              child = TrNode_parent(child);
          }
          child = TrNode_next(child);
        } while (child);
      }
    } while (bucket != first_bucket);
    */
  } else {
    do {
      if (TrNode_entry(child) == PairEndTag) {
        /* do breadth reduction simplification */
        printf("SIMPLIFICATION ERROR: I should never arrive here, please contact Theo!\n");
        abort();
        /*
        node = TrNode_parent(child);
        DATA_DESTRUCT_FUNCTION = destruct_function;
        remove_child_nodes(TrNode_child(node));
        TrNode_child(node) = NULL;
        node = trie_node_check_insert(node, PairEndTag);
        INCREMENT_ENTRIES(CURRENT_TRIE_ENGINE);
        return node;
        */
      }
      while (IS_FUNCTOR_NODE(child)) {
        child = TrNode_child(child);
        if (IS_HASH_NODE(child)) { // gets first child in the hash
          printf("HASH NODE ERROR: db_tries do not support hash nodes.\n");
          abort();
          TrNode *first_bucket, *bucket;
          TrHash hash = (TrHash) child;
          first_bucket = TrHash_buckets(hash);
          bucket = first_bucket + TrHash_num_buckets(hash);
          while(!(child = *--bucket));
        }
      }
      if (TrNode_child(child) == NULL) return NULL;
      if (TrNode_entry(TrNode_child(child)) != PairEndTag) return NULL;
      /* nested trie: stop procedure and return nested trie node */
      if (IS_FUNCTOR_NODE(TrNode_parent(child)) && (strcmp(YAP_AtomName(YAP_NameOfFunctor((YAP_Functor)(~ApplTag & TrNode_entry(TrNode_parent(child))))), NESTED_TRIE_TERM) == 0))
        return child;
      PUSH_DOWN(stack_args, TrNode_entry(child), stack_top);
      count++;
      if (IS_FUNCTOR_NODE(TrNode_parent(child))) {
        TrNode temp = TrNode_parent(child);
        while (IS_FUNCTOR_NODE(temp)) {
          PUSH_DOWN(stack_args, TrNode_entry(temp), stack_top);
          temp = TrNode_parent(temp);
        }
        while ((TrNode_next(child) == NULL) && IS_FUNCTOR_NODE(TrNode_parent(child)))
          child = TrNode_parent(child);
      }
      child = TrNode_next(child);
    } while (child);
  }
  if (!count) {
    /* termination condition */
    core_set_trie_db_return_term(get_return_node_term(TrNode_child(node)));
    node = TrNode_parent(node);
    DATA_DESTRUCT_FUNCTION = destruct_function;
    remove_child_nodes(TrNode_child(node));
    TrNode_child(node) = NULL;
    return NULL;
  }

  t = update_depth_breadth_trie(engine, breadth_node, opt_level, construct_function, destruct_function, copy_function, correct_order_function);

  /* do breadth reduction */
  DATA_DESTRUCT_FUNCTION = destruct_function;
  remove_child_nodes(TrNode_child(node));
  TrNode_child(node) = NULL;
  node = trie_node_check_insert(node, t);
  node = trie_node_check_insert(node, PairEndTag);
  INCREMENT_ENTRIES(CURRENT_TRIE_ENGINE);
  return node;
}


inline
YAP_Term get_return_node_term(TrNode node) {
  YAP_Term args[1], t;
  if (IS_HASH_NODE(node)) {
    TrNode *first_bucket, *bucket;
    TrHash hash = (TrHash) node;
    first_bucket = TrHash_buckets(hash);
    bucket = first_bucket + TrHash_num_buckets(hash);
    while(!(node = *--bucket));
    t = TrNode_entry(node);
  } else if (IS_FUNCTOR_NODE(node)) {
    args[0] = get_return_node_term(TrNode_child(node));
    t = YAP_MkApplTerm((YAP_Functor)(~ApplTag & TrNode_entry(node)), 1, args);
  } else {
    t = TrNode_entry(node);
  }
  return t;
}


int traverse_get_counter(TrNode node) {
  int count = -1;
  while (TrNode_entry(node) != PairEndTag) {
    if (!IS_FUNCTOR_NODE(node))
      count++;
    node = TrNode_child(node);
    if (IS_HASH_NODE(node)) {
      TrNode *first_bucket, *bucket;
      TrHash hash = (TrHash) node;
      first_bucket = TrHash_buckets(hash);
      bucket = first_bucket + TrHash_num_buckets(hash);
      do {
        if ((node = *--bucket)) {
          while(TrNode_next(node) != NULL)
            node = TrNode_next(node);
        }
      } while (bucket != first_bucket);
    } else {
      while(TrNode_next(node) != NULL)
        node = TrNode_next(node);
    }
  }
  return atoi(YAP_AtomName(YAP_AtomOfTerm(TrNode_entry(TrNode_child(node)))) + 1) - count;
}


YAP_Term generate_label(YAP_Int Index) {
  char label[20];
  sprintf(label,"L%" Int_F, Index);
  return YAP_MkAtomTerm(YAP_LookupAtom(label));
}



YAP_Term update_depth_breadth_trie(TrEngine engine, TrNode root, YAP_Int opt_level, void (*construct_function)(TrNode), void (*destruct_function)(TrNode), void (*copy_function)(TrNode, TrNode), void (*correct_order_function)(void)) {
  TrNode node = root, remember = NULL;
  int count = -1, cnt = -1, c_cnt = 0, f_cnt = 0;
  YAP_Int BAK_CURRENT_TRIE_MODE = CURRENT_TRIE_MODE;
  YAP_Term t, tt, ret_t = PairEndTag;
  if (opt_level > 0)
    CURRENT_TRIE_MODE = TRIE_MODE_MINIMAL;
  else
    CURRENT_TRIE_MODE = TRIE_MODE_STANDARD;
  CURRENT_TRIE_ENGINE = engine;
  DATA_DESTRUCT_FUNCTION = destruct_function;
  DATA_COPY_FUNCTION = copy_function;
  do {
    t = POP_UP(stack_args);
    node = trie_node_check_insert(node, t);
    if (!IS_FUNCTOR_NODE(node))
      count++;
    if (opt_level > 0) {
      // Optimization 1: when asserting a non-minimal you can reuse minimal.
      if (TrNode_entry(node) == PairEndTag) {
        // Check to find the longest re-usage
        TrNode c_node = trie_node_check(TrNode_parent(node), t), end_node;
        c_cnt = 0;
        f_cnt = 0;
        while (c_node != NULL) {
          if (stack_args_base != stack_args) {
            c_cnt++;
            tt = POP_UP(stack_args);
            end_node = trie_node_check(c_node, PairEndTag);
            c_node = trie_node_check(c_node, tt);
            if ((c_node!= NULL) && !IS_FUNCTOR_NODE(c_node))
              f_cnt++;
            if ((end_node != NULL)) {
              count += f_cnt;
              f_cnt = 0;
              c_cnt = 0;
              t = tt;
              node = end_node;
            }
          } else {
            // reached the end
            node = c_node;
            c_node = NULL;
            count += f_cnt;
            f_cnt = 0;
            c_cnt = 0;
          }
        }
        stack_args += c_cnt;
      }
      if (TrNode_entry(node) == PairEndTag) {
        if (count > TRIE_DEPTH_BREADTH_MIN_PREFIX - 2) {
          TRIE_DEPTH_BREADTH_OPT_COUNT[0]++;
          cnt = -1; // reset optimization 3 counter
          count = 0;
          node = trie_node_check_insert(root, TrNode_entry(TrNode_child(node)));
          if (TrNode_child(node) != NULL)
            cnt++;
          node = trie_node_check_insert(node, t);
          if (!IS_FUNCTOR_NODE(node)) {
            count++;
            if (TrNode_child(node) != NULL)
              cnt++;
          }
        } else {
          CURRENT_TRIE_MODE = TRIE_MODE_STANDARD;
          node = trie_node_check_insert(TrNode_parent(node), t);
          CURRENT_TRIE_MODE = TRIE_MODE_MINIMAL;
        }
      }
    }
    if (opt_level > 2) {
      // Optimization 3: when asserting common prefix of size 2 or longer.
      // 1) remember last node and count
      // 2) after normal assertion ends go to remembered node, count
      // 3) assert PairEndTag to triger optimization 2
      if (TrNode_child(node) != NULL) {
        if (!IS_FUNCTOR_NODE(node))
          cnt++;
      } else {
        if ((remember == NULL) && (cnt > 0) && (cnt > TRIE_DEPTH_BREADTH_MIN_PREFIX - 2)) {
          TRIE_DEPTH_BREADTH_OPT_COUNT[1]--;
          TRIE_DEPTH_BREADTH_OPT_COUNT[2]++;
          remember = node;
          do {
            remember = TrNode_parent(remember);
          } while(IS_FUNCTOR_NODE(remember));
        }
      }
    }
  } while (stack_args_base != stack_args);
  do {
    t = PairEndTag;
    if (opt_level > 1) {
      // Optimization 2: when asserting a more minimal you can reuse it
      // a) Traverse and find the lowest L and the length of branch LN = (L? - (length - 1) + count)
      // b) insert LN
      // c) copy childs of node
      // d) remove childs of node
      // e) insert ]
      // f) insert LN
      // g) REORDER Entries
      if ((TrNode_child(node) != NULL) && (TrNode_entry(TrNode_child(node)) != PairEndTag) && (count > TRIE_DEPTH_BREADTH_MIN_PREFIX - 2)) {
        TRIE_DEPTH_BREADTH_OPT_COUNT[1]++;
        t = generate_label(traverse_get_counter(node));
        root = trie_node_check_insert(root, t);
        TrNode_child(root) = copy_child_nodes(root, TrNode_child(node));
        remove_child_nodes(TrNode_child(node));
        TrNode_child(node) = NULL;
      }
    }
    node = trie_node_check_insert(node, PairEndTag);

    if (t == PairEndTag) {
      if (TrNode_child(node)) {
        t = TrNode_entry(TrNode_child(node));
      } else {
        LABEL_COUNTER += count;
        t = generate_label(LABEL_COUNTER);
        node = trie_node_check_insert(node, t);
        INCREMENT_ENTRIES(CURRENT_TRIE_ENGINE);
        (*construct_function)(node);
      }
    } else {
      node = trie_node_check_insert(node, t);
      INCREMENT_ENTRIES(CURRENT_TRIE_ENGINE);
      (*construct_function)(node);
      (*correct_order_function)();
    }
    // Optimization 3: part 2
    node = remember;
    count = cnt;
    remember = NULL;
    if (ret_t == PairEndTag)
      ret_t = t;
  } while(node != NULL);

  CURRENT_TRIE_MODE = BAK_CURRENT_TRIE_MODE;
  return ret_t;
}



YAP_Int core_db_trie_get_optimization_level_count(YAP_Int opt_level) {
  return TRIE_DEPTH_BREADTH_OPT_COUNT[opt_level - 1];
}


/* -------------------------- */
/*       Debug Procedures     */
/* -------------------------- */

void displaynode(TrNode node) {
  if (node != NULL) {
    if (IS_HASH_NODE(node))
      printf("HASH n%i, b%i, p%p\n", TrHash_num_nodes((TrHash) node), TrHash_num_buckets((TrHash) node), node);
    else if (TrNode_entry(node) == PairInitTag)
      printf("PairInitTag\n");
    else if (TrNode_entry(node) == PairEndTag)
      printf("PairEndTag\n");
    else if (IS_FUNCTOR_NODE(node))
      printf("functor(%s)\n", YAP_AtomName(YAP_NameOfFunctor((YAP_Functor)( ~ApplTag & TrNode_entry(node)))));
    else if (YAP_IsIntTerm(TrNode_entry(node)))
      printf("int(%"Int_F")\n", YAP_IntOfTerm(TrNode_entry(node)));
    else if (YAP_IsAtomTerm(TrNode_entry(node)))
       printf("atom(%s)\n", YAP_AtomName(YAP_AtomOfTerm(TrNode_entry(node))));
    else
      printf("What?\n");
  } else
    printf("null\n");
  return;
}

void displayentry(TrNode node) {
  printf("Entry Contains Bottom Up:\n");
  while (node) {
    displaynode(node);
    node = TrNode_parent(node);
  }
  printf("--- End of Entry ---\n");
}

void displayterm(YAP_Term term) {
  if (term) {
   if (term == PairInitTag)
      printf("PairInitTag\n");
    else if (term == PairEndTag)
      printf("PairEndTag\n");
    else if (YAP_IsApplTerm(term))
      printf("functor(%s)\n", YAP_AtomName(YAP_NameOfFunctor((YAP_Functor)( ~ApplTag & term))));
    else if (YAP_IsIntTerm(term))
      printf("int(%"Int_F")\n", YAP_IntOfTerm(term));
    else if (YAP_IsAtomTerm(term))
       printf("atom(%s)\n", YAP_AtomName(YAP_AtomOfTerm(term)));
    else
      printf("What?\n");
  } else
    printf("null\n");
  return;
}

void displaytrie(TrNode node) {
  while(TrNode_entry(node) != PairInitTag){
    printf("?: "); displaynode(node);
    node = TrNode_parent(node);
  }
  display_trie_inner(node);
}

void display_trie_inner(TrNode node) {
  trie_display_node(node);
  if (TrNode_entry(node) != PairEndTag && TrNode_child(node))
    display_trie_inner(TrNode_child(node));
  if (TrNode_next(node)) {
    trie_display_node(TrNode_parent(node)); display_trie_inner(TrNode_next(node));
  }
}

void trie_display_node(TrNode node) {
  if (node != NULL) {
    if (IS_HASH_NODE(node))
      printf("HASH(n%i, b%i, p%p), ", TrHash_num_nodes((TrHash) node), TrHash_num_buckets((TrHash) node), node);
    else if (TrNode_entry(node) == PairInitTag)
      printf("PairInitTag, ");
    else if (TrNode_entry(node) == PairEndTag)
      printf("PairEndTag\n");
    else if (IS_FUNCTOR_NODE(node))
      printf("functor(%s), ", YAP_AtomName(YAP_NameOfFunctor((YAP_Functor)( ~ApplTag & TrNode_entry(node)))));
    else if (YAP_IsIntTerm(TrNode_entry(node)))
      printf("int(%" Int_F"), ", YAP_IntOfTerm(TrNode_entry(node)));
    else if (YAP_IsAtomTerm(TrNode_entry(node)))
       printf("atom(%s), ", YAP_AtomName(YAP_AtomOfTerm(TrNode_entry(node))));
    else
      printf("What?\n");
  } else
    printf("null\n");
  return;
}

