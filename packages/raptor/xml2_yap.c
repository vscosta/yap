/* Copyright (C) 2013 David Vaz <davidvaz@dcc.fc.up.pt>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General  Public  License
 * as published by the Free Software Foundation; either  version
 * 2 of the License, or (at your option) any later version.
 *
 * This program is distributed in  the  hope  that  it  will  be
 * useful, but WITHOUT ANY WARRANTY; without  even  the  implied
 * warranty of  MERCHANTABILITY  or  FITNESS  FOR  A  PARTICULAR
 * PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of  the  GNU  General  Public
 * License along with this program; if not, write  to  the  Free
 * Software Foundation, Inc., 51 Franklin Street,  Fifth  Floor,
 * Boston, MA 02110-1301, USA.
 */
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>


#include "Yap.h"
#include "Yatom.h"

#include <libxml/parser.h>
#include <libxml/tree.h>

X_API void libxml2_yap_init (void);

struct exo_aux {
  YAP_Functor functor;
  YAP_PredEntryPtr pred;
  size_t n;
};


static Term  read_atts(xmlAttr *att_node, sigjmp_buf *ji USES_REGS)
{
  if (att_node == NULL)
    return TermNil;
  Term ttail = read_atts(att_node->next, ji PASS_REGS);
  Term thead;
  Term tf;
    if (HR > ASP-1024)
      siglongjmp(*ji, 2);
  if (att_node->children) {
    if (att_node->children->type == XML_TEXT_NODE) {
      Term ts[2];
      ts[0] = MkAtomTerm(Yap_LookupAtom((const char *)att_node->name));
      ts[1] = MkStringTerm((const char *)att_node->children->content);
      thead = Yap_MkApplTerm(FunctorEq, 2, ts );
      tf = MkPairTerm(thead, ttail);
    } else {
      // error
      tf = TermNil;
    }
  } else {
    tf = MkAtomTerm(Yap_LookupAtom((const char *)att_node->name));
  }    
  if (att_node->ns) {
    Term ts[2];
    ts[0] = MkAtomTerm(Yap_LookupAtom((const char *)att_node->ns->prefix));
    ts[1] = tf;
    tf = Yap_MkApplTerm( FunctorModule, 2, ts );
  }
  return tf;
}

/**
 * load_element_names:
 * @a_node: the initial xml node to consider.
 *
 * Prints the names of the all the xml elements
 * that are siblings or children of a given xml node.
 */
static Term
print_element_names(xmlNode * a_node, sigjmp_buf *ji USES_REGS)
{
    xmlNode *cur_node = NULL;
    int count = 0;

    for (cur_node = a_node->children; cur_node; cur_node = cur_node->next) {
        if (cur_node->type == XML_ELEMENT_NODE) {
          count++;
        }
    }
    if (HR > ASP-(1024+count))
      siglongjmp(*ji, 1);
    Atom at = Yap_LookupAtom((char *)a_node->name);
    Functor f = Yap_MkFunctor(at, count+1);
    Term t = Yap_MkNewApplTerm(f, count+1);
    CELL *s = RepAppl(t) + 2;
    s[-1]  = read_atts(a_node->properties, ji PASS_REGS);
    int i = 0;
    for (cur_node = a_node->children; cur_node; cur_node = cur_node->next) {
      if (cur_node->type == XML_ELEMENT_NODE) {
        s[i++] =  print_element_names(cur_node, ji PASS_REGS);
      }
    }
    if (a_node->ns) {
      Term ts[2];
      ts[0] = MkAtomTerm(Yap_LookupAtom((const char *)a_node->ns->prefix));
      ts[1] = t;
      t = Yap_MkApplTerm( FunctorModule, 2, ts );
    }
    return t;
}

static Int
load_xml ( void )
{
  CACHE_REGS
    sigjmp_buf jmp_info;
   xmlDoc *doc = NULL;
    xmlNode *root_element = NULL;
    CELL *h0 = HR;
    
    const char *f = AtomOfTerm(Deref(ARG1))->StrOfAE;

    /*
     * this initialize the library and check potential ABI mismatches
     * between the version it was compiled for and the actual shared
     * library used.
     */
    LIBXML_TEST_VERSION

    /*parse the file and get the DOM */
    doc = xmlReadFile(f, NULL, 0);

    if (doc == NULL) {
      fprintf(stderr, "error: could not parse file %s\n", f);
      return false;
    }
    

    /*Get the root element node */
    root_element = xmlDocGetRootElement(doc);
    if (sigsetjmp(jmp_info, 0)) {
      HR = h0;
      Yap_gcl(0, 2, ENV, P);
      h0 = HR;
    }
    Term t = print_element_names(root_element, &jmp_info PASS_REGS);

    /*free the document */
    xmlFreeDoc(doc);

    /*
     *Free the global variables that may
     *have been allocated by the parser.
     */
    xmlCleanupParser();

    return Yap_unify(ARG2, t);
}

extern Int YAP_UserCPredicate(const char *, Int f(void), int arity);
 
X_API void libxml2_yap_init (void)
{
   YAP_UserCPredicate("load_xml", load_xml, 2);
   YAP_UserCPredicate("load_xml2", load_xml, 2);
}
