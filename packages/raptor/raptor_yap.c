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
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "YapInterface.h"
#include "raptor_config.h"
#ifdef HAVE_RAPTOR2_RAPTOR2_H
#include "raptor2/raptor2.h"
#else
#include "raptor2.h"
#endif

X_API void raptor_yap_init(void);

raptor_world *world;

struct exo_aux {
  YAP_Functor functor;
  YAP_PredEntryPtr pred;
  size_t n;
};

static YAP_Atom term_load(const raptor_term *term) {
  size_t len;
  switch (term->type) {
  case RAPTOR_TERM_TYPE_LITERAL:
    //        fprintf(stderr, "%s,", term->value.literal.string);
    return YAP_LookupAtom((const char *)term->value.literal.string);

  case RAPTOR_TERM_TYPE_BLANK:
    //        fprintf(stderr, "%s,", term->value.blank.string);
    return YAP_LookupAtom((const char *)term->value.blank.string);

  case RAPTOR_TERM_TYPE_URI:
    //        fprintf(stderr, "%s,",
    //        raptor_uri_as_counted_string(term->value.uri, &len));
    return YAP_LookupAtom(
        (const char *)raptor_uri_as_counted_string(term->value.uri, &len));

  case RAPTOR_TERM_TYPE_UNKNOWN:
  default:

      raptor_log_error_formatted(term->world, RAPTOR_LOG_LEVEL_ERROR, NULL,
                               "Triple has unsupported term type %d",
                               term->type);
    break;
  }

  return NULL;
}

static int so_far = 0;

static void load_triples(void *user_data, raptor_statement *triple) {
  struct exo_aux *aux = (struct exo_aux *)user_data;
  YAP_Term args[4];

  // args[0] = (YAP_CELL)aux->functor;
  args[0] = YAP_MkAtomTerm(term_load(triple->subject));
  args[1] = YAP_MkAtomTerm(term_load(triple->predicate));
  args[2] = YAP_MkAtomTerm(term_load(triple->object));
  //  fprintf(stderr, "\n");

  YAP_AssertTuples(aux->pred, args, so_far++, 1);
}

static void count_triples(void *user_data, raptor_statement *triple) {
  unsigned int *count_p = (unsigned int *)user_data;
  (*count_p)++;

  term_load(triple->subject);
  term_load(triple->predicate);
  term_load(triple->object);
  //  fprintf(stderr, "\n");
}

static YAP_Bool load(void) {
  YAP_Term tfn = YAP_ARG1;
  YAP_Term mod = YAP_ARG2;
  YAP_Term tfunctor = YAP_ARG3;
  const char *filename;

  raptor_parser *rdf_parser = NULL;
  unsigned int count;
  unsigned char *uri_string;
  raptor_uri *uri, *base_uri;

  if (YAP_IsVarTerm(tfn) || !YAP_IsAtomTerm(tfn)) {
    return FALSE;
  }

  filename = YAP_AtomName(YAP_AtomOfTerm(tfn));

  rdf_parser = raptor_new_parser(world, "rdfxml");

  raptor_parser_set_statement_handler(rdf_parser, &count, count_triples);

  uri_string = raptor_uri_filename_to_uri_string(filename);
  uri = raptor_new_uri(world, uri_string);
  base_uri = raptor_uri_copy(uri);

  count = 0;
  if (!raptor_parser_parse_file(rdf_parser, uri, base_uri)) {
    //      fprintf(stderr, "%s : %d triples\n", filename, count);
  } else {
    fprintf(stderr, "%s : failed to parse\n", filename);
    return FALSE;
  }

  /* now lets load */
  {
    struct exo_aux aux;
    size_t sz;

    aux.functor = YAP_MkFunctor(YAP_AtomOfTerm(tfunctor), 3);
    aux.pred = YAP_FunctorToPredInModule(aux.functor, mod);
    sz = 3 * sizeof(YAP_CELL) * count;

    if (!YAP_NewExo(aux.pred, sz, NULL)) {
      fprintf(stderr, "Failed to alocate space\n");
      return FALSE;
    }

    aux.n = 0;
    raptor_parser_set_statement_handler(rdf_parser, (void *)&aux, load_triples);
    if (!raptor_parser_parse_file(rdf_parser, uri, base_uri)) {
      fprintf(stderr, "%s : %d triples\n", filename, count);
    }
  }

  raptor_free_uri(base_uri);
  raptor_free_uri(uri);
  raptor_free_memory(uri_string);

  raptor_free_parser(rdf_parser);

  return TRUE;
}

static inline void raptor_yap_halt(int exit, void *world) {
  raptor_free_world((raptor_world *)world);
}

X_API void raptor_yap_init(void) {
  world = raptor_new_world();
  YAP_HaltRegisterHook(raptor_yap_halt, (void *)world);

  YAP_UserCPredicate("rdf_load", load, 3);
}
