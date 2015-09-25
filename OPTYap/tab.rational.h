/************************************************************************
**                                                                     **
**                   The YapTab/YapOr/OPTYap systems                   **
**                                                                     **
** YapTab extends the Yap Prolog engine to support sequential tabling  **
** YapOr extends the Yap Prolog engine to support or-parallelism       **
** OPTYap extends the Yap Prolog engine to support or-parallel tabling **
**                                                                     **
**                                                                     **
**      Yap Prolog was developed at University of Porto, Portugal      **
**                                                                     **
************************************************************************/

#define RationalMark 7 //0m0...111
#define IsRationalTerm(TERM) ((int) TERM == 7)

typedef struct term_array {
  void* *terms;
  void* *nodes;
  size_t length;
  size_t capacity;
} term_array;

void term_array_init(term_array *array, int capacity);
void term_array_free(term_array *array);
void term_array_push(term_array *array, void* t, void* n);
void* term_array_member(term_array array, void* t);

void term_array_init(term_array *array, int capacity) {
  array->length = 0;
  array->terms = malloc(capacity * sizeof(void*));
  if (array->terms != NULL) {
    array->capacity = capacity;
  } else
    Yap_Error(RESOURCE_ERROR_HEAP, TermNil, "Out of memory."); // Handle out-of-memory
  array->capacity = capacity;
  array->nodes = malloc(capacity * sizeof(void*));
  if (array->nodes == NULL)
    Yap_Error(RESOURCE_ERROR_HEAP, TermNil, "Out of memory."); // Handle out-of-memory
}

void term_array_free(term_array *array) {
  free(array->terms);
  free(array->nodes);
  array->terms = NULL;
  array->nodes = NULL;
  array->length = 0;
  array->capacity = 0;
}

void term_array_push(term_array *array, void* t, void* n) {
  if (array->length == array->capacity) {
    int new_capacity = array->capacity * 2;
    void *new_terms = realloc(array->terms, new_capacity * sizeof(void*));
    if (new_terms != NULL) {
      array->terms = new_terms;
    } else
      Yap_Error(RESOURCE_ERROR_HEAP, TermNil, "Out of memory."); // Handle out-of-memory
    void *new_nodes = realloc(array->nodes, new_capacity * sizeof(void *));
    if (new_nodes != NULL) {
      array->nodes = new_nodes;
    } else
      Yap_Error(RESOURCE_ERROR_HEAP, TermNil, "Out of memory."); // Handle out-of-memory
    array->capacity = new_capacity;
  }
  array->terms[array->length] = t;
  array->nodes[array->length] = n;
  array->length++;
}

void* term_array_member(term_array array, void* t) {
  int i;
  for (i = 0; i < array.length; i++)
    if (array.terms[i] == t) return array.nodes[i];
  return NULL;
}

