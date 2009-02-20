
/*chamada a cada index/2
  controi estrutura de control, para definir a indexação, contem a
  rtree p.e.
  retorna a estrutura de control
*/
typedef void *
(* Yap_UdiInit)(
		Term  spec,  /* mode spec */
		void *pred, /* pass predicate information */
		int   arity);

/*chamada a cada assert*/
typedef void *
(* Yap_UdiInsert)(Term t, /* termo asserted */
		  void *control, /* estrutura de control*/
		  void *clausule); /* valor a guardar na arvore, para retornar na pesquisa */

/* chamada cada vez que um predicado indexado aparece no código
   Returns:
       NULL quando não há indexação usavel no predicado (fallback to
yap indexing)
       FALSE
       TRY_RETRY_TRUST quando há resultados positivos
*/
typedef void *
(* Yap_UdiSearch)(void * control);

/* chamada cada vez que um predicado indexado aparece no código
   Returns:
       NULL quando não há indexação usavel no predicado (fallback to
yap indexing)
       FALSE
       TRY_RETRY_TRUST quando há resultados positivos
*/
typedef int
(* Yap_UdiDestroy)(void * control);

typedef struct udi_control_block {
  Yap_UdiInit   init;
  Yap_UdiInsert insert;
  Yap_UdiSearch search;
  Yap_UdiDestroy destroy;
} *UdiControlBlock;

