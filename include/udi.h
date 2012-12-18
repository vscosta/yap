/*chamada a cada index/2
  controi estrutura de control, para definir a indexação, contem a
  rtree p.e.
  retorna a estrutura de control
*/
typedef void *
(* Yap_UdiInit)(
		YAP_Term  spec,  /* mode spec */
		void *pred, /* pass predicate information */
		int   arity);

/*chamada a cada assert*/
typedef void *
(* Yap_UdiInsert)(YAP_Term t, /* termo asserted */
		  void *control, /* estrutura de control*/
		  void *clausule); /* valor a guardar na arvore, para retornar na pesquisa */

/* Callback for each value found in a search */
typedef int /* with a FALSE return should abort the search */
(* Yap_UdiCallback) (void *key, /*index key*/
                     void *clausule, /*clause*/
                     void *arg); /* auxiliary to callback */

/* chamada cada vez que um predicado indexado aparece no código
   Returns:
       NULL quando não há indexação usavel no predicado (fallback to
yap indexing)
       FALSE
       TRY_RETRY_TRUST quando há resultados positivos
*/
typedef void *
(* Yap_UdiSearch)(void * control,
				  Yap_UdiCallback f, /* callback on each found value*/
		          void *arg);

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
  YAP_Atom decl; //atom that triggers this indexing structure
  Yap_UdiInit   init;
  Yap_UdiInsert insert;
  Yap_UdiSearch search;
  Yap_UdiDestroy destroy;
} *UdiControlBlock;

/* used to register the new indexing structure */
void Yap_UdiRegister(UdiControlBlock);
