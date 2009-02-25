#ifndef _RTREE_UDI_
#define _RTREE_UDI_

#ifndef _RTREE_
typedef void control_t;
#endif

/*Prolog term from :- udi(a(-,+,+)).
  User defined index announce
*/
extern control_t *RtreeUdiInit (Term spec,
                                void *pred,
                                int arity);

/*this is called in each asserted term that was declared to udi_init*/
extern control_t *RtreeUdiInsert (Term term, /*asserted term*/
                                  control_t *control,
                                  void *clausule); /*to store in tree and return
                                                     in search*/

extern void *RtreeUdiSearch (control_t *control);
extern int RtreeUdiDestroy(control_t *control);

#endif /* _RTREE_UDI_ */
