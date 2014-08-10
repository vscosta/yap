#include <yaam_basics_d.h>
#include <yaam_primitive_predicates_d.h>
#include <yaam_call_d.h>
#include <yaam_call_count_d.h>
#include <yaam_cpred_d.h>
#include <yaam_cut_d.h>
#include <yaam_failure_d.h>
#include <yaam_get_d.h>
#include <indexing_ext_d.h>
#include <indexing_std_d.h>
#include <yaam_misc_d.h>
#include <yaam_pop_d.h>
#include <yaam_put_d.h>
#include <yaam_unify_d.h>
#include <yaam_write_d.h>

extern void print_instruction(yamop*, enumPlace);
extern void print_block(YAP_BBs, enumPlace);
extern void print_main_when_head(yamop*, enumPlace);
