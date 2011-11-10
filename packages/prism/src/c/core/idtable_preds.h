#ifndef IDTABLE_AUX_H
#define IDTABLE_AUX_H

/*--------------------------------------------------------------------*/

int    prism_goal_id_register(TERM);
int    prism_sw_id_register(TERM);
int    prism_sw_ins_id_register(TERM);
int    prism_goal_id_get(TERM);
int    prism_sw_id_get(TERM);
int    prism_sw_ins_id_get(TERM);
int    prism_goal_count(void);
int    prism_sw_id_count(void);
int    prism_sw_ins_id_count(void);
TERM   prism_goal_term(IDNUM);
TERM   prism_sw_term(IDNUM);
TERM   prism_sw_ins_term(IDNUM);
char * prism_goal_string(IDNUM);
char * prism_sw_string(IDNUM);
char * prism_sw_ins_string(IDNUM);
char * copy_prism_goal_string(IDNUM);
char * copy_prism_sw_string(IDNUM);
char * copy_prism_sw_ins_string(IDNUM);

int pc_prism_id_table_init(void);
int pc_prism_goal_id_register(void);
int pc_prism_sw_id_register(void);
int pc_prism_sw_ins_id_register(void);
int pc_prism_goal_id_get(void);
int pc_prism_sw_id_get(void);
int pc_prism_sw_ins_id_get(void);
int pc_prism_goal_count(void);
int pc_prism_sw_count(void);
int pc_prism_sw_ins_count(void);
int pc_prism_goal_term(void);
int pc_prism_sw_term(void);
int pc_prism_sw_ins_term(void);

/*--------------------------------------------------------------------*/

#endif /* IDTABLE_AUX_H */
