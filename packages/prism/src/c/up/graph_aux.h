#ifndef GRAPH_AUX_H
#define GRAPH_AUX_H

/*
 * mode for print_egraph
 * (positive for EM learning; negative for other inferences)
 */
#define PRINT_NEUTRAL  0
#define PRINT_EM       1
#define PRINT_VBEM     2
#define PRINT_VITERBI -1

void print_egraph(int, int);

#endif /* GRAPH_AUX_H */
