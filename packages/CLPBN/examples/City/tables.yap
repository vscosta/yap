
cons_table(amsterdam,[0.2,
	      0.8]) :- !.
cons_table(_, [0.8,
	      0.2]).

color_table(_,
	   /* tm tf fm ff */
	   [ 0.05,  0.1, 0.3, 0.5 ,
             0.95,  0.9, 0.7, 0.5 ]).
    
ccolor_table(_,
	   /* t f */
	   [ 0.9,  0.2 ,
             0.1,  0.8 ]).
	
height_table(_,
	   /* m f */
	   [ 0.6,  0.4 ,
             0.4,  0.6 ]).
	
shoe_size_table(_,
	   /* t f */
	   [ 0.9,  0.1 ,
             0.1,  0.9 ]).
	
  A: professor's ability;
  B: student's grade (for course registration).
*/
descn_table(_,
       /* color, hair, height, guilt */
       /* ttttt tttf ttft ttff tfttt tftf tfft tfff ttttt fttf ftft ftff ffttt fftf ffft ffff  */
/*t*/     [0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99, 0.99,
/*f*/	   0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01 ]).

