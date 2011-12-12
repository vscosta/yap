
cons_table(amsterdam, [0.2, 0.8]) :- !.
cons_table(_, [0.8, 0.2]).


gender_table(_, [0.55, 0.44]).


hair_color_table(_,
     /* conservative_city */
     /* y     n   */
      [ 0.05, 0.1,
        0.95, 0.9 ]).
    

car_color_table(_,
     /* t    f */
      [ 0.9, 0.2,
        0.1, 0.8 ]).
	

height_table(_,
     /* m    f */
      [ 0.6, 0.4,
        0.4, 0.6 ]).
	

shoe_size_table(_,
     /* t    f */
      [ 0.9, 0.1,
        0.1, 0.9 ]).


guilty_table(_, [0.23, 0.77]).


descn_table(_,
     /* color, hair, height, guilt */
     /* ttttt  tttf  ttft  ttff  tfttt tftf  tfft  tfff  ttttt fttf  ftft  ftff  ffttt fftf  ffft  ffff */
      [ 0.99,  0.5,  0.23, 0.88, 0.41, 0.3, 0.76, 0.87, 0.44, 0.43, 0.29, 0.72, 0.33, 0.91, 0.95, 0.92,
        0.01,  0.5,  0.77, 0.12, 0.59, 0.7, 0.24, 0.13, 0.56, 0.57, 0.61, 0.28, 0.77, 0.09, 0.05, 0.08]).


wit_table([0.2, 0.45, 0.24, 0.34,
           0.8, 0.55, 0.76, 0.66]).

