            :- python_import( seaborn as sns ).

        :- python_import( pandas as pd ).

        main :-
            sns.set_theme(style= `white` ),

        % Load the example mpg dataset
        %mpg = pd.load_dataset("mpg")
          mpg := pd.read_csv(`mpg.csv`),

        % Plot miles per gallon against horsepower with other semantics
		    sns.relplot(x= `horsepower`,
				y= `mpg`,
				hue= `origin`,
				size= `weight`,
				sizes= [40, 400],
				alpha= 0.5,
					palette= `muted`,
					      height=6, data= mpg).

