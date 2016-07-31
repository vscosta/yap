:- module( unix, [pipe/2] ).

pipe(Inp, Out) :- open_pipe_stream(Inp, Out).
