

%:- initialization(main).

test_mq(N) :-
    test_mq(0,N),
    thread_send_message(0,msg(-1)),
    N1 is N-1,
    thread_receive_message(0,msg(N1)).
    

test_mq(N,N) :-
    !.
test_mq(I,N) :-
    % round-robin
    I1 is I+1,
    thread_create(pass(I,N)),
    test_mq(I1,N).

pass(I,N) :-
    thread_receive_message(I,msg(_)),
    I1 is (I+1)mod N,
    thread_send_message(I1,msg(I)).

main :-
    test_mq(2).
