/*
import nltk
sentence = """At eight o'clock on Thursday morning
... Arthur didn't feel very good."""
tokens = nltk.word_tokenize(sentence)
tagged = nltk.pos_tag(tokens)
tagged[0:6]
entities = nltk.chunk.ne_chunk(tagged)
entities
*/

:- use_module(library(python)).
:- use_module(library(maplist)).

main :-
	main(Sentence, Tokens, Tagged),
	writeln(Sentence),
	writeln(tokens=Tokens),
	writeln(tagged=Tagged),
	fail.
main :-
	Sentence = 'Debutta a New York il nuovo sistema operativo (cronaca in diretta). E c\'è il tablet Surface. Svolta radicale che strizza l\'occhio al mondo touch per l\'azienda che controlla il 92% dei pc dal nostro inviato M. Serafini',
    % c = nltk.stem.snowball.ItalianStemmer("italian')
        $c := nltk:stem:snowball:'ItalianStemmer'(italian),
	Tokens := nltk:word_tokenize(Sentence),
	writeln(tokens=Tokens),
    % o = c.stem('voglio')
	maplist(process, Tokens, Stems),
	writeln(stems=Stems).

process(In, Out) :-
	Out := $c:stem(In),
	writeln(In:=Out).
process(In, In).
    
main(Sentence, Tokens, Tagged) :- 
	Sentence = '\"At eight o\'clock on Thursday morning\
... Arthur didn\'t feel very good.\"',
	Tokens := nltk:word_tokenize(Sentence),
%['At', 'eight', "o'clock", 'on', 'Thursday', 'morning',
% 'Arthur', 'did', "n't", 'feel', 'very', 'good', '.']
	Tagged := nltk:pos_tag(Tokens).
%>>> tagged[0:6]
%[('At', 'IN'), ('eight', 'CD'), ("o'clock", 'JJ'), ('on', 'IN'),
%('Thursday', 'NNP'), ('morning', 'NN')]

