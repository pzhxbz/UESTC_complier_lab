

all:
	g++ LexicalAnalysis.cpp -o LexicalAnalysis -std=c++11 -g 
	g++ SyntacticAnalysis.cpp -o SyntacticAnalysis -std=c++11 -g 
w:
	g++ LexicalAnalysis.cpp -o LexicalAnalysis -std=c++11 -g 
s:
	g++ SyntacticAnalysis.cpp -o SyntacticAnalysis -std=c++11 -g 
clean:
	rm LexicalAnalysis SyntacticAnalysis