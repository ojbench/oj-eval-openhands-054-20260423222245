all: code

code: solution.cpp
	g++ -O2 -std=c++17 -o code solution.cpp

clean:
	rm -f code
