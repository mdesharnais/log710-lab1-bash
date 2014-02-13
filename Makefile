all:
	clang++ -o run -std=c++11 -Wall -Wextra -Werror main.cpp
	./run
