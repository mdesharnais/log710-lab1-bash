all:
	clang++ -std=c++11 -Wall -Wextra -Werror main.cpp
	./a.out ls -la /usr/src
