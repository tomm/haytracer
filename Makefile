default:
	-rm *.o
	ghc -Wall -O Main.hs -o haytracer

caytracer: Main.cpp
	g++ -g -Wall -O3 -fsanitize=address -fsanitize=leak -std=c++14 `sdl-config --cflags --libs` -pthread Main.cpp -o caytracer 

caytracer-prod: Main.cpp
	g++ -g -Wall -O3 -std=c++14 `sdl-config --cflags --libs` -pthread Main.cpp -o caytracer 
