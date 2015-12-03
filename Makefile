default:
	-rm *.o
	ghc -Wall -O Main.hs -o haytracer
