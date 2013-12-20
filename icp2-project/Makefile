all:	
	$(MAKE) -C ./Pico
	$(MAKE) tpico
	
tpico: configure.libs tpico.o ./Pico/*.o
	gcc `cat configure.opts` -o tpico tpico.o ./Pico/*.o -lm `cat configure.libs` 

configure.libs:
	./configure

clean:
	rm -f tpico *.o core tags configure.libs configure.opts

tpico.o: tpico.c ./Pico/Pico.h ./Pico/PicoMai.h
	gcc `cat configure.opts` -c tpico.c
