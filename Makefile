cl-gd-glue.so:
	gcc -I/usr/local/include -fPIC -c cl-gd-glue.c
	ld -lgd -lz -lpng -ljpeg -lfreetype -lm -liconv -shared cl-gd-glue.o -o cl-gd-glue.so -L/usr/local/lib
	rm cl-gd-glue.o
