cl-gd-glue.so:
	gcc -I/usr/local/include -fPIC -c cl-gd-glue.c
	ld -shared -lgd -lz -lpng -ljpeg -lfreetype -liconv -lm -lc cl-gd-glue.o -o cl-gd-glue.so -L/usr/local/lib
	rm cl-gd-glue.o
