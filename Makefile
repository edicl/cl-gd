
cl-gd-glue.so: cl-gd-glue-gif.c
	gcc -I/usr/local/include -fPIC -c cl-gd-glue-gif.c
	ld -L/usr/local/lib -lgd -lz -lpng -ljpeg -lfreetype -lm -liconv -shared cl-gd-glue-gif.o -o cl-gd-glue.so
	
