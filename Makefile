# this should work for FreeBSD and most Linux distros

cl-gd-glue.so:
	gcc -I/usr/local/include -fPIC -c cl-gd-glue.c
	ld -shared -lgd -lz -lpng -ljpeg -lfreetype -lm -lc cl-gd-glue.o -o cl-gd-glue.so -L/usr/local/lib
	rm cl-gd-glue.o

# this should work for Mac OS X

cl-gd-glue.dylib:
	gcc -arch i386 -arch x86_64 -lgif -lgd -lpng -lz -lfreetype -ljpeg -dynamiclib cl-gd-glue.c -o cl-gd-glue.dylib -I/opt/local/include -L/opt/local/lib
