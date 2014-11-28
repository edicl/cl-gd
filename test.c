#include <gd.h>
#include <assert.h>

int
main(int argc, char* argv[])
{
  assert(argc == 2);
  FILE* f = fopen(argv[1], "r");
  assert(f);
  gdImagePtr img = gdImageCreateFromJpeg(f);
  assert(img);
  printf("%x %x %d\n", (int) img->alpha, (int) img->tpixels, sizeof(int));
}
