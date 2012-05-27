/* Copyright (c) 2003-2009, Dr. Edmund Weitz.  All rights reserved. 

   Redistribution and use in source and binary forms, with or without 
   modification, are permitted provided that the following conditions 
   are met: 

     * Redistributions of source code must retain the above copyright 
       notice, this list of conditions and the following disclaimer. 

     * Redistributions in binary form must reproduce the above 
       copyright notice, this list of conditions and the following 
       disclaimer in the documentation and/or other materials 
       provided with the distribution. 

   THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED 
   OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
   WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
   ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY 
   DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
   DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE 
   GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
   INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
   WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING 
   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS 
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */

#include <errno.h>
#include <stdio.h>
#include "gd.h"

gdImagePtr gdImageCreateFromJpegFile (char *filename, int *err) {
  FILE *in;
  gdImagePtr im;
  
  if (in = fopen(filename, "rb")) {
    im = gdImageCreateFromJpeg(in);
    if (im == NULL) {
      *err = 0;
      return NULL;
    }
    fclose(in);
    return im;
  }
  *err = errno;
  return NULL;
}

#ifndef GD_DONT_USE_GIF
gdImagePtr gdImageCreateFromGifFile (char *filename, int *err) {
  FILE *in;
  gdImagePtr im;
  
  if (in = fopen(filename, "rb")) {
    im = gdImageCreateFromGif(in);
    if (im == NULL) {
      *err = 0;
      return NULL;
    }
    fclose(in);
    return im;
  }
  *err = errno;
  return NULL;
}
#endif

gdImagePtr gdImageCreateFromPngFile (char *filename, int *err) {
  FILE *in;
  gdImagePtr im;
  
  if (in = fopen(filename, "rb")) {
    im = gdImageCreateFromPng(in);
    if (im == NULL) {
      *err = 0;
      return NULL;
    }
    fclose(in);
    return im;
  }
  *err = errno;
  return NULL;
}

gdImagePtr gdImageCreateFromGdFile (char *filename, int *err) {
  FILE *in;
  gdImagePtr im;
  
  if (in = fopen(filename, "rb")) {
    im = gdImageCreateFromGd(in);
    if (im == NULL) {
      *err = 0;
      return NULL;
    }
    fclose(in);
    return im;
  }
  *err = errno;
  return NULL;
}

gdImagePtr gdImageCreateFromGd2File (char *filename, int *err) {
  FILE *in;
  gdImagePtr im;
  
  if (in = fopen(filename, "rb")) {
    im = gdImageCreateFromGd2(in);
    if (im == NULL) {
      *err = 0;
      return NULL;
    }
    fclose(in);
    return im;
  }
  *err = errno;
  return NULL;
}

gdImagePtr gdImageCreateFromGd2PartFile (char *filename, int *err, int srcX, int srcY, int w, int h) {
  FILE *in;
  gdImagePtr im;
  
  if (in = fopen(filename, "rb")) {
    im = gdImageCreateFromGd2Part(in, srcX, srcY, w, h);
    if (im == NULL) {
      *err = 0;
      return NULL;
    }
    fclose(in);
    return im;
  }
  *err = errno;
  return NULL;
}

gdImagePtr gdImageCreateFromXbmFile (char *filename, int *err) {
  FILE *in;
  gdImagePtr im;
  
  if (in = fopen(filename, "rb")) {
    im = gdImageCreateFromXbm(in);
    if (im == NULL) {
      *err = 0;
      return NULL;
    }
    fclose(in);
    return im;
  }
  *err = errno;
  return NULL;
}

int gdImageGetAlpha (gdImagePtr im, int color) {
  return gdImageAlpha(im, color);
}

int gdImageGetRed (gdImagePtr im, int color) {
  return gdImageRed(im, color);
}

int gdImageGetGreen (gdImagePtr im, int color) {
  return gdImageGreen(im, color);
}

int gdImageGetBlue (gdImagePtr im, int color) {
  return gdImageBlue(im, color);
}

int gdImageGetSX (gdImagePtr im) {
  return gdImageSX(im);
}

int gdImageGetSY (gdImagePtr im) {
  return gdImageSY(im);
}

int gdImageGetColorsTotal (gdImagePtr im) {
  return gdImageColorsTotal(im);
}

/* dumb names, I know... */
int gdImageGetGetInterlaced (gdImagePtr im) {
  return gdImageGetInterlaced(im);
}

int gdImageGetGetTransparent (gdImagePtr im) {
  return gdImageGetTransparent(im);
}

/* GIF animation support */

void*
gdImageGifAnimBeginWrap(gdImagePtr im,
                        char* filename,
                        int globalCM,
                        int loops)
{
  FILE* out = fopen(filename, "w");

  if (out) {
    gdImageGifAnimBegin(im, out, globalCM, loops);
  }

  return out;
}

void
gdImageGifAnimEndWrap(void* out)
{
  gdImageGifAnimEnd(out);
  fclose(out);
}
