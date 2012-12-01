#include "geo.h"
#include "util.h"

void 
rect_set(rect_t* rect, int x, int y, int width, int height) {
  rect->x = x;
  rect->y = y;
  rect->width = width;
  rect->height = height;
}

