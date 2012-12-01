/**
   @file geo.h
   
   Simple geometry structures and routines.
*/
#ifndef GEO_H
#define GEO_H

#include <stdint.h>

/**
   Models a rectangle.
*/
typedef struct rect {
  /** Starting x for the rectangle. */
  int x;
  /** Starting y for the rectangle. */
  int y;

  int width;
  int height;
} rect_t;

/**
   2 dimensional point.
*/
typedef struct point {
  int x;
  int y;
} point_t;

/**
   Simple setter for a rectangle.
*/
void 
rect_set(rect_t* rect, int x, int y, int width, int height);

#endif
