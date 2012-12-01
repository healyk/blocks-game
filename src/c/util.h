/**
   @file util.h
   
   Various utility functions.
*/
#ifndef UTIL_H
#define UTIL_H

#include <stdlib.h>

/**
   Initializes the game log.
*/
void log_init(const char* filename);

/**
   Logs a message to the message log.
*/
void logmsg(char* fmt, ...);

/**
   Cleans up a pointer to the passed in memory.
   
   @param mem
     Pointer to clean up.
*/
void _delete(void* mem);

/**
   Allocates a new chunk of memory.
   
   @param element_size
     Size of the element to allocate
   @param element_count
     Number of elements to allocate.
   @param typename
     Name of the type being allocated.

   @return
     A new pointer or NULL if an error occured.
*/
void* _new(size_t element_size,
           size_t element_count, 
           const char* typename);

/**
   Allocates memory on the heap for type.
*/
#define new(type)                               \
  ((type*)_new(sizeof(type), 1, #type))

#define new_array(type, size)                   \
  ((type*)_new(sizeof(type), size, #type))

/**
   Frees memory on the heap for the variable.
*/
#define delete(var) \
  (_delete(var))

/**
   Generates a random integer in the range [min, max).
*/
int rand_int(int min, int max);

/**
   Generates a random double between [0, 1.0].
*/
double rand_double(void);

/*
  Guard against including these functions in scheme compiled files.  Gambit
  already defines a min and max.  If these are included we'll get collisions
  (don't really know why) and errors will pop up when compiling.
*/
#ifndef ___LIBRARY

/**
   Returns the minimum integer between two numbers.
*/
int min(int a, int b);

/**
   Returns the maximum integer between two numbers.
*/
int max(int a, int b);

#endif // ___LIBRARY

#endif
