#include <errno.h>
#include <string.h>
#include <stdarg.h>
#include <stdio.h>

#include "util.h"

static FILE* log_handle = NULL;

void
log_shutdown(void) {
  if(log_handle != NULL) {
    // One extra flush before we close the log.
    fflush(log_handle);
    fclose(log_handle);
  }
}

void 
log_init(const char* filename) {
  log_handle = fopen(filename, "wt");

  logmsg("Initializing log file.");  
}

void
logmsg(char* fmt, ...) {
  FILE* stream = stderr;
  va_list args;

  if(NULL != log_handle) {
    stream = log_handle;
  }

  va_start(args, fmt);
  vfprintf(stream, fmt, args);
  fprintf(stream, "\n");
  va_end(args);

  fflush(stream);
}

void 
_delete(void* mem) {
  if(mem != NULL) {
    free(mem);
  }
}

void* 
_new(size_t element_size, size_t element_count, const char* typename) {
  void* pointer = NULL;

  pointer = calloc(element_count, element_size);
  if(NULL == pointer) {
    fprintf(stderr, "Unable to allocate %zu bytes, %zu elements for variable of"
            " type %s.\n", element_size, element_count, typename);
    fprintf(stderr, "errno: %d, %s\n", errno, strerror(errno));
    fflush(stderr);

    exit(1);
  }

  return pointer;
}

int rand_int(int min, int max) {
  int val = (rand() % (max - min)) + min;
  return val;
}

double rand_double(void) {
  static double div = (RAND_MAX * 1.0);
  return rand() / div;
}

int min(int a, int b) {
  return a < b ? a : b; 
}

int max(int a, int b) {
  return a > b ? a : b; 
}
