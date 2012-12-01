/**
   @file mylib.h

   Prototypes for functions in Scheme called from C.
*/

#ifndef MYLIB_H
#define MYLIB_H

#include <stdbool.h>

extern void scheme_init_game();
extern bool scheme_update_game(long delta);
extern bool scheme_render_game(long delta);

extern void scheme_keyboard_callback(int key, int pressed);

#endif
