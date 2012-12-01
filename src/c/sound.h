/**
   @file sound.h

   Used to abstract the sound layer away and provide a common sound api.
*/
#ifndef SOUND_H
#define SOUND_H

#include <stdbool.h>

/**
   Initializes the sound system.
   @return
     True on success, false on failure.
*/
bool sound_init(void);

/**
   Performs sound system cleanup.
*/
void sound_shutdown(void);

#endif 
