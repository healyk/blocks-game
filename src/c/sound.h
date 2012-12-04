/**
   @file sound.h

   Used to abstract the sound layer away and provide a common sound api.
*/
#ifndef SOUND_H
#define SOUND_H

#include <stdbool.h>
#include <AL/al.h>

/**
   Detailed information about a sound wave in memory.  This contains data
   on the actual sound and OpenAL data.
*/
typedef struct sound {
  ALenum    format;
  /** This is the size of the data field in bytes. */
  ALsizei   size;
  ALsizei   frequency;
  /** Contains the raw sample data. */
  ALvoid*   data;

  /** Number of buffers used. */
  size_t    buffer_count;
  /** Buffers used to hold the sound in OpenAL memory. */
  ALuint*   buffers;

  /** Used to keep track of any source this sound is curerntly queued in. */
  ALuint    source;
} sound_t;

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

/**
   Loads a sound file into an audio chunk.
*/
sound_t* sound_new(char* filename);

/** 
    Deletes an existing sound.
*/
void sound_delete(sound_t* sound);

/**
   Plays a chunk.
*/
void sound_play(sound_t* sound);

#endif 
