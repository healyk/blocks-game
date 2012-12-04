#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <AL/alc.h>

#include "sound.h"
#include "util.h"
#include "wav.h"

//==============================================================================
// Constants
//==============================================================================
#define BUFFER_SIZE 32768

typedef struct source {
  ALuint source;
  sound_t* buffered_sound;
} source_t;

//==============================================================================
// Globals
//==============================================================================
/**
   Used to keep track of the OpenAL global state.
*/
struct {
  //! Has this been initialized?
  bool        inited;

  ALCcontext* context;
  ALCdevice*  device;

  ALuint      source;
} g_sound_context = {
  false, NULL, NULL, 0
};

bool
sound_init(void) {
  bool result = true;

  // TODO: open a specific device?
  g_sound_context.device = alcOpenDevice(NULL);
  if(NULL == g_sound_context.device) {
    logmsg("Unable to open OpenAL device: %d", alGetError());
    result = false;
  } else {
    logmsg("Initialized OpenAL device: %s", 
           alcGetString(NULL, ALC_DEFAULT_DEVICE_SPECIFIER));
  }                                                     
  
  if(result) {
    g_sound_context.context = alcCreateContext(g_sound_context.device, NULL);
    alcMakeContextCurrent(g_sound_context.context);
    alcProcessContext(g_sound_context.context);
  }

  // Create a global source
  alGenSources(1, &g_sound_context.source);

  alDistanceModel(AL_NONE);
  
  g_sound_context.inited = result;
  alGetError();
  
  return result;
}

/**
   Fills in some of the sound data from a wav structure.
*/
static void
wav_to_sound(wav_t* wav, sound_t* sound) {
  if(wav->fmt.bits_per_sample == 8) {
    if(wav->fmt.num_channels == 1) {
      sound->format = AL_FORMAT_MONO8;
    } else if(wav->fmt.num_channels == 2) {
      sound->format = AL_FORMAT_STEREO8;
    }
  } else {
    if(wav->fmt.num_channels == 1) {
      sound->format = AL_FORMAT_MONO16;
    } else {
      sound->format = AL_FORMAT_STEREO16;
    }
  }

  sound->size = wav->data.subchunk2_size;
  sound->frequency = wav->fmt.sample_rate;
  
  sound->data = new_array(unsigned char, sound->size);
  memcpy(sound->data, wav->data.data, sound->size);
}

static bool
generate_buffers(sound_t* sound) {
  int error = 0;

  sound->buffer_count = (size_t)ceil(sound->size / (BUFFER_SIZE * 1.0));
  sound->buffers = new_array(ALuint, sound->buffer_count);
  alGenBuffers(sound->buffer_count, sound->buffers);

  error = alGetError();
  if(error != AL_NO_ERROR) {
    logmsg("OpenAL error when generating buffers: %d", error);
  } else {
    for(int i = 0; i < sound->buffer_count; i++) {
      size_t buffer_size = min(BUFFER_SIZE, sound->size - (i * BUFFER_SIZE));
      
      alBufferData(sound->buffers[i],
                   sound->format, 
                   (void*)(sound->data + (i * BUFFER_SIZE)), 
                   buffer_size,
                   sound->frequency);
      
      error = alGetError();
      if(error != AL_NO_ERROR) {
        logmsg("OpenAL error when buffering data: %x", error);
      }
    }
  }

  return error == AL_NO_ERROR;
}

sound_t*
sound_new(char* filename) {
  sound_t* sound = NULL;

  logmsg("Loading sound %s into memory.", filename);

  // Allocate memory and pre-initialize some values
  sound = new(sound_t);

  sound->data = NULL;
  sound->buffers = NULL;

  {
    wav_t* wav = wav_load(filename);
    wav_to_sound(wav, sound);
    wav_delete(wav);
  }

  if(!generate_buffers(sound)) {
    sound_delete(sound);
    
    sound = NULL;
    logmsg("Error creating sound!  NULL returned.");
  }

  return sound;
}

void
sound_delete(sound_t* sound) {
  logmsg("Deleting sound...");

  if(sound->buffers != NULL) {
    alDeleteBuffers(sound->buffer_count, sound->buffers);
    delete(sound->buffers);
  }

  if(sound->data != NULL) {
    delete(sound->data);
  }

  delete(sound);
}

void 
sound_shutdown(void) {
  if(g_sound_context.inited) {
    logmsg("Shutting down sound.");

    alcMakeContextCurrent(NULL);
    alcDestroyContext(g_sound_context.context);
    alcCloseDevice(g_sound_context.device);

    alDeleteSources(1, &g_sound_context.source);

    g_sound_context.inited = false;
  }
}

void 
sound_play(sound_t* sound) {
  int error = 0;

  ALint current_buffer;
  alGetSourceiv(g_sound_context.source, AL_BUFFER, &current_buffer);

  if(current_buffer != AL_NONE) {
    alSourceStop(g_sound_context.source);
    alSourcei(g_sound_context.source,
              AL_BUFFER,
              AL_NONE);
  }

  alSourceQueueBuffers(g_sound_context.source, 
                       sound->buffer_count, 
                       sound->buffers);

  alSourcePlay(g_sound_context.source);
  
  error = alGetError();
  if(error != AL_NO_ERROR) {
    logmsg("OpenAL error when playing source: %x", error);
  }
}
