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

/**
   Queries OpenAL for an error.  If an error occurs it prints it to the logs
   and returns false.

   @param operation
     Operation being executed.   Will be included in the log message.

   @return
     True if no error occured, false otherwise.
*/
static bool
openal_check_for_error(const char* operation) {
  int error = alGetError();
  bool result = true;

  if(error != AL_NO_ERROR) {
    logmsg("Received OpenAL error %x for operation %s", error, operation);
    result = false;
  }

  return result;
}

bool
sound_init(void) {
  bool result = true;

  // TODO: open a specific device?
  g_sound_context.device = alcOpenDevice(NULL);
  if(NULL == g_sound_context.device) {
    logmsg("Unable to open OpenAL device: %x", alGetError());
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
  bool result = true;

  sound->buffer_count = (size_t)ceil(sound->size / (BUFFER_SIZE * 1.0));
  sound->buffers = new_array(ALuint, sound->buffer_count);
  alGenBuffers(sound->buffer_count, sound->buffers);

  result = openal_check_for_error("generating buffers");
  if(result) {
    for(int i = 0; i < sound->buffer_count && result; i++) {
      size_t buffer_size = min(BUFFER_SIZE, sound->size - (i * BUFFER_SIZE));
      
      alBufferData(sound->buffers[i],
                   sound->format, 
                   (void*)(sound->data + (i * BUFFER_SIZE)), 
                   buffer_size,
                   sound->frequency);
      
      result = openal_check_for_error("filling buffers");
    }
  }

  return result;
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
  ALint current_buffer;
  alGetSourceiv(g_sound_context.source, AL_BUFFER, &current_buffer);

  // unbind any previous sources
  if(current_buffer != AL_NONE) {
    alSourceStop(g_sound_context.source);
    alSourcei(g_sound_context.source,
              AL_BUFFER,
              AL_NONE);
  }

  if(openal_check_for_error("unqueueing buffers")) {
    alSourceQueueBuffers(g_sound_context.source, 
                         sound->buffer_count, 
                         sound->buffers);
    
    alSourcePlay(g_sound_context.source);
  }
  
  openal_check_for_error("playing sound");
}
