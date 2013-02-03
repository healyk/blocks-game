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
#define BUFFER_SIZE   32768
#define CHANNEL_COUNT 4

//==============================================================================
// Globals
//==============================================================================
/**
   Sound channels model a currently playing audio stream.
*/
typedef struct sound_channel {
  /** OpenAL source for the channel when playing. */
  ALuint source;

  /** Sound currently occuping the channel. */
  sound_t* sound;
} sound_channel_t;

/**
   Used to keep track of the OpenAL global state.
*/
struct {
  //! Has this been initialized?
  bool        inited;

  ALCcontext* context;
  ALCdevice*  device;

  sound_channel_t channels[CHANNEL_COUNT];
} g_sound_context;

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

  // Create the channels and sources
  for(int i = 0; i < CHANNEL_COUNT; i++) {
    sound_channel_t* channel = &g_sound_context.channels[i];

    channel->sound = NULL;
    alGenSources(1, &channel->source);
  }

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
                   (ALvoid*)((char*)sound->data + (i * BUFFER_SIZE)), 
                   buffer_size,
                   sound->frequency);
      
      result = openal_check_for_error("filling buffers");
    }
  }

  return result;
}

sound_t*
sound_new(void) {
  sound_t* sound = NULL;

  sound = new(sound_t);

  sound->data = NULL;
  sound->buffers = NULL;
  
  return sound;
}

static void
log_wav_info(char* filename, wav_t* wav) {
  logmsg("Loaded [%s]:  %dHZ, %d channels, %d bits, %d bytes in size", 
         filename, wav->fmt.sample_rate, wav->fmt.num_channels, 
         wav->fmt.bits_per_sample, wav->data.subchunk2_size);
}

sound_t*
sound_load(char* filename) {
  sound_t* sound = NULL;

  sound = sound_new();

  {
    wav_t* wav = wav_load(filename);
    log_wav_info(filename, wav);

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
  logmsg("Deleting sound with %d buffers", sound->buffer_count);

  if(sound->buffers != NULL) {
    alDeleteBuffers(sound->buffer_count, sound->buffers);
    delete(sound->buffers);
  }

  if(sound->data != NULL) {
    delete(sound->data);
  }

  delete(sound);
}

/**
   Determines if a channel is currently playing.

   @param channel
     Channel to check
   @return
     True if the channel is still playing, false if not.
*/
static bool
is_channel_playing(sound_channel_t* channel) {
  ALenum state;
  alGetSourcei(channel->source, AL_SOURCE_STATE, &state);

  return state == AL_PLAYING;
}

/**
   Stops a channel from playing any more sound.  It will remove the current
   sound.
*/
static void
sound_stop_channel(sound_channel_t* channel) {
  if(channel->sound != NULL) {
    if(is_channel_playing(channel)) {
      alSourceStop(channel->source);
      openal_check_for_error("Stopping channel");
    }

    alSourcei(channel->source, AL_BUFFER, AL_NONE);
    openal_check_for_error("Unbinding source");
    channel->sound = NULL;
  }
}

void 
sound_shutdown(void) {
  if(g_sound_context.inited) {
    logmsg("Shutting down sound.");

    alcMakeContextCurrent(NULL);
    alcDestroyContext(g_sound_context.context);
    alcCloseDevice(g_sound_context.device);

    // clean up channels
    for(int i = 0; i < CHANNEL_COUNT; i++) {
      sound_stop_channel(&g_sound_context.channels[i]);
      alDeleteSources(1, &g_sound_context.channels[i].source);
    }

    g_sound_context.inited = false;
  }
}

void 
sound_play(sound_t* sound) {
  sound_channel_t* open_channel = NULL;

  // Find an open channel
  for(int i = 0; i < CHANNEL_COUNT && open_channel == NULL; i++) {
    if(g_sound_context.channels[i].sound == NULL) {
      open_channel = &g_sound_context.channels[i];
    }
  }

  // Play the sound
  if(open_channel != NULL) {
    open_channel->sound = sound;

    alSourceQueueBuffers(open_channel->source, 
                         sound->buffer_count, sound->buffers);
    
    alSourcePlay(open_channel->source);
  } else {
    logmsg("Warning - no open sound channels.");
  }
  
  openal_check_for_error("playing sound");
}

void
sound_update(long delta) {
  for(int i = 0; i < CHANNEL_COUNT; i++) {
    sound_channel_t* channel = &g_sound_context.channels[i];

    if(channel->sound != NULL) {
      if(!is_channel_playing(channel)) {
        sound_stop_channel(channel);
      }
    }
  }
}
