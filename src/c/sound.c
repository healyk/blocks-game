#include <SDL.h>
#include "SDL_mixer.h"

#include "sound.h"
#include "util.h"

//==============================================================================
// Globals
//==============================================================================
static bool g_sound_inited = false;

//==============================================================================

static void
log_sdl_version(void) {
  const SDL_version* ver = SDL_Linked_Version();
  SDL_version com_ver;
  SDL_VERSION(&com_ver);

  logmsg("Staring SDL ver %u.%u.%u, compiled with ver %u.%u.%u", 
         ver->major, ver->minor, ver->patch,
         com_ver.major, com_ver.minor, com_ver.patch);
}

static void
log_mix_version(void) {
  const SDL_version* ver = Mix_Linked_Version();
  SDL_version com_ver;
  MIX_VERSION(&com_ver);

  logmsg("Staring SDL_mixer ver %u.%u.%u, compiled with ver %u.%u.%u", 
         ver->major, ver->minor, ver->patch,
         com_ver.major, com_ver.minor, com_ver.patch);
}

bool 
sound_init(void) {
  bool result = true;

  int audio_rate = 22050;
  Uint16 audio_format = AUDIO_S8;
  int audio_channels = 2;
  int audio_buffers = 1024;

  if(SDL_Init(SDL_INIT_AUDIO) < 0) {
    logmsg("Unable to initialize sound system: %s", SDL_GetError());
    result = false;
  } else {
    log_sdl_version();
  }

  if(result && Mix_OpenAudio(audio_rate, 
                             audio_format, 
                             audio_channels, 
                             audio_buffers) != 0) 
  {
    logmsg("Unable to configure OpenAudio: %s", Mix_GetError());
    SDL_Quit();
    result = false;
  } else {
    log_mix_version();
  }

  g_sound_inited = result;
  return result;
}

void 
sound_shutdown(void) {
  if(g_sound_inited) {
    Mix_HaltChannel(-1);
    
    Mix_CloseAudio();
    SDL_Quit();

    g_sound_inited = false;
  }
}
