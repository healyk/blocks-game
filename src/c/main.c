#include <stdbool.h>
#include <time.h>

#include "gfx.h"
#include "util.h"
#include "sound.h"

//==============================================================================
// Gambit scheme intergration
//==============================================================================
#define ___VERSION 406006
#include "gambit.h"

#include "mylib.h"

___BEGIN_C_LINKAGE
extern ___mod_or_lnk SCHEME_LIBRARY_LINKER(___global_state_struct*);
___END_C_LINKAGE
//==============================================================================
// End integration
//==============================================================================

//==============================================================================
// Constants
//==============================================================================
#define VERSION "1.0"

const int SCREEN_WIDTH = 800;
const int SCREEN_HEIGHT = 600;

//==============================================================================
// Globals
//==============================================================================
bool g_is_scheme_init = false;
bool g_is_gfx_init = false;

/**
   Wraps the scheme call for keyboard input.  This is done to catch any
   exceptions that leak up.
*/
void GLFWCALL 
keyboard_callback_wrapper(int key, int pressed) {
  ___ON_THROW(scheme_keyboard_callback(key, pressed), 
  {
    logmsg("Recevied throw when calling keyboard input callback.");
    exit(1);
  });
}

void
shutdown_game(void) {
  // Shutdown scheme - this must be done first since some scheme variables
  // will be set to various resources.  Scheme will clean these up for us.
  if(g_is_scheme_init) {
    ___cleanup();
  }

  sound_shutdown();

  if(g_is_gfx_init) {
    gfx_end_2d();
    gfx_shutdown();
  }

  log_shutdown();
}

/**
   Initializes the scheme system/bindings.
   @return
     True on success, false on failure.
*/
bool
init_scheme(void) {
  bool result = true;

  logmsg("Starting scheme init");

  ___setup_params_struct setup_params;
  ___setup_params_reset(&setup_params);
  
  setup_params.version = ___VERSION;
  setup_params.linker = SCHEME_LIBRARY_LINKER;
  
  ___setup(&setup_params);
  
  // Allow scheme to initialize
  ___ON_THROW(scheme_init_game(), {
    logmsg("Received throw when calling init, exiting.");
    result = false;
  });
  
  g_is_scheme_init = true;
  glfwSetKeyCallback(&keyboard_callback_wrapper);

  logmsg("Finished scheme init.");
  
  return result;
}

/**
   Initializes the game.  This will set up the graphics and initialize
   scheme.
*/
bool
init_game(void) {
  bool result;

  atexit(shutdown_game);

  log_init("blocks.log");
  logmsg("Starting Blocks ver. " VERSION);

  result = gfx_init("Blocks ver. " VERSION, SCREEN_WIDTH, SCREEN_HEIGHT);
  if(result) {
    glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
    gfx_begin_2d();

    g_is_gfx_init = true;
  }

  // Intialize sound
  if(result) {
    result = sound_init();
  }

  // Init scheme
  if(result) {
    result = init_scheme();
  }

  // Seed the number generator
  srand(time(NULL));
  for(int i = 0; i < 5; i++) { rand(); }

  return result;
}

bool 
update_game(long delta) {
  bool result = true;
  
  ___ON_THROW(result = scheme_update_game(delta), {
    logmsg("Caught scheme exception in update");
    result = false;
  });

  return result;
}

bool 
render_game(long delta) {
  bool result = true;

  ___ON_THROW(scheme_render_game(delta), {
    logmsg("Caught scheme exception in render.");
    result = false;
  });

  return result;
}

int 
main(int argc, char** argv, char** envp) {
  bool running = true;
  double current_time = 0.0;
  double last_update_time = 0.0;

  init_game();

  // Simple test loop
  while(running) {
    current_time = glfwGetTime();

    if(current_time - last_update_time >= (32.0 / 1000.0)) {
      long delta = (int)((current_time - last_update_time) * 1000.0);

      running = update_game(delta);
      
      if(running) {
        last_update_time = current_time;

        glClear(GL_COLOR_BUFFER_BIT);
        running = render_game(delta);
        glfwSwapBuffers();
      }
    }

    // Don't check if we're not running any more
    if(running) {
      running = glfwGetWindowParam(GLFW_OPENED);
    }
  }

  return 0;
}
