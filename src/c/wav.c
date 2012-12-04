#include <stdio.h>
#include <string.h>

#include "util.h"
#include "wav.h"

static bool
wav_read_riff(wav_t* wav, FILE* file_handle) {
  size_t amount = 0;
  bool result = true;

  // Read in our header information
  amount = fread(&wav->riff, sizeof(riff_format_t), 1, file_handle);

  // Error checking
  result = (amount == 1) &&
    (strncmp(wav->riff.chunk_id, "RIFF", 4) == 0) &&
    (strncmp(wav->riff.format, "WAVE", 4) == 0);

  return result;
}

static bool
wav_read_fmt(wav_t* wav, FILE* file_handle) {
  size_t amount = 0;
  bool result = true;

  // Read in data
  amount = fread(&wav->fmt, sizeof(wav_fmt_t), 1, file_handle);
  
  result = (amount == 1) &&
    (strncmp(wav->fmt.subchunk1_id, "fmt ", 4) == 0);
    
  // Extra check for compression scheme.  We don't handle any compression so
  // report an error if the wav is compressed.
  if(wav->fmt.audio_format != 1) {
    logmsg("wav file is compressed: %d", wav->fmt.audio_format);
    result = false;
  }

  return result;
}

static bool
wav_read_data(wav_t* wav, FILE* file_handle) {
  size_t amount = 0;
  bool result = true;

  amount = fread(&wav->data.subchunk2_id, 1, 4, file_handle);
  amount += fread(&wav->data.subchunk2_size, 4, 1, file_handle);

  if(amount != 5) {
    result = false; 
  } else {
    // Allocate space for the data
    wav->data.data = new_array(uint8_t, wav->data.subchunk2_size);
    amount = fread(wav->data.data, 
                   sizeof(unsigned char), 
                   wav->data.subchunk2_size, 
                   file_handle);

    result = (amount == wav->data.subchunk2_size) &&
      (strncmp(wav->data.subchunk2_id, "data", 4) == 0);
  }

  return result;
}

/**
   Allocates a new wav structure.
*/
wav_t* 
wav_new(void) {
  wav_t* wav = new(wav_t);
  wav->data.data = NULL;

  return wav;
}

wav_t* 
wav_load(char* filename) {
  FILE* file_handle = fopen(filename, "rb");
  wav_t* wav = NULL;

  if(NULL != file_handle) {
    bool result = true;

    wav = new(wav_t);

    result = wav_read_riff(wav, file_handle);
    if(!result) {
      logmsg("Unable to read fmt information from wav file: %s", filename);
    }

    if(result) {
      result = wav_read_fmt(wav, file_handle);
    }

    // Error check
    if(!result) {
      logmsg("Unable to read riff information from wav file %s", filename);
    }

    if(result) {
      wav_read_data(wav, file_handle);
    }
    
    // Error check
    if(!result) {
      logmsg("Unable to read wav data from file: %s", filename);
    }

    fclose(file_handle);
  } else {
    logmsg("Unable to open wav file %s", filename);
  }

  return wav;
}

void 
wav_delete(wav_t* wav) {
  if(wav->data.data != NULL) {
    delete(wav->data.data);
  }

  delete(wav);
}

