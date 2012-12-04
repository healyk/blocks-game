/**
   @file wav.h

   Interface for reading .wav files.
*/

#include <stdint.h>
#include <stdbool.h>

/**
   Details the RIFF data area.  This is used to identify various media file 
   formats.
*/
typedef struct riff_format {
  /** This should simply contain the letters "RIFF" in ASCII (big-endian). */
  char chunk_id[4];

  /**
     Should be 4 + (* + subchunk1_size) + (8 + subchunk2_size) 
     Basically it's the size of the file minus the this field and chunk_id
     (8 bytes).
   */
  uint32_t chunk_size;

  /* Contains the letters "WAVE" in ASCII (big-endian). */
  char format[4];
} riff_format_t;

/**
   Details the format for a wav file.
*/
typedef struct wav_fmt {
  /** Contains the letters "fmt". */
  char     subchunk1_id[4];

  /** This is the size of subchunk 1 */
  uint32_t subchunk1_size;

  /** Audio format.  PCM = 1, Compressed = non-1. */
  uint16_t audio_format;

  /** Mono = 1, Stereo = 2, ... */
  uint16_t num_channels;

  /** Sample qualitity.  22050, 44100, ... */ 
  uint32_t sample_rate;

  /** sample_rate * num_channels * bits_per_sample / 8 */
  uint32_t byte_rate;

  /** num_channels * bits_per_sample / 8.  Number of bytes for one sample
      including all channels. */
  uint16_t block_align;

  /** 8 = 8bit, 16 = 16bit and so forth. */
  uint16_t bits_per_sample;
} wav_fmt_t;

typedef struct wav_data {
  /** Contains the letters "data" */
  char     subchunk2_id[4];

  /**
     num_samples * num_channels * bits_per_sample / 8.
     Size of the data field.
  */
  uint32_t subchunk2_size;

  unsigned char* data;
} wav_data_t;

typedef struct wav {
  riff_format_t riff;
  wav_fmt_t     fmt;
  wav_data_t    data;
} wav_t;

/**
   Reads in the wav file from a filename.
   @param filename
     Name + path of the file you want to load.
   @return
     A new wav_t or NULL if there was a problem loading the file.
*/
wav_t* wav_load(char* filename);

/**
   Cleans up an existing wav structure.
   @param wav
     wav_t to remove from memory.
*/
void wav_delete(wav_t* wav);
