
#include <string.h>
#include <blake2b.c>


int blake2b_256_personalized(
    const uint8_t* per, size_t perLen,
    const uint8_t* inp, size_t inpLen,
    uint8_t* out32)
{
  uint8_t outlen = 32;

  blake2b_param P[1] = {0};
  blake2b_state S[1];

  if ( ( !outlen ) || ( outlen > BLAKE2B_OUTBYTES ) ) return -1;
  if ( perLen > sizeof( P->personal ) ) return -2;

  P->digest_length = (uint8_t)outlen;
  P->fanout        = 1;
  P->depth         = 1;
  memcpy( P->personal, per, perLen );
  
  return blake2b_init_param( S, P )
    || blake2b_update( S, inp, inpLen )
    || blake2b_final( S, out32, 32 );
}


