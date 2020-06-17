
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include <secp256k1.h>
#include <secp256k1_recovery.h>


static secp256k1_context* ctx;

void __attribute__ ((constructor)) premain()
{
    ctx = secp256k1_context_create(SECP256K1_CONTEXT_SIGN | SECP256K1_CONTEXT_VERIFY);

    unsigned char seed[32];
    int read;
#ifdef SYS_getrandom
    read = syscall(SYS_getrandom, seed, 32, 0);
#else
    FILE *fp = fopen("/dev/urandom", "r");
    read = (int) fread(&seed, 1, 32, fp);
    fclose(fp);
#endif

    if (read != 32) {
        fprintf(stderr, "%s: Could not initialize secp256k1 context\n", __FILE__);
        exit(1);
    }

    if (!secp256k1_context_randomize(ctx, seed)) {
        fprintf(stderr, "secp256k1_context_randomize failed\n");
        exit(1);
    }
}

int secp256k1_recoverable_sign(
    secp256k1_ecdsa_recoverable_signature *sig,
    const unsigned char *msg32,
    const unsigned char *seckey)
{
    if (!secp256k1_ecdsa_sign_recoverable(ctx, sig, msg32, seckey, NULL, NULL)) {
        fprintf(stderr, "secp256k1_ecdsa_sign_recoverable failed\n");
        return -1;
    }
    return 1;
}

int secp256k1_recoverable_recover(
    unsigned char *pubkey33,
    const secp256k1_ecdsa_recoverable_signature *sig,
    const unsigned char *msg32)
{
    secp256k1_pubkey pk;
    if (!secp256k1_ecdsa_recover(ctx, &pk, sig, msg32)) {
        fprintf(stderr, "secp256k1_ecdsa_recover failed\n");
        return -1;
    }
    size_t out_size = 33;
    secp256k1_ec_pubkey_serialize(ctx, pubkey33, &out_size, &pk, SECP256K1_EC_COMPRESSED);
    if (out_size != 33) {
        fprintf(stderr, "secp256k1_ec_pubkey_serialize in secp256k1_recoverable_recover failed\n");
        return -1;
    }
    return 1;
}

int secp256k1_recoverable_derive_pubkey(unsigned char *pubkey33, const unsigned char *seckey)
{
    secp256k1_pubkey pubkey;
    if (!secp256k1_ec_pubkey_create(ctx, &pubkey, seckey)) {
        fprintf(stderr, "secp256k1_ec_pubkey_create failed\n");
        return -1;
    }
    size_t out_size = 33;
    secp256k1_ec_pubkey_serialize(ctx, pubkey33, &out_size, &pubkey, SECP256K1_EC_COMPRESSED);
    if (out_size != 33) {
        fprintf(stderr, "secp256k1_ec_pubkey_serialize failed\n");
        return -1;
    }
    return 1;
}

int secp256k1_recoverable_pubkey_serialize_der(unsigned char* pubkey65, const unsigned char* pubkey33)
{
    secp256k1_pubkey pubkey;
    if (!secp256k1_ec_pubkey_parse(ctx, &pubkey, pubkey33, 33)) {
        fprintf(stderr, "secp256k1_ec_pubkey_parse in secp256k1_recoverable_pubkey_serialize_der failed\n");
        return -1;
    }
    size_t out_size = 65;
    secp256k1_ec_pubkey_serialize(ctx, pubkey65, &out_size, &pubkey, SECP256K1_EC_UNCOMPRESSED);
    if (out_size != 65) {
        fprintf(stderr, "secp256k1_ec_pubkey_serialize in secp256k1_recoverable_pubkey_serialize_der failed\n");
        return -1;
    }
    return 1;
}
