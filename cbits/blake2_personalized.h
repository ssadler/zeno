#pragma once

int blake2b_256_personalized(const uint8_t* per, size_t perLen,
                             const uint8_t* inp, size_t inpLen,
                             uint8_t* out32)
