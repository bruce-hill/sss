// Base 64 encoding/decoding
#include <gc.h>
#include <string.h>

#include "string.h"

static const unsigned char base64_table[65] =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

string_t base64_encode(string_t raw)
{
    unsigned char *out, *pos;
    const unsigned char *end, *in;

    size_t olen;

    olen = 4*((raw.length + 2) / 3); /* 3-byte blocks to 4-byte */

    if (olen < (size_t)raw.length)
        return (string_t){0,0,0,0}; /* integer overflow */

    string_t ret = {
        .data = GC_MALLOC_ATOMIC(olen+1),
        .length = olen,
        .stride = 1,
        .free = 0,
    };
    out = (unsigned char*)ret.data;

    end = (const unsigned char*)raw.data + raw.length;
    in = (const unsigned char*)raw.data;
    pos = out;
    while (end - in >= 3) {
        *pos++ = base64_table[in[0] >> 2];
        *pos++ = base64_table[((in[0] & 0x03) << 4) | (in[1] >> 4)];
        *pos++ = base64_table[((in[1] & 0x0f) << 2) | (in[2] >> 6)];
        *pos++ = base64_table[in[2] & 0x3f];
        in += 3;
    }

    if (end - in) {
        *pos++ = base64_table[in[0] >> 2];
        if (end - in == 1) {
            *pos++ = base64_table[(in[0] & 0x03) << 4];
            *pos++ = '=';
        }
        else {
            *pos++ = base64_table[((in[0] & 0x03) << 4) |
                (in[1] >> 4)];
            *pos++ = base64_table[(in[1] & 0x0f) << 2];
        }
        *pos++ = '=';
    }

    return ret;
}


static const int B64index[256] = { -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
    -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
    -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 62, 63, 62, 62, 63, 52, 53, 54, 55,
    56, 57, 58, 59, 60, 61,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  0,  1,  2,  3,  4,  5,  6,
    7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,  -1,
    -1,  -1,  -1, 63,  0, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
    41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51,
    -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1,-1,
};

typedef struct {
    bool success;
    string_t value;
} b64decode_result_t;
b64decode_result_t base64_decode(string_t b64)
{
    unsigned char* p = (unsigned char*)b64.data;
    int32_t len = b64.length;
    int32_t stride = b64.stride;
    int pad = len > 0 && (len % 4 || p[(len - 1)*stride] == '=');
    int32_t L = ((len + 3) / 4 - pad) * 4;
    int32_t ret_len = L / 4 * 3 + pad;
    string_t ret = {
        .data = GC_MALLOC_ATOMIC(ret_len + 2),
        .length = 0,
        .stride = 1,
        .free = 0,
    };
    char *str = (char*)ret.data;
    for (int32_t i = 0; i < L; i += 4) {
        int n = B64index[p[i*stride]] << 18 | B64index[p[(i + 1)*stride]] << 12 | B64index[p[(i + 2)*stride]] << 6 | B64index[p[(i + 3)*stride]];
        if (n < 0) return (b64decode_result_t){.success=false};
        str[ret.length++] = n >> 16;
        str[ret.length++] = n >> 8 & 0xFF;
        str[ret.length++] = n & 0xFF;
    }

    if (pad) {
        int n = B64index[p[L*stride]] << 18 | B64index[p[(L + 1)*stride]] << 12;
        str[ret.length++] = n >> 16;

        if (len > L + 2 && p[(L + 2)*stride] != '=') {
            n |= B64index[p[(L + 2)*stride]] << 6;
            str[ret.length++] = (n >> 8 & 0xFF);
        }
    }
    return (b64decode_result_t){.success=true, .value=ret};
}
