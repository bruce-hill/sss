#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <gc.h>
#include <gc/cord.h>
#include <math.h>
#include <stdalign.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#include "../SipHash/halfsiphash.h"
#include "array.h"
#include "string.h"
#include "types.h"

static CORD Num_cord(const double *f, bool colorize, const Type *type) { 
    (void)type;
    CORD c; 
    if (colorize) CORD_sprintf(&c, "\x1b[35m%g\x1b[m", *f); 
    else CORD_sprintf(&c, "%g", *f); 
    return c; 
} 

Str_t Num__format(double f, int64_t precision) { 
    int len = snprintf(NULL, 0, "%.*f", (int)precision, f); 
    char *str = GC_MALLOC_ATOMIC(len + 1); 
    snprintf(str, len+1, "%.*f", (int)precision, f); 
    return (Str_t){.data=str, .length=len, .stride=1}; 
} 

double Num__mod(double num, double modulus) { 
    double result = fmod(num, modulus); 
    return (result < 0) != (modulus < 0) ? result + modulus : result; 
}

double Num__e = M_E, Num__log2e = M_LOG2E, Num__ln2 = M_LN2, Num__ln10 = M_LN10, Num__pi = M_PI,
       Num__tau = 2.*M_PI, Num__half_pi = M_PI_2, Num__quarter_pi = M_PI_4, Num__inverse_pi = M_1_PI,
       Num__inverse_half_pi = M_2_PI, Num__2_sqrt_pi = M_2_SQRTPI, Num__sqrt2 = M_SQRT2, Num__sqrt_half = M_SQRT1_2,
       Num__NaN = NAN, Num__inf = 1./0.;

bool Num__isinf(double n) { return isinf(n); }
bool Num__finite(double n) { return finite(n); }
bool Num__isnan(double n) { return isnan(n); }

Type Num_type = {
    .name="Num",
    .size=sizeof(double),
    .align=alignof(double),
    .tag=VTableInfo,
    .VTableInfo={.cord=(void*)Num_cord},
    // .bindings=STATIC_ARRAY((void*)
    //     Num_mod, Num_format, drand48,
    //     // Unops
    //     acos, asin, atan, cos, sin, tan, cosh, sinh, tanh,
    //     acosh, asinh, atanh, exp, log, log10, exp10, expm1,
    //     log1p, logb, exp2, log2, sqrt, cbrt, ceil, fabs,
    //     floor, significand, j0, j1, y0, y1, erf, erfc,
    //     tgamma, rint, nextdown, nextup, round,
    //     trunc, roundeven,
    //     // Binops
    //     atan2, pow, hypot, copysign, nextafter,
    //     remainder, fmaxmag, fminmag, fdim,
    //     // Predicates
    //     isinf, finite, isnan,
    //     // Constants
    //     (double[]){M_E}, (double[]){M_LOG2E}, (double[]){M_LOG10E}, (double[]){M_LN2},
    //     (double[]){M_LN10}, (double[]){M_PI}, (double[]){2.*M_PI}, (double[]){M_PI_2},
    //     (double[]){M_PI_4}, (double[]){M_1_PI}, (double[]){M_2_PI},
    //     (double[]){M_2_SQRTPI}, (double[]){M_SQRT2}, (double[]){M_SQRT1_2},
    //     (double[]){NAN}, (double[]){1./0.},
    // ),
};
#undef UNOP
#undef BINOP
#undef PRED
#undef CONST

static CORD Num32_cord(float *f, bool colorize, const Type *type) { 
    (void)type;
    CORD c; 
    if (colorize) CORD_sprintf(&c, "\x1b[35m%g\x1b[m", *f); 
    else CORD_sprintf(&c, "%g", *f); 
    return c; 
} 

Str_t Num32__format(float f, int64_t precision) { 
    int len = snprintf(NULL, 0, "%.*f", (int)precision, f); 
    char *str = GC_MALLOC_ATOMIC(len + 1); 
    snprintf(str, len+1, "%.*f", (int)precision, f); 
    return (Str_t){.data=str, .length=len, .stride=1}; 
} 

float Num32__mod(float num, float modulus) { 
    float result = fmodf(num, modulus); 
    return (result < 0) != (modulus < 0) ? result + modulus : result; 
}

float Num32__random(void) { 
    return (float)drand48(); 
}

float Num32__e = M_E, Num32__log2e = M_LOG2E, Num32__ln2 = M_LN2, Num32__ln10 = M_LN10, Num32__pi = M_PI,
      Num32__tau = 2.*M_PI, Num32__half_pi = M_PI_2, Num32__quarter_pi = M_PI_4, Num32__inverse_pi = M_1_PI,
      Num32__inverse_half_pi = M_2_PI, Num32__2_sqrt_pi = M_2_SQRTPI, Num32__sqrt2 = M_SQRT2,
      Num32__sqrt_half = M_SQRT1_2, Num32__NaN = NAN, Num32__inf = 1./0.;

bool Num32__isinf(float n) { return isinf(n); }
bool Num32__finite(float n) { return finite(n); }
bool Num32__isnan(float n) { return isnan(n); }

#define UNOP(name) {#name, "func(n:Num32) Num32", #name}
#define BINOP(name) {#name, "func(x:Num32, y:Num32) Num32", #name}
#define PRED(name) {#name, "func(n:Num32) Bool", #name}
#define CONST(name, c_name) {#name, "Num32", (float[]){c_name}}
Type Num32_type = {
    .name="Num32",
    .size=sizeof(float),
    .align=alignof(float),
    .tag=VTableInfo,
    .VTableInfo={.cord=(void*)Num32_cord},
    // .bindings=STATIC_ARRAY((void*)
    //     Num32_mod, Num32_format, Num32_random,
    //     // Unops
    //     acos, asin, atan, cos, sin, tan, cosh, sinh, tanh,
    //     acosh, asinh, atanh, exp, log, log10, exp10, expm1,
    //     log1p, logb, exp2, log2, sqrt, cbrt, ceil, fabs,
    //     floor, significand, j0, j1, y0, y1, erf, erfc,
    //     tgamma, rint, nextdown, nextup, round,
    //     trunc, roundeven,
    //     // Binops
    //     atan2, pow, hypot, copysign, nextafter,
    //     remainder, fmaxmag, fminmag, fdim,
    //     // Predicates
    //     isinf, finite, isnan,
    //     // Constants
    //     (double[]){M_E}, (double[]){M_LOG2E}, (double[]){M_LOG10E}, (double[]){M_LN2},
    //     (double[]){M_LN10}, (double[]){M_PI}, (double[]){2.*M_PI}, (double[]){M_PI_2},
    //     (double[]){M_PI_4}, (double[]){M_1_PI}, (double[]){M_2_PI},
    //     (double[]){M_2_SQRTPI}, (double[]){M_SQRT2}, (double[]){M_SQRT1_2},
    //     (double[]){NAN}, (double[]){1./0.},
    // ),
};
#undef UNOP
#undef BINOP
#undef CONST
#undef PRED

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
