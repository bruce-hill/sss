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
#include "string.h"
#include "types.h"

static CORD Num_cord(const double *f, bool colorize, const Type *type) { 
    (void)type;
    CORD c; 
    if (colorize) CORD_sprintf(&c, "\x1b[35m%g\x1b[m", *f); 
    else CORD_sprintf(&c, "%g", *f); 
    return c; 
} 

static String_t Num_format(double f, int64_t precision) { 
    int len = snprintf(NULL, 0, "%.*f", (int)precision, f); 
    char *str = GC_MALLOC_ATOMIC(len + 1); 
    snprintf(str, len+1, "%.*f", (int)precision, f); 
    return (String_t){.data=str, .length=len, .stride=1}; 
} 

static double Num_mod(double num, double modulus) { 
    double result = fmod(num, modulus); 
    return (result < 0) != (modulus < 0) ? result + modulus : result; 
}

#define UNOP(name) {#name, "func(n:Num) Num", #name}
#define BINOP(name) {#name, "func(x:Num, y:Num) Num", #name}
#define PRED(name) {#name, "func(n:Num) Bool", #name}
#define CONST(name, c_name) {#name, "Num", (double[]){c_name}}
Type Num_type = {
    .name="Num",
    .size=sizeof(double),
    .align=alignof(double),
    .cord=CordMethod(Function, (void*)Num_cord),
    .order=OrderingMethod(Data, sizeof(double)),
    .hash=HashMethod(Data, sizeof(double)),
    .bindings=(NamespaceBinding[]){
        {"mod", "func(n:Num, modulus:Num) Num", Num_mod},
        {"format", "func(n:Num, precision:Int) Num", Num_format},
        {"random", "func() Num", drand48},
        // Unops
        UNOP(acos), UNOP(asin), UNOP(atan), UNOP(cos), UNOP(sin), UNOP(tan), UNOP(cosh), UNOP(sinh), UNOP(tanh),
        UNOP(acosh), UNOP(asinh), UNOP(atanh), UNOP(exp), UNOP(log), UNOP(log10), UNOP(exp10), UNOP(expm1),
        UNOP(log1p), UNOP(logb), UNOP(exp2), UNOP(log2), UNOP(sqrt), UNOP(cbrt), UNOP(ceil), UNOP(fabs),
        UNOP(floor), UNOP(significand), UNOP(j0), UNOP(j1), UNOP(y0), UNOP(y1), UNOP(erf), UNOP(erfc),
        UNOP(tgamma), UNOP(rint), UNOP(nextdown), UNOP(nextup), UNOP(round),
        UNOP(trunc), UNOP(roundeven),
        // Binops
        BINOP(atan2), BINOP(pow), BINOP(hypot), BINOP(copysign), BINOP(nextafter),
        BINOP(remainder), BINOP(fmaxmag), BINOP(fminmag), BINOP(fdim),
        // Predicates
        PRED(isinf), PRED(finite), PRED(isnan),
        // Constants
        CONST(E, M_E), CONST(Log2_E, M_LOG2E), CONST(Log10_E, M_LOG10E), CONST(Ln_2, M_LN2),
        CONST(Ln_10, M_LN10), CONST(Pi, M_PI), CONST(Tau, 2.*M_PI), CONST(HalfPi, M_PI_2),
        CONST(QuarterPi, M_PI_4), CONST(InversePi, M_1_PI), CONST(DoubleInversePi, M_2_PI),
        CONST(DoubleInverseSqrtPi, M_2_SQRTPI), CONST(Sqrt2, M_SQRT2), CONST(InverseSqrt2, M_SQRT1_2),
        CONST(NaN, nan("")), CONST(Infinity, 1./0.),
        {NULL, NULL, NULL},
    },
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

static String_t Num32_format(float f, int64_t precision) { 
    int len = snprintf(NULL, 0, "%.*f", (int)precision, f); 
    char *str = GC_MALLOC_ATOMIC(len + 1); 
    snprintf(str, len+1, "%.*f", (int)precision, f); 
    return (String_t){.data=str, .length=len, .stride=1}; 
} 

static float Num32_mod(float num, float modulus) { 
    float result = fmodf(num, modulus); 
    return (result < 0) != (modulus < 0) ? result + modulus : result; 
}

static float Num32_random(void) { 
    return (float)drand48(); 
} 

#define UNOP(name) {#name, "func(n:Num32) Num32", #name}
#define BINOP(name) {#name, "func(x:Num32, y:Num32) Num32", #name}
#define PRED(name) {#name, "func(n:Num32) Bool", #name}
#define CONST(name, c_name) {#name, "Num32", (float[]){c_name}}
Type Num32_type = {
    .name="Num32",
    .size=sizeof(float),
    .align=alignof(float),
    .cord=CordMethod(Function, (void*)Num32_cord),
    .order=OrderingMethod(Data, sizeof(float)),
    .hash=HashMethod(Data, sizeof(float)),
    .bindings=(NamespaceBinding[]){
        {"mod", "func(n:Num32, modulus:Num32) Num32", Num32_mod},
        {"format", "func(n:Num32, precision:Int) Num32", Num32_format},
        {"random", "func() Num32", Num32_random},
        // Unops
        UNOP(acos), UNOP(asin), UNOP(atan), UNOP(cos), UNOP(sin), UNOP(tan), UNOP(cosh), UNOP(sinh), UNOP(tanh),
        UNOP(acosh), UNOP(asinh), UNOP(atanh), UNOP(exp), UNOP(log), UNOP(log10), UNOP(exp10), UNOP(expm1),
        UNOP(log1p), UNOP(logb), UNOP(exp2), UNOP(log2), UNOP(sqrt), UNOP(cbrt), UNOP(ceil), UNOP(fabs),
        UNOP(floor), UNOP(significand), UNOP(j0), UNOP(j1), UNOP(y0), UNOP(y1), UNOP(erf), UNOP(erfc),
        UNOP(tgamma), UNOP(rint), UNOP(nextdown), UNOP(nextup), UNOP(round),
        UNOP(trunc), UNOP(roundeven),
        // Binops
        BINOP(atan2), BINOP(pow), BINOP(hypot), BINOP(copysign), BINOP(nextafter),
        BINOP(remainder), BINOP(fmaxmag), BINOP(fminmag), BINOP(fdim),
        // Predicates
        PRED(isinf), PRED(finite), PRED(isnan),
        // Constants
        CONST(E, M_E), CONST(Log2_E, M_LOG2E), CONST(Log10_E, M_LOG10E), CONST(Ln_2, M_LN2),
        CONST(Ln_10, M_LN10), CONST(Pi, M_PI), CONST(Tau, 2.*M_PI), CONST(HalfPi, M_PI_2),
        CONST(QuarterPi, M_PI_4), CONST(InversePi, M_1_PI), CONST(DoubleInversePi, M_2_PI),
        CONST(DoubleInverseSqrtPi, M_2_SQRTPI), CONST(Sqrt2, M_SQRT2), CONST(InverseSqrt2, M_SQRT1_2),
        CONST(NaN, nan("")), CONST(Infinity, 1./0.),
        {NULL, NULL, NULL},
    },
};
#undef UNOP
#undef BINOP
#undef CONST
#undef PRED

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
