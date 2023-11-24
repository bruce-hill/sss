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

public CORD Num__cord(const double *f, bool colorize, const TypeInfo *type) { 
    CORD c;
    const char *units = strchrnul(type->name, '<');
    if (colorize) CORD_sprintf(&c, "\x1b[35m%g\x1b[33;2m%s\x1b[m", *f, units); 
    else CORD_sprintf(&c, "%g%s", *f, units); 
    return c; 
} 

static int32_t Num__compare(const double *x, const double *y, const TypeInfo *type) { 
    (void)type;
    return (*x > *y) - (*x < *y);
} 

static bool Num__equal(const double *x, const double *y, const TypeInfo *type) { 
    (void)type;
    return *x == *y;
} 

public Str_t Num__format(double f, int64_t precision) { 
    int len = snprintf(NULL, 0, "%.*f", (int)precision, f); 
    char *str = GC_MALLOC_ATOMIC(len + 1); 
    snprintf(str, len+1, "%.*f", (int)precision, f); 
    return (Str_t){.data=str, .length=len, .stride=1}; 
}

public Str_t Num__scientific(double f, int64_t precision) { 
    int len = snprintf(NULL, 0, "%.*e", (int)precision, f); 
    char *str = GC_MALLOC_ATOMIC(len + 1); 
    snprintf(str, len+1, "%.*e", (int)precision, f); 
    return (Str_t){.data=str, .length=len, .stride=1}; 
}

public double Num__mod(double num, double modulus) { 
    double result = fmod(num, modulus); 
    return (result < 0) != (modulus < 0) ? result + modulus : result; 
}

public double Num__e = M_E, Num__log2e = M_LOG2E, Num__ln2 = M_LN2, Num__ln10 = M_LN10, Num__pi = M_PI,
       Num__tau = 2.*M_PI, Num__half_pi = M_PI_2, Num__quarter_pi = M_PI_4, Num__inverse_pi = M_1_PI,
       Num__inverse_half_pi = M_2_PI, Num__2_sqrt_pi = M_2_SQRTPI, Num__sqrt2 = M_SQRT2, Num__sqrt_half = M_SQRT1_2,
       Num__NaN = NAN, Num__inf = 1./0.;

public bool Num__isinf(double n) { return isinf(n); }
public bool Num__finite(double n) { return finite(n); }
public bool Num__isnan(double n) { return isnan(n); }

public TypeInfo Num_type = {
    .name="Num",
    .size=sizeof(double),
    .align=alignof(double),
    .tag=CustomInfo,
    .CustomInfo={
        .compare=(void*)Num__compare,
        .equal=(void*)Num__equal,
        .cord=(void*)Num__cord,
    },
};

public CORD Num32__cord(float *f, bool colorize, const TypeInfo *type) { 
    CORD c;
    const char *units = strchrnul(type->name, '<');
    if (colorize) CORD_sprintf(&c, "\x1b[35m%g_f32%s\x1b[m", *f, units);
    else CORD_sprintf(&c, "%g_f32%s", *f, units);
    return c;
}

static int32_t Num32__compare(const float *x, const float *y, const TypeInfo *type) { 
    (void)type;
    return (*x > *y) - (*x < *y);
} 

static bool Num32__equal(const float *x, const float *y, const TypeInfo *type) { 
    (void)type;
    return *x == *y;
} 

public Str_t Num32__format(float f, int64_t precision) { 
    int len = snprintf(NULL, 0, "%.*f", (int)precision, f); 
    char *str = GC_MALLOC_ATOMIC(len + 1); 
    snprintf(str, len+1, "%.*f", (int)precision, f); 
    return (Str_t){.data=str, .length=len, .stride=1}; 
}

public Str_t Num32__scientific(float f, int64_t precision) { 
    int len = snprintf(NULL, 0, "%.*e", (int)precision, f); 
    char *str = GC_MALLOC_ATOMIC(len + 1); 
    snprintf(str, len+1, "%.*e", (int)precision, f); 
    return (Str_t){.data=str, .length=len, .stride=1}; 
}

public float Num32__mod(float num, float modulus) { 
    float result = fmodf(num, modulus); 
    return (result < 0) != (modulus < 0) ? result + modulus : result; 
}

public float Num32__random(void) { 
    return (float)drand48(); 
}

public float Num32__e = M_E, Num32__log2e = M_LOG2E, Num32__ln2 = M_LN2, Num32__ln10 = M_LN10, Num32__pi = M_PI,
      Num32__tau = 2.*M_PI, Num32__half_pi = M_PI_2, Num32__quarter_pi = M_PI_4, Num32__inverse_pi = M_1_PI,
      Num32__inverse_half_pi = M_2_PI, Num32__2_sqrt_pi = M_2_SQRTPI, Num32__sqrt2 = M_SQRT2,
      Num32__sqrt_half = M_SQRT1_2, Num32__NaN = NAN, Num32__inf = 1./0.;

public bool Num32__isinf(float n) { return isinf(n); }
public bool Num32__finite(float n) { return finite(n); }
public bool Num32__isnan(float n) { return isnan(n); }

public TypeInfo Num32_type = {
    .name="Num32",
    .size=sizeof(float),
    .align=alignof(float),
    .tag=CustomInfo,
    .CustomInfo={
        .compare=(void*)Num32__compare,
        .equal=(void*)Num32__equal,
        .cord=(void*)Num32__cord,
    },
};

// vim: ts=4 sw=0 et cino=L2,l1,(0,W4,m1,\:0
