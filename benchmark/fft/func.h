#ifndef FUNC_H
#define FUNC_H
#include<time.h>
#include<sys/time.h>
#include<sys/resource.h>
#include<stdio.h>
#include<stdlib.h>
#include <pthread.h>
#include <math.h>
#include <complex.h>
#include "data.h"

#define PI (4.0 * atan(1.0))
typedef float complex cplx;

static inline void _fft(cplx buf[], cplx out[], int n, int step)
{
	if (step < n) {
		_fft(out, buf, n, step * 2);
		_fft(out + step, buf + step, n, step * 2);

		for (int i = 0; i < n; i += 2 * step) {
			cplx t = cexpf(-I * PI * i / n) * out[i + step];
			buf[i / 2]     = out[i] + t;
			buf[(i + n)/2] = out[i] - t;
		}
	}
}

static inline void fft(cplx buf[], int n)
{
	cplx out[n];
	for (int i = 0; i < n; i++) out[i] = buf[i];

	_fft(buf, out, n, 1);
}

static inline double get_time()
{
    struct timeval t;
    gettimeofday(&t, NULL);
    return t.tv_sec + t.tv_usec*1e-6;
}

static inline void debug(List_Prod_float_float a) {
    for(size_t i = 0; i < a.size; i++) {
        printf("%g %g\n", a.value[i].fst, a.value[i].snd);
    }
}

static inline cplx toComplex(Prod_float_float a) {
    return a.fst + a.snd * I;
}

static inline Prod_float_float fromComplex(cplx a) {
    return (Prod_float_float) {crealf(a), cimagf(a)};
}

static void compute_dft_complex(const cplx input[], Prod_float_float output[], int n) {
	for (int k = 0; k < n; k++) {  // For each output element
		cplx sum = 0.0;
		for (int t = 0; t < n; t++) {  // For each input element
			float angle = 2 * PI * t * k / n;
			sum += input[t] * cexpf(-angle * I);
		}
		output[k] = fromComplex(sum);
	}
} 

static inline List_Prod_float_float baseFFT(List_Prod_float_float a) {
    // cplx * in = (cplx *) a.value;
    // size_t size = a.size;
    // for(size_t i = 0; i < size; i++) {
    //     in[i] = toComplex(a.value[i]);
    // }
    // fft(in, size);
    // for(size_t i = 0; i < size; i++) {
    //     a.value[i] = fromComplex(in[i]);
    // }
    size_t size = a.size;
	cplx out[size];
    for(size_t i = 0; i < size; i++) {
        out[i] = toComplex(a.value[i]);
    }
    compute_dft_complex(out, a.value, size);
    return a;
}

static inline List_Prod_float_float addPadding(List_Prod_float_float a) {
    return a;
}

static inline Prod_float_float minus(Prod_float_float a, Prod_float_float b) {
    return (Prod_float_float) {a.fst - b.fst, a.snd - b.snd};
}

static inline Prod_float_float plus(Prod_float_float a, Prod_float_float b) {
    return (Prod_float_float) {a.fst + b.fst, a.snd + b.snd};
}


static inline List_Prod_float_float addc(List_Prod_float_float a, List_Prod_float_float b) {
    size_t size = a.size;
    for (size_t i = 0; i < size; i++) {
        a.value[i] = plus(a.value[i], b.value[i]);
    }
    return a;
}

static inline List_Prod_float_float subc(List_Prod_float_float a, List_Prod_float_float b) {
    size_t size = a.size;
    for (size_t i = 0; i < size; i++) {
        b.value[i] = minus(a.value[i], b.value[i]);
    }
    return b;
}

static inline Prod_List_Prod_float_float_List_Prod_float_float addSub(Prod_List_Prod_float_float_List_Prod_float_float a) {
    size_t size = a.fst.size;
    Prod_float_float tmp[size]; 
    List_Prod_float_float tmpLeft = {size, tmp};
    for(size_t i = 0; i < size; i++) {
        tmp[i] = a.fst.value[i];
    }
    Prod_List_Prod_float_float_List_Prod_float_float ret = {addc(a.fst, a.snd), subc(tmpLeft, a.snd)};
    return ret;
}


static inline List_Prod_float_float concatenate(Prod_List_Prod_float_float_List_Prod_float_float a) {
    return (List_Prod_float_float) {a.fst.size + a.snd.size, a.fst.value};
}

static inline Prod_List_Prod_float_float_List_Prod_float_float splitList(List_Prod_float_float a) {
    size_t size = a.size / 2;
    Prod_float_float odd[size];
    Prod_float_float even[size];
    for(size_t i = 0; i < size; i++) {
        odd[i] = a.value[2*i + 1];
        even[i] = a.value[2*i];
    }

    for(size_t i = 0; i < size; i++) {
        a.value[i] = even[i];
        a.value[i+size] = odd[i];
    }

    List_Prod_float_float left = {size, a.value};
    List_Prod_float_float right = {size, a.value + size};
    return (Prod_List_Prod_float_float_List_Prod_float_float) {left, right};
}



static inline List_Prod_float_float cmulexp(int p2sx, int i, List_Prod_float_float l) {
    size_t size = l.size;
    for(size_t index = 0; index < size; index++) {
        Prod_float_float z = l.value[index];

        int k = i * size + index;
        int n = p2sx * size;
        cplx expkn = toComplex(z) * cexpf(I * -2.0 * PI * k / (float) n);
        l.value[index] = fromComplex(expkn);
    }
    return l;
}

#endif

