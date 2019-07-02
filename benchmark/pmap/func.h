#ifndef FUNC_H
#define FUNC_H
#include<time.h>
#include<sys/time.h>
#include<sys/resource.h>
#include<stdio.h>
#include<stdlib.h>
#include "data.h"
#include <pthread.h>
static inline void debug(Prod_int_Prod_int_Prod_int_int a) {
    printf("%d\n", a.fst);
    printf("%d\n", a.snd.fst);
}
static inline double get_time()
{
    struct timeval t;
    gettimeofday(&t, NULL);
    return t.tv_sec + t.tv_usec*1e-6;
}
static inline int result(int a) {
    sleep(5);
    return a + 4;
}
#endif
