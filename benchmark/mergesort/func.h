#ifndef FUNC_H
#define FUNC_H
#include<time.h>
#include<sys/time.h>
#include<sys/resource.h>
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include "data.h"
#define SORT_NAME int
#define SORT_TYPE int
#define SORT_CMP(x, y) ((x) - (y))
#include "sort.h"

static inline double get_time()
{
    struct timeval t;
    gettimeofday(&t, NULL);
    return t.tv_sec + t.tv_usec*1e-6;
}

static inline debug(List_int a) {
    if (a.size > 0) {
        printf("%d\n", a.value[0]);
        printf("%d\n", a.value[a.size - 1]);
    }
}

static inline int* randomList(size_t a) {
    srand((unsigned int)time(NULL));
    int * tmp = (int *) malloc(sizeof(int) * a);
    for (size_t i = 0; i < a; i++) {
        tmp[i] = rand() + rand();
    }
    return tmp;
}

static inline void quicksort(int *A, int len) {
    int_merge_sort_in_place(A, len);
//   if (len < 2) return;
 
//   int pivot = A[len / 2];
 
//   int i, j;
//   for (i = 0, j = len - 1; ; i++, j--) {
//     while (A[i] < pivot) i++;
//     while (A[j] > pivot) j--;
 
//     if (i >= j) break;
 
//     int temp = A[i];
//     A[i]     = A[j];
//     A[j]     = temp;
//   }
 
//   quicksort(A, i);
//   quicksort(A + i, len - i);
}

static inline void printList(List_int a) {
    printf("size is %u\n", a.size);
    for(size_t i = 0; i < a.size; i++) {
        printf("%d ", a.value[i]);
    }
    printf("\n");
}

static inline Sum_Sum_unit_int_Prod_List_int_List_int split(List_int a) {
    size_t size = a.size;
    int *value = a.value;
    Sum_unit_int single;
    if (size == 0) {
        single = (Sum_unit_int) {LEFT, {.left = -1}};
        return (Sum_Sum_unit_int_Prod_List_int_List_int) {LEFT, {.left = single}};
    } else if (size == 1) {
        single = (Sum_unit_int) {RIGHT, {.right = value[0]}};
        return (Sum_Sum_unit_int_Prod_List_int_List_int) {LEFT, {.left = single}};
    } else {
        size_t leftSize = size/2;
        size_t rightSize = size - leftSize;
        List_int left = (List_int) {leftSize, value};
        List_int right = (List_int) {rightSize, value + leftSize};
        Prod_List_int_List_int prodList = (Prod_List_int_List_int) {left, right};
        return (Sum_Sum_unit_int_Prod_List_int_List_int) {RIGHT, {.right = prodList}};
    }
}

static inline int compare (const void * a, const void * b)
{
  return ( *(int*)a - *(int*)b );
}

static inline List_int sort(List_int a) {
    quicksort(a.value, a.size);
    return a;
}

static inline List_int merge(Sum_Sum_unit_int_Prod_List_int_List_int a) {
    if (a.label == LEFT) {
        Sum_unit_int val = a.value.left;
        if (val.label == LEFT) {
            return (List_int) {0, NULL};
        } else {
            // memory leak 
            int *single = (int *) malloc(sizeof(int));
            *single = val.value.right;
            return (List_int) {1, single};
        }
    } else {
        List_int left = a.value.right.fst;
        List_int right = a.value.right.snd;
        int *lval = left.value;
        int *rval = right.value;
        size_t leftSize = left.size;
        size_t rightSize = right.size;
        size_t totalSize = leftSize + rightSize;
        int* tmp = (int*) malloc(totalSize * sizeof(int));
        size_t index = 0;
        size_t leftIndex = 0;
        size_t rightIndex = 0;
        while(leftIndex < leftSize && rightIndex < rightSize) {
            if (lval[leftIndex] <= rval[rightIndex]) {
                tmp[index] = lval[leftIndex];
                leftIndex++;
            } else {
                tmp[index] = rval[rightIndex];
                rightIndex++;
            }
            index++;
        }

        if (leftIndex < leftSize) {
            memcpy(tmp + index, lval + leftIndex, (totalSize - index) * sizeof(int));
        }

        if (rightIndex < rightSize) {
            memcpy(tmp + index, rval + rightIndex, (totalSize - index) * sizeof(int));
        }

        memcpy(lval, tmp, leftSize * sizeof(int)); 
        memcpy(rval, tmp + leftSize, rightSize * sizeof(int));
        free(tmp);
        return (List_int) {totalSize, lval};
    }
}
static inline int comp (const void * elem1, const void * elem2) 
{
    int f = *((int*)elem1);
    int s = *((int*)elem2);
    if (f > s) return  1;
    if (f < s) return -1;
    return 0;
}
#endif
