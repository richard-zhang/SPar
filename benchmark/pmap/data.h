#ifndef DATA_H
#define DATA_H
typedef enum Label {
            LEFT, RIGHT
        } Label;
typedef struct Prod_int_int {
            int fst; int snd;
        } Prod_int_int;
typedef struct Prod_float_float {
            float fst; float snd;
        } Prod_float_float;
typedef struct Prod_int_Prod_int_int {
            int fst; Prod_int_int snd;
        } Prod_int_Prod_int_int;
typedef struct List_Prod_float_float {
            size_t size; Prod_float_float * value;
        } List_Prod_float_float;
typedef struct Prod_int_Prod_int_Prod_int_int {
            int fst; Prod_int_Prod_int_int snd;
        } Prod_int_Prod_int_Prod_int_int;
typedef struct Prod_List_Prod_float_float_List_Prod_float_float {
            List_Prod_float_float fst; List_Prod_float_float snd;
        } Prod_List_Prod_float_float_List_Prod_float_float;
#endif
