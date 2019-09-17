#ifndef DATA_H
#define DATA_H
typedef enum Label {
            LEFT, RIGHT
        } Label;
typedef struct List_int {
            size_t size; int * value;
        } List_int;
typedef struct Sum_unit_int {
            Label label;
            union {
                int left; int right;
            } value;
        } Sum_unit_int;
typedef struct Prod_float_float {
            float fst; float snd;
        } Prod_float_float;
typedef struct Prod_List_int_List_int {
            List_int fst; List_int snd;
        } Prod_List_int_List_int;
typedef struct List_Prod_float_float {
            size_t size; Prod_float_float * value;
        } List_Prod_float_float;
typedef struct Sum_Sum_unit_int_Prod_List_int_List_int {
            Label label;
            union {
                Sum_unit_int left; Prod_List_int_List_int right;
            } value;
        } Sum_Sum_unit_int_Prod_List_int_List_int;
typedef struct Prod_List_Prod_float_float_List_Prod_float_float {
            List_Prod_float_float fst; List_Prod_float_float snd;
        } Prod_List_Prod_float_float_List_Prod_float_float;
#endif
