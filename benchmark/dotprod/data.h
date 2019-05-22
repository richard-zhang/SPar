#ifndef DATA_H
#define DATA_H
typedef enum Label {
            LEFT, RIGHT
        } Label;
typedef struct List_int {
            size_t size; int * value;
        } List_int;
typedef struct Prod_int_int {
            int fst; int snd;
        } Prod_int_int;
typedef struct Prod_List_int_List_int {
            List_int fst; List_int snd;
        } Prod_List_int_List_int;
typedef struct Sum_unit_Prod_int_int {
            Label label;
            union {
                int left; Prod_int_int right;
            } value;
        } Sum_unit_Prod_int_int;
typedef struct Sum_Sum_unit_Prod_int_int_Prod_int_int {
            Label label;
            union {
                Sum_unit_Prod_int_int left; Prod_int_int right;
            } value;
        } Sum_Sum_unit_Prod_int_int_Prod_int_int;
typedef struct Prod_Prod_List_int_List_int_Prod_List_int_List_int {
            Prod_List_int_List_int fst; Prod_List_int_List_int snd;
        } Prod_Prod_List_int_List_int_Prod_List_int_List_int;
typedef struct Sum_Sum_unit_Prod_int_int_Prod_Prod_List_int_List_int_Prod_List_int_List_int {
            Label label;
            union {
                Sum_unit_Prod_int_int left;
                Prod_Prod_List_int_List_int_Prod_List_int_List_int right;
            } value;
        } Sum_Sum_unit_Prod_int_int_Prod_Prod_List_int_List_int_Prod_List_int_List_int;
#endif
