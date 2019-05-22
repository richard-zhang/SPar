#ifndef DATA_H
#define DATA_H
typedef enum Label {
            LEFT, RIGHT
        } Label;
typedef struct Prod_int_int {
            int fst; int snd;
        } Prod_int_int;
typedef struct List_int {
            size_t size; int * value;
        } List_int;
typedef struct List_Prod_int_int {
            size_t size; Prod_int_int * value;
        } List_Prod_int_int;
typedef struct Prod_List_int_List_int {
            List_int fst; List_int snd;
        } Prod_List_int_List_int;
typedef struct Prod_List_Prod_int_int_List_Prod_int_int {
            List_Prod_int_int fst; List_Prod_int_int snd;
        } Prod_List_Prod_int_int_List_Prod_int_int;
typedef struct Sum_List_int_Prod_List_int_List_int {
            Label label;
            union {
                List_int left; Prod_List_int_List_int right;
            } value;
        } Sum_List_int_Prod_List_int_List_int;
typedef struct Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int {
            Label label;
            union {
                List_int left; Prod_List_Prod_int_int_List_Prod_int_int right;
            } value;
        } Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int;
#endif
