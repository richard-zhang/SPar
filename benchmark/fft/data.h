#ifndef DATA_H
#define DATA_H
typedef enum Label {
            LEFT, RIGHT
        } Label;
typedef struct Prod_float_float {
            float fst; float snd;
        } Prod_float_float;
typedef struct List_Prod_float_float {
            size_t size; Prod_float_float * value;
        } List_Prod_float_float;
typedef struct Prod_List_Prod_float_float_List_Prod_float_float {
            List_Prod_float_float fst; List_Prod_float_float snd;
        } Prod_List_Prod_float_float_List_Prod_float_float;
#endif
