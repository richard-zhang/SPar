#include<stdint.h>
#include<stdio.h>
#include<stdlib.h>
#include<chan.h>
#include<pthread.h>
#include"../data.h"
#include"../func.h"
chan_t * c1;
chan_t * c2;
chan_t * c3;
void proc1Rt()
{
    List_int v0;
    chan_recv_buf(c1, &v0, sizeof(List_int));
    Sum_Sum_unit_int_Prod_List_int_List_int v1;
    v1 = split(v0);
    Label v2;
    v2 = v1.label;
    chan_send_int(c2, v2);
    Sum_unit_int v3;
    Prod_List_int_List_int v4;
    int v5;
    if (v1.label == LEFT)
    {
        v3 = v1.value.left;
        Sum_Sum_unit_int_Prod_List_int_List_int v6;
        v6 = (Sum_Sum_unit_int_Prod_List_int_List_int) {LEFT, { .left = v3 }};
        chan_send_buf(c2,
                      &v6,
                      sizeof(Sum_Sum_unit_int_Prod_List_int_List_int));
        v5 = 0;
    }
    else
    {
        v4 = v1.value.right;
        Prod_List_int_List_int v7;
        v7 = v4;
        chan_send_buf(c2, &v7, sizeof(Prod_List_int_List_int));
        List_int v8;
        v8 = sort(v4.snd);
        chan_send_buf(c2, &v8, sizeof(List_int));
        v5 = 0;
    }
}
void proc2Rt()
{
    Sum_Sum_unit_int_Prod_List_int_List_int v10;
    Label v9;
    chan_recv_int(c2, &v9);
    if (v9 == LEFT)
    {
        Sum_Sum_unit_int_Prod_List_int_List_int v11;
        chan_recv_buf(c2,
                      &v11,
                      sizeof(Sum_Sum_unit_int_Prod_List_int_List_int));
        v10 = v11;
    }
    else
    {
        Prod_List_int_List_int v12;
        chan_recv_buf(c2, &v12, sizeof(Prod_List_int_List_int));
        List_int v13;
        v13 = sort(v12.fst);
        List_int v14;
        chan_recv_buf(c2, &v14, sizeof(List_int));
        v10 = (Sum_Sum_unit_int_Prod_List_int_List_int) {RIGHT, { .right = (Prod_List_int_List_int) {v13, v14} }};
    }
    List_int v15;
    v15 = merge(v10);
    chan_send_buf(c3, &v15, sizeof(List_int));
}
void * proc1()
{
    proc1Rt();
    return NULL;
}
void * proc2()
{
    proc2Rt();
    return NULL;
}
List_int proc0(List_int v0)
{
    c1 = chan_init(1);
    c2 = chan_init(1);
    c3 = chan_init(1);
    pthread_t th1;
    pthread_create(&th1, NULL, proc1, NULL);
    pthread_t th2;
    pthread_create(&th2, NULL, proc2, NULL);
    chan_send_buf(c1, &v0, sizeof(List_int));
    List_int v1;
    chan_recv_buf(c3, &v1, sizeof(List_int));
    pthread_join(th1, NULL);
    pthread_join(th2, NULL);
    chan_dispose(c1);
    chan_dispose(c2);
    chan_dispose(c3);
    return v1;
}
int main()
{
    int tmp[] = { 799455262, 582537186, -804370111, 2128118000, 1916241897, 2063278296, 1805600171, -2014632644, 1412700370, 1482320109, -1580844817, -1707696242, -732062849, 1140778324, 1079149609, 112738389 };
    List_int a = (List_int) {16, tmp};
    double start = get_time();
    proc0(a);
    double end = get_time();
    printf("%lf\n", end - start);
    return 0;
}
