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
chan_t * c4;
chan_t * c5;
chan_t * c6;
chan_t * c7;
chan_t * c8;
void proc1Rt()
{
    List_Prod_int_int v0;
    chan_recv_buf(c1, &v0, sizeof(List_Prod_int_int));
    List_Prod_int_int v1;
    v1 = v0;
    chan_send_buf(c2, &v1, sizeof(List_Prod_int_int));
    Prod_Prod_int_int_Prod_int_int v2;
    v2 = findMaxMin(v0);
    chan_send_buf(c2, &v2, sizeof(Prod_Prod_int_int_Prod_int_int));
}
void proc2Rt()
{
    List_Prod_int_int v3;
    chan_recv_buf(c2, &v3, sizeof(List_Prod_int_int));
    List_Prod_int_int v4;
    v4 = v3;
    Prod_Prod_int_int_Prod_int_int v5;
    chan_recv_buf(c2, &v5, sizeof(Prod_Prod_int_int_Prod_int_int));
    Prod_List_Prod_int_int_Prod_Prod_int_int_Prod_int_int v6;
    v6 = (Prod_List_Prod_int_int_Prod_Prod_int_int_Prod_int_int) {v4, v5};
    Prod_List_Prod_int_int_Prod_Prod_int_int_Prod_int_int v7;
    v7 = v6;
    chan_send_buf(c3,
                  &v7,
                  sizeof(Prod_List_Prod_int_int_Prod_Prod_int_int_Prod_int_int));
    List_Prod_int_int v8;
    v8 = v6.fst;
    chan_send_buf(c4, &v8, sizeof(List_Prod_int_int));
    Prod_Prod_int_int_Prod_int_int v9;
    v9 = swap(v6.snd);
    chan_send_buf(c4, &v9, sizeof(Prod_Prod_int_int_Prod_int_int));
}
void proc3Rt()
{
    List_Prod_int_int v10;
    chan_recv_buf(c4, &v10, sizeof(List_Prod_int_int));
    List_Prod_int_int v11;
    v11 = v10;
    Prod_Prod_int_int_Prod_int_int v12;
    chan_recv_buf(c4, &v12, sizeof(Prod_Prod_int_int_Prod_int_int));
    Prod_List_Prod_int_int_Prod_Prod_int_int_Prod_int_int v13;
    v13 = (Prod_List_Prod_int_int_Prod_Prod_int_int_Prod_int_int) {v11, v12};
    Prod_Prod_List_Prod_int_int_Prod_Prod_int_int_Prod_int_int_Prod_List_Prod_int_int_Prod_Prod_int_int_Prod_int_int v14;
    v14 = split(v13);
    Prod_List_Prod_int_int_Prod_Prod_int_int_Prod_int_int v15;
    v15 = v14.fst;
    chan_send_buf(c5,
                  &v15,
                  sizeof(Prod_List_Prod_int_int_Prod_Prod_int_int_Prod_int_int));
    List_Prod_int_int v16;
    v16 = baseFunc(v14.snd);
    chan_send_buf(c5, &v16, sizeof(List_Prod_int_int));
}
void proc4Rt()
{
    Prod_List_Prod_int_int_Prod_Prod_int_int_Prod_int_int v17;
    chan_recv_buf(c5,
                  &v17,
                  sizeof(Prod_List_Prod_int_int_Prod_Prod_int_int_Prod_int_int));
    List_Prod_int_int v18;
    v18 = baseFunc(v17);
    List_Prod_int_int v19;
    chan_recv_buf(c5, &v19, sizeof(List_Prod_int_int));
    Prod_List_Prod_int_int_List_Prod_int_int v20;
    v20 = (Prod_List_Prod_int_int_List_Prod_int_int) {v18, v19};
    List_Prod_int_int v21;
    v21 = merge(v20);
    chan_send_buf(c6, &v21, sizeof(List_Prod_int_int));
}
void proc5Rt()
{
    Prod_List_Prod_int_int_Prod_Prod_int_int_Prod_int_int v22;
    chan_recv_buf(c3,
                  &v22,
                  sizeof(Prod_List_Prod_int_int_Prod_Prod_int_int_Prod_int_int));
    Prod_Prod_List_Prod_int_int_Prod_Prod_int_int_Prod_int_int_Prod_List_Prod_int_int_Prod_Prod_int_int_Prod_int_int v23;
    v23 = split(v22);
    Prod_List_Prod_int_int_Prod_Prod_int_int_Prod_int_int v24;
    v24 = v23.fst;
    chan_send_buf(c7,
                  &v24,
                  sizeof(Prod_List_Prod_int_int_Prod_Prod_int_int_Prod_int_int));
    List_Prod_int_int v25;
    v25 = baseFunc(v23.snd);
    chan_send_buf(c7, &v25, sizeof(List_Prod_int_int));
}
void proc6Rt()
{
    Prod_List_Prod_int_int_Prod_Prod_int_int_Prod_int_int v26;
    chan_recv_buf(c7,
                  &v26,
                  sizeof(Prod_List_Prod_int_int_Prod_Prod_int_int_Prod_int_int));
    List_Prod_int_int v27;
    v27 = baseFunc(v26);
    List_Prod_int_int v28;
    chan_recv_buf(c7, &v28, sizeof(List_Prod_int_int));
    Prod_List_Prod_int_int_List_Prod_int_int v29;
    v29 = (Prod_List_Prod_int_int_List_Prod_int_int) {v27, v28};
    List_Prod_int_int v30;
    v30 = merge(v29);
    List_Prod_int_int v31;
    chan_recv_buf(c6, &v31, sizeof(List_Prod_int_int));
    Prod_List_Prod_int_int_List_Prod_int_int v32;
    v32 = (Prod_List_Prod_int_int_List_Prod_int_int) {v30, v31};
    List_Prod_int_int v33;
    v33 = merge(v32);
    chan_send_buf(c8, &v33, sizeof(List_Prod_int_int));
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
void * proc3()
{
    proc3Rt();
    return NULL;
}
void * proc4()
{
    proc4Rt();
    return NULL;
}
void * proc5()
{
    proc5Rt();
    return NULL;
}
void * proc6()
{
    proc6Rt();
    return NULL;
}
List_Prod_int_int proc0(List_Prod_int_int v0)
{
    c1 = chan_init(1);
    c2 = chan_init(1);
    c3 = chan_init(1);
    c4 = chan_init(1);
    c5 = chan_init(1);
    c6 = chan_init(1);
    c7 = chan_init(1);
    c8 = chan_init(1);
    pthread_t th1;
    pthread_create(&th1, NULL, proc1, NULL);
    pthread_t th2;
    pthread_create(&th2, NULL, proc2, NULL);
    pthread_t th3;
    pthread_create(&th3, NULL, proc3, NULL);
    pthread_t th4;
    pthread_create(&th4, NULL, proc4, NULL);
    pthread_t th5;
    pthread_create(&th5, NULL, proc5, NULL);
    pthread_t th6;
    pthread_create(&th6, NULL, proc6, NULL);
    chan_send_buf(c1, &v0, sizeof(List_Prod_int_int));
    List_Prod_int_int v1;
    chan_recv_buf(c8, &v1, sizeof(List_Prod_int_int));
    pthread_join(th1, NULL);
    pthread_join(th2, NULL);
    pthread_join(th3, NULL);
    pthread_join(th4, NULL);
    pthread_join(th5, NULL);
    pthread_join(th6, NULL);
    chan_dispose(c1);
    chan_dispose(c2);
    chan_dispose(c3);
    chan_dispose(c4);
    chan_dispose(c5);
    chan_dispose(c6);
    chan_dispose(c7);
    chan_dispose(c8);
    return v1;
}
int main()
{
    Prod_int_int tmp[] = { (Prod_int_int) {0, 0}, (Prod_int_int) {1, 1}, (Prod_int_int) {1, 0}, (Prod_int_int) {2, 2}, (Prod_int_int) {4, 4}, (Prod_int_int) {0, 3}, (Prod_int_int) {1, 2}, (Prod_int_int) {3, 1}, (Prod_int_int) {3, 3} };
    List_Prod_int_int a = (List_Prod_int_int) {9, tmp};
    double start = get_time();
    printList(proc0(a));
    double end = get_time();
    printf("%lf\n", end - start);
    return 0;
}
