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
chan_t * c9;
chan_t * c10;
chan_t * c11;
chan_t * c12;
void proc1Rt()
{
    List_Prod_float_float v0;
    chan_recv_buf(c1, &v0, sizeof(List_Prod_float_float));
    List_Prod_float_float v1;
    v1 = addPadding(v0);
    Prod_List_Prod_float_float_List_Prod_float_float v2;
    v2 = splitList(v1);
    List_Prod_float_float v3;
    v3 = v2.fst;
    chan_send_buf(c2, &v3, sizeof(List_Prod_float_float));
    List_Prod_float_float v4;
    v4 = v2.snd;
    chan_send_buf(c2, &v4, sizeof(List_Prod_float_float));
}
void proc2Rt()
{
    List_Prod_float_float v5;
    chan_recv_buf(c2, &v5, sizeof(List_Prod_float_float));
    List_Prod_float_float v6;
    v6 = v5;
    List_Prod_float_float v7;
    chan_recv_buf(c2, &v7, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v8;
    v8 = (Prod_List_Prod_float_float_List_Prod_float_float) {v6, v7};
    List_Prod_float_float v9;
    v9 = v8.fst;
    chan_send_buf(c3, &v9, sizeof(List_Prod_float_float));
    List_Prod_float_float v10;
    v10 = baseFFT(v8.snd);
    chan_send_buf(c3, &v10, sizeof(List_Prod_float_float));
}
void proc3Rt()
{
    List_Prod_float_float v11;
    chan_recv_buf(c3, &v11, sizeof(List_Prod_float_float));
    List_Prod_float_float v12;
    v12 = baseFFT(v11);
    List_Prod_float_float v13;
    chan_recv_buf(c3, &v13, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v14;
    v14 = (Prod_List_Prod_float_float_List_Prod_float_float) {v12, v13};
    List_Prod_float_float v15;
    v15 = v14.fst;
    chan_send_buf(c4, &v15, sizeof(List_Prod_float_float));
    List_Prod_float_float v16;
    v16 = cmulexp(2, 0, v14.snd);
    chan_send_buf(c4, &v16, sizeof(List_Prod_float_float));
}
void proc4Rt()
{
    List_Prod_float_float v17;
    chan_recv_buf(c4, &v17, sizeof(List_Prod_float_float));
    List_Prod_float_float v18;
    v18 = v17;
    List_Prod_float_float v19;
    chan_recv_buf(c4, &v19, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v20;
    v20 = (Prod_List_Prod_float_float_List_Prod_float_float) {v18, v19};
    Prod_List_Prod_float_float_List_Prod_float_float v21;
    v21 = v20;
    chan_send_buf(c5,
                  &v21,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v22;
    v22 = v20;
    chan_send_buf(c6,
                  &v22,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc5Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v23;
    chan_recv_buf(c6,
                  &v23,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    List_Prod_float_float v24;
    v24 = subc(v23);
    chan_send_buf(c7, &v24, sizeof(List_Prod_float_float));
}
void proc6Rt()
{
    List_Prod_float_float v25;
    chan_recv_buf(c7, &v25, sizeof(List_Prod_float_float));
    List_Prod_float_float v26;
    v26 = v25;
    chan_send_buf(c8, &v26, sizeof(List_Prod_float_float));
}
void proc7Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v27;
    chan_recv_buf(c5,
                  &v27,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v28;
    v28 = v27;
    chan_send_buf(c9,
                  &v28,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc8Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v29;
    chan_recv_buf(c9,
                  &v29,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    List_Prod_float_float v30;
    v30 = addc(v29);
    chan_send_buf(c10, &v30, sizeof(List_Prod_float_float));
}
void proc9Rt()
{
    List_Prod_float_float v31;
    chan_recv_buf(c10, &v31, sizeof(List_Prod_float_float));
    List_Prod_float_float v32;
    v32 = v31;
    List_Prod_float_float v33;
    chan_recv_buf(c8, &v33, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v34;
    v34 = (Prod_List_Prod_float_float_List_Prod_float_float) {v32, v33};
    List_Prod_float_float v35;
    v35 = v34.fst;
    chan_send_buf(c11, &v35, sizeof(List_Prod_float_float));
    List_Prod_float_float v36;
    v36 = v34.snd;
    chan_send_buf(c11, &v36, sizeof(List_Prod_float_float));
}
void proc10Rt()
{
    List_Prod_float_float v37;
    chan_recv_buf(c11, &v37, sizeof(List_Prod_float_float));
    List_Prod_float_float v38;
    v38 = v37;
    List_Prod_float_float v39;
    chan_recv_buf(c11, &v39, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v40;
    v40 = (Prod_List_Prod_float_float_List_Prod_float_float) {v38, v39};
    List_Prod_float_float v41;
    v41 = concatenate(v40);
    chan_send_buf(c12, &v41, sizeof(List_Prod_float_float));
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
void * proc7()
{
    proc7Rt();
    return NULL;
}
void * proc8()
{
    proc8Rt();
    return NULL;
}
void * proc9()
{
    proc9Rt();
    return NULL;
}
void * proc10()
{
    proc10Rt();
    return NULL;
}
List_Prod_float_float proc0(List_Prod_float_float v0)
{
    c1 = chan_init(1);
    c2 = chan_init(1);
    c3 = chan_init(1);
    c4 = chan_init(1);
    c5 = chan_init(1);
    c6 = chan_init(1);
    c7 = chan_init(1);
    c8 = chan_init(1);
    c9 = chan_init(1);
    c10 = chan_init(1);
    c11 = chan_init(1);
    c12 = chan_init(1);
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
    pthread_t th7;
    pthread_create(&th7, NULL, proc7, NULL);
    pthread_t th8;
    pthread_create(&th8, NULL, proc8, NULL);
    pthread_t th9;
    pthread_create(&th9, NULL, proc9, NULL);
    pthread_t th10;
    pthread_create(&th10, NULL, proc10, NULL);
    chan_send_buf(c1, &v0, sizeof(List_Prod_float_float));
    List_Prod_float_float v1;
    chan_recv_buf(c12, &v1, sizeof(List_Prod_float_float));
    pthread_join(th1, NULL);
    pthread_join(th2, NULL);
    pthread_join(th3, NULL);
    pthread_join(th4, NULL);
    pthread_join(th5, NULL);
    pthread_join(th6, NULL);
    pthread_join(th7, NULL);
    pthread_join(th8, NULL);
    pthread_join(th9, NULL);
    pthread_join(th10, NULL);
    chan_dispose(c1);
    chan_dispose(c2);
    chan_dispose(c3);
    chan_dispose(c4);
    chan_dispose(c5);
    chan_dispose(c6);
    chan_dispose(c7);
    chan_dispose(c8);
    chan_dispose(c9);
    chan_dispose(c10);
    chan_dispose(c11);
    chan_dispose(c12);
    return v1;
}
int main()
{
    Prod_float_float tmp[] = { (Prod_float_float) {1.0, 1.0}, (Prod_float_float) {1.0, 1.0}, (Prod_float_float) {1.0, 1.0}, (Prod_float_float) {1.0, 1.0} };
    List_Prod_float_float a = (List_Prod_float_float) {4, tmp};
    double start = get_time();
    debug(proc0(a));
    double end = get_time();
    printf("%lf\n", end - start);
    return 0;
}
