#include<stdint.h>
#include<stdio.h>
#include<stdlib.h>
#include<chan.h>
#include<pthread.h>
#include"../data.h"
#include"../func.h"
chan_t * c1;
chan_t * c2;
void proc1Rt()
{
    List_Prod_float_float v0;
    chan_recv_buf(c1, &v0, sizeof(List_Prod_float_float));
    List_Prod_float_float v1;
    v1 = addPadding(v0);
    List_Prod_float_float v2;
    v2 = v1;
    List_Prod_float_float v3;
    v3 = baseFFT(v2);
    List_Prod_float_float v4;
    v4 = v3;
    chan_send_buf(c2, &v4, sizeof(List_Prod_float_float));
}
void * proc1()
{
    proc1Rt();
    return NULL;
}
List_Prod_float_float proc0(List_Prod_float_float v0)
{
    c1 = chan_init(1);
    c2 = chan_init(1);
    pthread_t th1;
    pthread_create(&th1, NULL, proc1, NULL);
    chan_send_buf(c1, &v0, sizeof(List_Prod_float_float));
    List_Prod_float_float v1;
    chan_recv_buf(c2, &v1, sizeof(List_Prod_float_float));
    pthread_join(th1, NULL);
    chan_dispose(c1);
    chan_dispose(c2);
    return v1;
}
int main()
{
    Prod_float_float tmp[] = { (Prod_float_float) {1.0, 0.0}, (Prod_float_float) {1.0, 0.0}, (Prod_float_float) {1.0, 0.0}, (Prod_float_float) {1.0, 0.0}, (Prod_float_float) {0.0, 0.0}, (Prod_float_float) {0.0, 0.0}, (Prod_float_float) {0.0, 0.0}, (Prod_float_float) {0.0, 0.0} };
    List_Prod_float_float a = (List_Prod_float_float) {8, tmp};
    double start = get_time();
    proc0(a);
    double end = get_time();
    printf("%lf\n", end - start);
    return 0;
}
