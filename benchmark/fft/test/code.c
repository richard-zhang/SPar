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
chan_t * c13;
chan_t * c14;
chan_t * c15;
chan_t * c16;
chan_t * c17;
chan_t * c18;
chan_t * c19;
chan_t * c20;
chan_t * c21;
chan_t * c22;
chan_t * c23;
chan_t * c24;
chan_t * c25;
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
    Prod_List_Prod_float_float_List_Prod_float_float v4;
    v4 = splitList(v2.snd);
    List_Prod_float_float v5;
    v5 = v4.fst;
    chan_send_buf(c3, &v5, sizeof(List_Prod_float_float));
    List_Prod_float_float v6;
    v6 = v4.snd;
    chan_send_buf(c3, &v6, sizeof(List_Prod_float_float));
}
void proc2Rt()
{
    List_Prod_float_float v7;
    chan_recv_buf(c3, &v7, sizeof(List_Prod_float_float));
    List_Prod_float_float v8;
    v8 = v7;
    List_Prod_float_float v9;
    chan_recv_buf(c3, &v9, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v10;
    v10 = (Prod_List_Prod_float_float_List_Prod_float_float) {v8, v9};
    chan_send_buf(c4,
                  &v10,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc3Rt()
{
    List_Prod_float_float v11;
    chan_recv_buf(c2, &v11, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v12;
    v12 = splitList(v11);
    List_Prod_float_float v13;
    v13 = v12.fst;
    chan_send_buf(c5, &v13, sizeof(List_Prod_float_float));
    List_Prod_float_float v14;
    v14 = v12.snd;
    chan_send_buf(c5, &v14, sizeof(List_Prod_float_float));
}
void proc4Rt()
{
    List_Prod_float_float v15;
    chan_recv_buf(c5, &v15, sizeof(List_Prod_float_float));
    List_Prod_float_float v16;
    v16 = v15;
    List_Prod_float_float v17;
    chan_recv_buf(c5, &v17, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v18;
    v18 = (Prod_List_Prod_float_float_List_Prod_float_float) {v16, v17};
    Prod_List_Prod_float_float_List_Prod_float_float v19;
    chan_recv_buf(c4,
                  &v19,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float v20;
    v20 = (Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float) {v18, v19};
    Prod_List_Prod_float_float_List_Prod_float_float v21;
    v21 = v20.fst;
    chan_send_buf(c6,
                  &v21,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    List_Prod_float_float v22;
    v22 = v20.snd.fst;
    chan_send_buf(c7, &v22, sizeof(List_Prod_float_float));
    List_Prod_float_float v23;
    v23 = baseFFT(v20.snd.snd);
    chan_send_buf(c7, &v23, sizeof(List_Prod_float_float));
}
void proc5Rt()
{
    List_Prod_float_float v24;
    chan_recv_buf(c7, &v24, sizeof(List_Prod_float_float));
    List_Prod_float_float v25;
    v25 = baseFFT(v24);
    List_Prod_float_float v26;
    chan_recv_buf(c7, &v26, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v27;
    v27 = (Prod_List_Prod_float_float_List_Prod_float_float) {v25, v26};
    List_Prod_float_float v28;
    v28 = v27.fst;
    chan_send_buf(c8, &v28, sizeof(List_Prod_float_float));
    List_Prod_float_float v29;
    v29 = cmulexp(2)(0)(v27.snd);
    chan_send_buf(c8, &v29, sizeof(List_Prod_float_float));
}
void proc6Rt()
{
    List_Prod_float_float v30;
    chan_recv_buf(c8, &v30, sizeof(List_Prod_float_float));
    List_Prod_float_float v31;
    v31 = v30;
    List_Prod_float_float v32;
    chan_recv_buf(c8, &v32, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v33;
    v33 = (Prod_List_Prod_float_float_List_Prod_float_float) {v31, v32};
    Prod_List_Prod_float_float_List_Prod_float_float v34;
    v34 = v33;
    chan_send_buf(c9,
                  &v34,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    List_Prod_float_float v35;
    v35 = subc(v33);
    chan_send_buf(c9, &v35, sizeof(List_Prod_float_float));
}
void proc7Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v36;
    chan_recv_buf(c9,
                  &v36,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    List_Prod_float_float v37;
    v37 = addc(v36);
    List_Prod_float_float v38;
    chan_recv_buf(c9, &v38, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v39;
    v39 = (Prod_List_Prod_float_float_List_Prod_float_float) {v37, v38};
    chan_send_buf(c10,
                  &v39,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc8Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v40;
    chan_recv_buf(c6,
                  &v40,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    List_Prod_float_float v41;
    v41 = v40.fst;
    chan_send_buf(c11, &v41, sizeof(List_Prod_float_float));
    List_Prod_float_float v42;
    v42 = baseFFT(v40.snd);
    chan_send_buf(c11, &v42, sizeof(List_Prod_float_float));
}
void proc9Rt()
{
    List_Prod_float_float v43;
    chan_recv_buf(c11, &v43, sizeof(List_Prod_float_float));
    List_Prod_float_float v44;
    v44 = baseFFT(v43);
    List_Prod_float_float v45;
    chan_recv_buf(c11, &v45, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v46;
    v46 = (Prod_List_Prod_float_float_List_Prod_float_float) {v44, v45};
    List_Prod_float_float v47;
    v47 = v46.fst;
    chan_send_buf(c12, &v47, sizeof(List_Prod_float_float));
    List_Prod_float_float v48;
    v48 = cmulexp(2)(0)(v46.snd);
    chan_send_buf(c12, &v48, sizeof(List_Prod_float_float));
}
void proc10Rt()
{
    List_Prod_float_float v49;
    chan_recv_buf(c12, &v49, sizeof(List_Prod_float_float));
    List_Prod_float_float v50;
    v50 = v49;
    List_Prod_float_float v51;
    chan_recv_buf(c12, &v51, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v52;
    v52 = (Prod_List_Prod_float_float_List_Prod_float_float) {v50, v51};
    Prod_List_Prod_float_float_List_Prod_float_float v53;
    v53 = v52;
    chan_send_buf(c13,
                  &v53,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    List_Prod_float_float v54;
    v54 = subc(v52);
    chan_send_buf(c13, &v54, sizeof(List_Prod_float_float));
}
void proc11Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v55;
    chan_recv_buf(c13,
                  &v55,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    List_Prod_float_float v56;
    v56 = addc(v55);
    List_Prod_float_float v57;
    chan_recv_buf(c13, &v57, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v58;
    v58 = (Prod_List_Prod_float_float_List_Prod_float_float) {v56, v57};
    Prod_List_Prod_float_float_List_Prod_float_float v59;
    chan_recv_buf(c10,
                  &v59,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float v60;
    v60 = (Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float) {v58, v59};
    Prod_List_Prod_float_float_List_Prod_float_float v61;
    v61 = v60.fst;
    chan_send_buf(c14,
                  &v61,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    List_Prod_float_float v62;
    v62 = v60.snd.fst;
    chan_send_buf(c15, &v62, sizeof(List_Prod_float_float));
    List_Prod_float_float v63;
    v63 = cmulexp(4)(1)(v60.s