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
void proc1Rt()
{
    List_int v0;
    chan_recv_buf(c1, &v0, sizeof(List_int));
    Sum_List_int_Prod_List_int_List_int v1;
    v1 = split(v0);
    Label v2;
    v2 = v1.label;
    chan_send_int(c2, v2);
    chan_send_int(c3, v2);
    chan_send_int(c4, v2);
    chan_send_int(c5, v2);
    chan_send_int(c6, v2);
    chan_send_int(c7, v2);
    chan_send_int(c8, v2);
    List_int v3;
    Prod_List_int_List_int v4;
    int v5;
    if (v1.label == LEFT)
    {
        v3 = v1.value.left;
        Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int v6;
        v6 = (Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int) {LEFT, { .left = v3 }};
        chan_send_buf(c8,
                      &v6,
                      sizeof(Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int));
        v5 = 0;
    }
    else
    {
        v4 = v1.value.right;
        Prod_List_int_List_int v7;
        v7 = v4;
        chan_send_buf(c5, &v7, sizeof(Prod_List_int_List_int));
        Sum_List_int_Prod_List_int_List_int v8;
        v8 = split(v4.snd);
        Label v9;
        v9 = v8.label;
        chan_send_int(c2, v9);
        chan_send_int(c3, v9);
        chan_send_int(c4, v9);
        List_int v10;
        Prod_List_int_List_int v11;
        int v12;
        if (v8.label == LEFT)
        {
            v10 = v8.value.left;
            Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int v13;
            v13 = (Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int) {LEFT, { .left = v10 }};
            chan_send_buf(c4,
                          &v13,
                          sizeof(Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int));
            v12 = 0;
        }
        else
        {
            v11 = v8.value.right;
            Prod_List_int_List_int v14;
            v14 = v11;
            chan_send_buf(c3, &v14, sizeof(Prod_List_int_List_int));
            Sum_List_int_Prod_List_int_List_int v15;
            v15 = split(v11.snd);
            Label v16;
            v16 = v15.label;
            chan_send_int(c2, v16);
            List_int v17;
            Prod_List_int_List_int v18;
            int v19;
            if (v15.label == LEFT)
            {
                v17 = v15.value.left;
                Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int v20;
                v20 = (Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int) {LEFT, { .left = v17 }};
                chan_send_buf(c2,
                              &v20,
                              sizeof(Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int));
                v19 = 0;
            }
            else
            {
                v18 = v15.value.right;
                Prod_List_int_List_int v21;
                v21 = v18;
                chan_send_buf(c2, &v21, sizeof(Prod_List_int_List_int));
                List_Prod_int_int v22;
                v22 = count(v18.snd);
                chan_send_buf(c2, &v22, sizeof(List_Prod_int_int));
                v19 = 0;
            }
            v12 = 0;
        }
        v5 = 0;
    }
}
void proc2Rt()
{
    int v24;
    Label v23;
    chan_recv_int(c2, &v23);
    if (v23 == LEFT)
    {
        v24 = 0;
    }
    else
    {
        Label v25;
        chan_recv_int(c2, &v25);
        if (v25 == LEFT)
        {
        }
        else
        {
            Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int v27;
            Label v26;
            chan_recv_int(c2, &v26);
            if (v26 == LEFT)
            {
                Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int v28;
                chan_recv_buf(c2,
                              &v28,
                              sizeof(Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int));
                v27 = v28;
            }
            else
            {
                Prod_List_int_List_int v29;
                chan_recv_buf(c2, &v29, sizeof(Prod_List_int_List_int));
                List_Prod_int_int v30;
                v30 = count(v29.fst);
                List_Prod_int_int v31;
                chan_recv_buf(c2, &v31, sizeof(List_Prod_int_int));
                v27 = (Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int) {RIGHT, { .right = (Prod_List_Prod_int_int_List_Prod_int_int) {v30, v31} }};
            }
            Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int v32;
            v32 = v27;
            Label v33;
            v33 = v32.label;
            List_int v34;
            Prod_List_Prod_int_int_List_Prod_int_int v35;
            List_Prod_int_int v36;
            if (v32.label == LEFT)
            {
                v34 = v32.value.left;
                v36 = count(v34);
            }
            else
            {
                v35 = v32.value.right;
                v36 = myunion(v35);
            }
            List_Prod_int_int v37;
            v37 = v36;
            chan_send_buf(c9, &v37, sizeof(List_Prod_int_int));
        }
        v24 = 0;
    }
}
void proc3Rt()
{
    int v39;
    Label v38;
    chan_recv_int(c3, &v38);
    if (v38 == LEFT)
    {
        v39 = 0;
    }
    else
    {
        int v41;
        Label v40;
        chan_recv_int(c3, &v40);
        if (v40 == LEFT)
        {
            v41 = 0;
        }
        else
        {
            Prod_List_int_List_int v42;
            chan_recv_buf(c3, &v42, sizeof(Prod_List_int_List_int));
            Sum_List_int_Prod_List_int_List_int v43;
            v43 = split(v42.fst);
            Label v44;
            v44 = v43.label;
            chan_send_int(c10, v44);
            List_int v45;
            Prod_List_int_List_int v46;
            int v47;
            if (v43.label == LEFT)
            {
                v45 = v43.value.left;
                Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int v48;
                v48 = (Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int) {LEFT, { .left = v45 }};
                chan_send_buf(c10,
                              &v48,
                              sizeof(Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int));
                v47 = 0;
            }
            else
            {
                v46 = v43.value.right;
                Prod_List_int_List_int v49;
                v49 = v46;
                chan_send_buf(c10, &v49, sizeof(Prod_List_int_List_int));
                List_Prod_int_int v50;
                v50 = count(v46.snd);
                chan_send_buf(c10, &v50, sizeof(List_Prod_int_int));
                v47 = 0;
            }
            v41 = v47;
        }
        v39 = v41;
    }
}
void proc4Rt()
{
    Label v51;
    chan_recv_int(c4, &v51);
    if (v51 == LEFT)
    {
    }
    else
    {
        Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int v53;
        Label v52;
        chan_recv_int(c4, &v52);
        if (v52 == LEFT)
        {
            Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int v54;
            chan_recv_buf(c4,
                          &v54,
                          sizeof(Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int));
            v53 = v54;
        }
        else
        {
            Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int v56;
            Label v55;
            chan_recv_int(c10, &v55);
            if (v55 == LEFT)
            {
                Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int v57;
                chan_recv_buf(c10,
                              &v57,
                              sizeof(Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int));
                v56 = v57;
            }
            else
            {
                Prod_List_int_List_int v58;
                chan_recv_buf(c10, &v58, sizeof(Prod_List_int_List_int));
                List_Prod_int_int v59;
                v59 = count(v58.fst);
                List_Prod_int_int v60;
                chan_recv_buf(c10, &v60, sizeof(List_Prod_int_int));
                v56 = (Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int) {RIGHT, { .right = (Prod_List_Prod_int_int_List_Prod_int_int) {v59, v60} }};
            }
            Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int v61;
            v61 = v56;
            Label v62;
            v62 = v61.label;
            List_int v63;
            Prod_List_Prod_int_int_List_Prod_int_int v64;
            List_Prod_int_int v65;
            if (v61.label == LEFT)
            {
                v63 = v61.value.left;
                v65 = count(v63);
            }
            else
            {
                v64 = v61.value.right;
                v65 = myunion(v64);
            }
            List_Prod_int_int v66;
            v66 = v65;
            List_Prod_int_int v67;
            chan_recv_buf(c9, &v67, sizeof(List_Prod_int_int));
            v53 = (Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int) {RIGHT, { .right = (Prod_List_Prod_int_int_List_Prod_int_int) {v66, v67} }};
        }
        Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int v68;
        v68 = v53;
        Label v69;
        v69 = v68.label;
        List_int v70;
        Prod_List_Prod_int_int_List_Prod_int_int v71;
        List_Prod_int_int v72;
        if (v68.label == LEFT)
        {
            v70 = v68.value.left;
            v72 = count(v70);
        }
        else
        {
            v71 = v68.value.right;
            v72 = myunion(v71);
        }
        List_Prod_int_int v73;
        v73 = v72;
        chan_send_buf(c11, &v73, sizeof(List_Prod_int_int));
    }
}
void proc5Rt()
{
    int v75;
    Label v74;
    chan_recv_int(c5, &v74);
    if (v74 == LEFT)
    {
        v75 = 0;
    }
    else
    {
        Prod_List_int_List_int v76;
        chan_recv_buf(c5, &v76, sizeof(Prod_List_int_List_int));
        Sum_List_int_Prod_List_int_List_int v77;
        v77 = split(v76.fst);
        Label v78;
        v78 = v77.label;
        chan_send_int(c12, v78);
        chan_send_int(c13, v78);
        chan_send_int(c14, v78);
        List_int v79;
        Prod_List_int_List_int v80;
        int v81;
        if (v77.label == LEFT)
        {
            v79 = v77.value.left;
            Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int v82;
            v82 = (Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int) {LEFT, { .left = v79 }};
            chan_send_buf(c14,
                          &v82,
                          sizeof(Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int));
            v81 = 0;
        }
        else
        {
            v80 = v77.value.right;
            Prod_List_int_List_int v83;
            v83 = v80;
            chan_send_buf(c13, &v83, sizeof(Prod_List_int_List_int));
            Sum_List_int_Prod_List_int_List_int v84;
            v84 = split(v80.snd);
            Label v85;
            v85 = v84.label;
            chan_send_int(c12, v85);
            List_int v86;
            Prod_List_int_List_int v87;
            int v88;
            if (v84.label == LEFT)
            {
                v86 = v84.value.left;
                Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int v89;
                v89 = (Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int) {LEFT, { .left = v86 }};
                chan_send_buf(c12,
                              &v89,
                              sizeof(Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int));
                v88 = 0;
            }
            else
            {
                v87 = v84.value.right;
                Prod_List_int_List_int v90;
                v90 = v87;
                chan_send_buf(c12, &v90, sizeof(Prod_List_int_List_int));
                List_Prod_int_int v91;
                v91 = count(v87.snd);
                chan_send_buf(c12, &v91, sizeof(List_Prod_int_int));
                v88 = 0;
            }
            v81 = 0;
        }
        v75 = v81;
    }
}
void proc6Rt()
{
    int v93;
    Label v92;
    chan_recv_int(c6, &v92);
    if (v92 == LEFT)
    {
        v93 = 0;
    }
    else
    {
        Label v94;
        chan_recv_int(c12, &v94);
        if (v94 == LEFT)
        {
        }
        else
        {
            Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int v96;
            Label v95;
            chan_recv_int(c12, &v95);
            if (v95 == LEFT)
            {
                Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int v97;
                chan_recv_buf(c12,
                              &v97,
                              sizeof(Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int));
                v96 = v97;
            }
            else
            {
                Prod_List_int_List_int v98;
                chan_recv_buf(c12, &v98, sizeof(Prod_List_int_List_int));
                List_Prod_int_int v99;
                v99 = count(v98.fst);
                List_Prod_int_int v100;
                chan_recv_buf(c12, &v100, sizeof(List_Prod_int_int));
                v96 = (Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int) {RIGHT, { .right = (Prod_List_Prod_int_int_List_Prod_int_int) {v99, v100} }};
            }
            Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int v101;
            v101 = v96;
            Label v102;
            v102 = v101.label;
            List_int v103;
            Prod_List_Prod_int_int_List_Prod_int_int v104;
            List_Prod_int_int v105;
            if (v101.label == LEFT)
            {
                v103 = v101.value.left;
                v105 = count(v103);
            }
            else
            {
                v104 = v101.value.right;
                v105 = myunion(v104);
            }
            List_Prod_int_int v106;
            v106 = v105;
            chan_send_buf(c15, &v106, sizeof(List_Prod_int_int));
        }
        v93 = 0;
    }
}
void proc7Rt()
{
    int v108;
    Label v107;
    chan_recv_int(c7, &v107);
    if (v107 == LEFT)
    {
        v108 = 0;
    }
    else
    {
        int v110;
        Label v109;
        chan_recv_int(c13, &v109);
        if (v109 == LEFT)
        {
            v110 = 0;
        }
        else
        {
            Prod_List_int_List_int v111;
            chan_recv_buf(c13, &v111, sizeof(Prod_List_int_List_int));
            Sum_List_int_Prod_List_int_List_int v112;
            v112 = split(v111.fst);
            Label v113;
            v113 = v112.label;
            chan_send_int(c16, v113);
            List_int v114;
            Prod_List_int_List_int v115;
            int v116;
            if (v112.label == LEFT)
            {
                v114 = v112.value.left;
                Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int v117;
                v117 = (Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int) {LEFT, { .left = v114 }};
                chan_send_buf(c16,
                              &v117,
                              sizeof(Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int));
                v116 = 0;
            }
            else
            {
                v115 = v112.value.right;
                Prod_List_int_List_int v118;
                v118 = v115;
                chan_send_buf(c16, &v118, sizeof(Prod_List_int_List_int));
                List_Prod_int_int v119;
                v119 = count(v115.snd);
                chan_send_buf(c16, &v119, sizeof(List_Prod_int_int));
                v116 = 0;
            }
            v110 = v116;
        }
        v108 = v110;
    }
}
void proc8Rt()
{
    Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int v121;
    Label v120;
    chan_recv_int(c8, &v120);
    if (v120 == LEFT)
    {
        Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int v122;
        chan_recv_buf(c8,
                      &v122,
                      sizeof(Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int));
        v121 = v122;
    }
    else
    {
        Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int v124;
        Label v123;
        chan_recv_int(c14, &v123);
        if (v123 == LEFT)
        {
            Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int v125;
            chan_recv_buf(c14,
                          &v125,
                          sizeof(Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int));
            v124 = v125;
        }
        else
        {
            Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int v127;
            Label v126;
            chan_recv_int(c16, &v126);
            if (v126 == LEFT)
            {
                Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int v128;
                chan_recv_buf(c16,
                              &v128,
                              sizeof(Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int));
                v127 = v128;
            }
            else
            {
                Prod_List_int_List_int v129;
                chan_recv_buf(c16, &v129, sizeof(Prod_List_int_List_int));
                List_Prod_int_int v130;
                v130 = count(v129.fst);
                List_Prod_int_int v131;
                chan_recv_buf(c16, &v131, sizeof(List_Prod_int_int));
                v127 = (Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int) {RIGHT, { .right = (Prod_List_Prod_int_int_List_Prod_int_int) {v130, v131} }};
            }
            Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int v132;
            v132 = v127;
            Label v133;
            v133 = v132.label;
            List_int v134;
            Prod_List_Prod_int_int_List_Prod_int_int v135;
            List_Prod_int_int v136;
            if (v132.label == LEFT)
            {
                v134 = v132.value.left;
                v136 = count(v134);
            }
            else
            {
                v135 = v132.value.right;
                v136 = myunion(v135);
            }
            List_Prod_int_int v137;
            v137 = v136;
            List_Prod_int_int v138;
            chan_recv_buf(c15, &v138, sizeof(List_Prod_int_int));
            v124 = (Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int) {RIGHT, { .right = (Prod_List_Prod_int_int_List_Prod_int_int) {v137, v138} }};
        }
        Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int v139;
        v139 = v124;
        Label v140;
        v140 = v139.label;
        List_int v141;
        Prod_List_Prod_int_int_List_Prod_int_int v142;
        List_Prod_int_int v143;
        if (v139.label == LEFT)
        {
            v141 = v139.value.left;
            v143 = count(v141);
        }
        else
        {
            v142 = v139.value.right;
            v143 = myunion(v142);
        }
        List_Prod_int_int v144;
        v144 = v143;
        List_Prod_int_int v145;
        chan_recv_buf(c11, &v145, sizeof(List_Prod_int_int));
        v121 = (Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int) {RIGHT, { .right = (Prod_List_Prod_int_int_List_Prod_int_int) {v144, v145} }};
    }
    Sum_List_int_Prod_List_Prod_int_int_List_Prod_int_int v146;
    v146 = v121;
    Label v147;
    v147 = v146.label;
    List_int v148;
    Prod_List_Prod_int_int_List_Prod_int_int v149;
    List_Prod_int_int v150;
    if (v146.label == LEFT)
    {
        v148 = v146.value.left;
        v150 = count(v148);
    }
    else
    {
        v149 = v146.value.right;
        v150 = myunion(v149);
    }
    List_Prod_int_int v151;
    v151 = v150;
    chan_send_buf(c17, &v151, sizeof(List_Prod_int_int));
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
List_Prod_int_int proc0(List_int v0)
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
    c13 = chan_init(1);
    c14 = chan_init(1);
    c15 = chan_init(1);
    c16 = chan_init(1);
    c17 = chan_init(1);
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
    chan_send_buf(c1, &v0, sizeof(List_int));
    List_Prod_int_int v1;
    chan_recv_buf(c17, &v1, sizeof(List_Prod_int_int));
    pthread_join(th1, NULL);
    pthread_join(th2, NULL);
    pthread_join(th3, NULL);
    pthread_join(th4, NULL);
    pthread_join(th5, NULL);
    pthread_join(th6, NULL);
    pthread_join(th7, NULL);
    pthread_join(th8, NULL);
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
    chan_dispose(c13);
    chan_dispose(c14);
    chan_dispose(c15);
    chan_dispose(c16);
    chan_dispose(c17);
    return v1;
}
int main()
{
    int tmp[] = { 1, 2, 1, 2, 1, 2, 1, 2, 1, 1, 1, 1, 1, 1, 1, 5, 5, 5, 6, 6, 6, 6, 8, 9, 9, 9 };
    List_int a = (List_int) {26, tmp};
    double start = get_time();
    printListProdIntInt(proc0(a));
    double end = get_time();
    printf("%lf\n", end - start);
    return 0;
}
