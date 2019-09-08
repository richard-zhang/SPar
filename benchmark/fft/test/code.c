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
chan_t * c26;
chan_t * c27;
chan_t * c28;
chan_t * c29;
chan_t * c30;
chan_t * c31;
chan_t * c32;
chan_t * c33;
chan_t * c34;
chan_t * c35;
chan_t * c36;
chan_t * c37;
chan_t * c38;
chan_t * c39;
chan_t * c40;
chan_t * c41;
chan_t * c42;
chan_t * c43;
chan_t * c44;
chan_t * c45;
chan_t * c46;
chan_t * c47;
chan_t * c48;
chan_t * c49;
chan_t * c50;
chan_t * c51;
chan_t * c52;
chan_t * c53;
chan_t * c54;
chan_t * c55;
chan_t * c56;
chan_t * c57;
chan_t * c58;
chan_t * c59;
chan_t * c60;
chan_t * c61;
chan_t * c62;
chan_t * c63;
chan_t * c64;
chan_t * c65;
chan_t * c66;
chan_t * c67;
chan_t * c68;
chan_t * c69;
chan_t * c70;
chan_t * c71;
chan_t * c72;
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
    Prod_List_Prod_float_float_List_Prod_float_float v6;
    v6 = splitList(v4.snd);
    List_Prod_float_float v7;
    v7 = v6.fst;
    chan_send_buf(c4, &v7, sizeof(List_Prod_float_float));
    List_Prod_float_float v8;
    v8 = v6.snd;
    chan_send_buf(c4, &v8, sizeof(List_Prod_float_float));
}
void proc2Rt()
{
    List_Prod_float_float v9;
    chan_recv_buf(c4, &v9, sizeof(List_Prod_float_float));
    List_Prod_float_float v10;
    v10 = v9;
    List_Prod_float_float v11;
    chan_recv_buf(c4, &v11, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v12;
    v12 = (Prod_List_Prod_float_float_List_Prod_float_float) {v10, v11};
    chan_send_buf(c5,
                  &v12,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc3Rt()
{
    List_Prod_float_float v13;
    chan_recv_buf(c3, &v13, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v14;
    v14 = splitList(v13);
    List_Prod_float_float v15;
    v15 = v14.fst;
    chan_send_buf(c6, &v15, sizeof(List_Prod_float_float));
    List_Prod_float_float v16;
    v16 = v14.snd;
    chan_send_buf(c6, &v16, sizeof(List_Prod_float_float));
}
void proc4Rt()
{
    List_Prod_float_float v17;
    chan_recv_buf(c6, &v17, sizeof(List_Prod_float_float));
    List_Prod_float_float v18;
    v18 = v17;
    List_Prod_float_float v19;
    chan_recv_buf(c6, &v19, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v20;
    v20 = (Prod_List_Prod_float_float_List_Prod_float_float) {v18, v19};
    Prod_List_Prod_float_float_List_Prod_float_float v21;
    chan_recv_buf(c5,
                  &v21,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float v22;
    v22 = (Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float) {v20, v21};
    chan_send_buf(c7,
                  &v22,
                  sizeof(Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc5Rt()
{
    List_Prod_float_float v23;
    chan_recv_buf(c2, &v23, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v24;
    v24 = splitList(v23);
    List_Prod_float_float v25;
    v25 = v24.fst;
    chan_send_buf(c8, &v25, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v26;
    v26 = splitList(v24.snd);
    List_Prod_float_float v27;
    v27 = v26.fst;
    chan_send_buf(c9, &v27, sizeof(List_Prod_float_float));
    List_Prod_float_float v28;
    v28 = v26.snd;
    chan_send_buf(c9, &v28, sizeof(List_Prod_float_float));
}
void proc6Rt()
{
    List_Prod_float_float v29;
    chan_recv_buf(c9, &v29, sizeof(List_Prod_float_float));
    List_Prod_float_float v30;
    v30 = v29;
    List_Prod_float_float v31;
    chan_recv_buf(c9, &v31, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v32;
    v32 = (Prod_List_Prod_float_float_List_Prod_float_float) {v30, v31};
    chan_send_buf(c10,
                  &v32,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc7Rt()
{
    List_Prod_float_float v33;
    chan_recv_buf(c8, &v33, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v34;
    v34 = splitList(v33);
    List_Prod_float_float v35;
    v35 = v34.fst;
    chan_send_buf(c11, &v35, sizeof(List_Prod_float_float));
    List_Prod_float_float v36;
    v36 = v34.snd;
    chan_send_buf(c11, &v36, sizeof(List_Prod_float_float));
}
void proc8Rt()
{
    List_Prod_float_float v37;
    chan_recv_buf(c11, &v37, sizeof(List_Prod_float_float));
    List_Prod_float_float v38;
    v38 = v37;
    List_Prod_float_float v39;
    chan_recv_buf(c11, &v39, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v40;
    v40 = (Prod_List_Prod_float_float_List_Prod_float_float) {v38, v39};
    Prod_List_Prod_float_float_List_Prod_float_float v41;
    chan_recv_buf(c10,
                  &v41,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float v42;
    v42 = (Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float) {v40, v41};
    Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float v43;
    chan_recv_buf(c7,
                  &v43,
                  sizeof(Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float_Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float v44;
    v44 = (Prod_Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float_Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float) {v42, v43};
    Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float v45;
    v45 = v44.fst;
    chan_send_buf(c12,
                  &v45,
                  sizeof(Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v46;
    v46 = v44.snd.fst;
    chan_send_buf(c13,
                  &v46,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    List_Prod_float_float v47;
    v47 = v44.snd.snd.fst;
    chan_send_buf(c14, &v47, sizeof(List_Prod_float_float));
    List_Prod_float_float v48;
    v48 = baseFFT(v44.snd.snd.snd);
    chan_send_buf(c14, &v48, sizeof(List_Prod_float_float));
}
void proc9Rt()
{
    List_Prod_float_float v49;
    chan_recv_buf(c14, &v49, sizeof(List_Prod_float_float));
    List_Prod_float_float v50;
    v50 = baseFFT(v49);
    List_Prod_float_float v51;
    chan_recv_buf(c14, &v51, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v52;
    v52 = (Prod_List_Prod_float_float_List_Prod_float_float) {v50, v51};
    List_Prod_float_float v53;
    v53 = v52.fst;
    chan_send_buf(c15, &v53, sizeof(List_Prod_float_float));
    List_Prod_float_float v54;
    v54 = cmulexp(2, 0, v52.snd);
    chan_send_buf(c15, &v54, sizeof(List_Prod_float_float));
}
void proc10Rt()
{
    List_Prod_float_float v55;
    chan_recv_buf(c15, &v55, sizeof(List_Prod_float_float));
    List_Prod_float_float v56;
    v56 = v55;
    List_Prod_float_float v57;
    chan_recv_buf(c15, &v57, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v58;
    v58 = (Prod_List_Prod_float_float_List_Prod_float_float) {v56, v57};
    Prod_List_Prod_float_float_List_Prod_float_float v59;
    v59 = v58;
    chan_send_buf(c16,
                  &v59,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc11Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v60;
    chan_recv_buf(c16,
                  &v60,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v61;
    v61 = addSub(v60);
    chan_send_buf(c17,
                  &v61,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc12Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v62;
    chan_recv_buf(c17,
                  &v62,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v63;
    v63 = v62;
    Prod_List_Prod_float_float_List_Prod_float_float v64;
    v64 = v63;
    chan_send_buf(c18,
                  &v64,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc13Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v65;
    chan_recv_buf(c13,
                  &v65,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    List_Prod_float_float v66;
    v66 = v65.fst;
    chan_send_buf(c19, &v66, sizeof(List_Prod_float_float));
    List_Prod_float_float v67;
    v67 = baseFFT(v65.snd);
    chan_send_buf(c19, &v67, sizeof(List_Prod_float_float));
}
void proc14Rt()
{
    List_Prod_float_float v68;
    chan_recv_buf(c19, &v68, sizeof(List_Prod_float_float));
    List_Prod_float_float v69;
    v69 = baseFFT(v68);
    List_Prod_float_float v70;
    chan_recv_buf(c19, &v70, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v71;
    v71 = (Prod_List_Prod_float_float_List_Prod_float_float) {v69, v70};
    List_Prod_float_float v72;
    v72 = v71.fst;
    chan_send_buf(c20, &v72, sizeof(List_Prod_float_float));
    List_Prod_float_float v73;
    v73 = cmulexp(2, 0, v71.snd);
    chan_send_buf(c20, &v73, sizeof(List_Prod_float_float));
}
void proc15Rt()
{
    List_Prod_float_float v74;
    chan_recv_buf(c20, &v74, sizeof(List_Prod_float_float));
    List_Prod_float_float v75;
    v75 = v74;
    List_Prod_float_float v76;
    chan_recv_buf(c20, &v76, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v77;
    v77 = (Prod_List_Prod_float_float_List_Prod_float_float) {v75, v76};
    Prod_List_Prod_float_float_List_Prod_float_float v78;
    v78 = v77;
    chan_send_buf(c21,
                  &v78,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc16Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v79;
    chan_recv_buf(c21,
                  &v79,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v80;
    v80 = addSub(v79);
    chan_send_buf(c22,
                  &v80,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc17Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v81;
    chan_recv_buf(c22,
                  &v81,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v82;
    v82 = v81;
    Prod_List_Prod_float_float_List_Prod_float_float v83;
    v83 = v82;
    Prod_List_Prod_float_float_List_Prod_float_float v84;
    chan_recv_buf(c18,
                  &v84,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float v85;
    v85 = (Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float) {v83, v84};
    Prod_List_Prod_float_float_List_Prod_float_float v86;
    v86 = v85.fst;
    chan_send_buf(c23,
                  &v86,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    List_Prod_float_float v87;
    v87 = v85.snd.fst;
    chan_send_buf(c24, &v87, sizeof(List_Prod_float_float));
    List_Prod_float_float v88;
    v88 = cmulexp(4, 1, v85.snd.snd);
    chan_send_buf(c24, &v88, sizeof(List_Prod_float_float));
}
void proc18Rt()
{
    List_Prod_float_float v89;
    chan_recv_buf(c24, &v89, sizeof(List_Prod_float_float));
    List_Prod_float_float v90;
    v90 = cmulexp(4, 0, v89);
    List_Prod_float_float v91;
    chan_recv_buf(c24, &v91, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v92;
    v92 = (Prod_List_Prod_float_float_List_Prod_float_float) {v90, v91};
    chan_send_buf(c25,
                  &v92,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc19Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v93;
    chan_recv_buf(c23,
                  &v93,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v94;
    v94 = v93;
    Prod_List_Prod_float_float_List_Prod_float_float v95;
    chan_recv_buf(c25,
                  &v95,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float v96;
    v96 = (Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float) {v94, v95};
    Prod_List_Prod_float_float_List_Prod_float_float v97;
    v97 = (Prod_List_Prod_float_float_List_Prod_float_float) {v96.fst.fst, v96.snd.fst};
    chan_send_buf(c26,
                  &v97,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v98;
    v98 = (Prod_List_Prod_float_float_List_Prod_float_float) {v96.fst.snd, v96.snd.snd};
    chan_send_buf(c27,
                  &v98,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc20Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v99;
    chan_recv_buf(c26,
                  &v99,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v100;
    v100 = addSub(v99);
    chan_send_buf(c28,
                  &v100,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc21Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v101;
    chan_recv_buf(c27,
                  &v101,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v102;
    v102 = addSub(v101);
    chan_send_buf(c29,
                  &v102,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc22Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v103;
    chan_recv_buf(c28,
                  &v103,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v104;
    chan_recv_buf(c29,
                  &v104,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float v105;
    v105 = (Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float) {v103, v104};
    Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float v106;
    v106 = (Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float) {(Prod_List_Prod_float_float_List_Prod_float_float) {v105.fst.fst, v105.snd.fst}, (Prod_List_Prod_float_float_List_Prod_float_float) {v105.fst.snd, v105.snd.snd}};
    chan_send_buf(c30,
                  &v106,
                  sizeof(Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc23Rt()
{
    Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float v107;
    chan_recv_buf(c12,
                  &v107,
                  sizeof(Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v108;
    v108 = v107.fst;
    chan_send_buf(c31,
                  &v108,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    List_Prod_float_float v109;
    v109 = v107.snd.fst;
    chan_send_buf(c32, &v109, sizeof(List_Prod_float_float));
    List_Prod_float_float v110;
    v110 = baseFFT(v107.snd.snd);
    chan_send_buf(c32, &v110, sizeof(List_Prod_float_float));
}
void proc24Rt()
{
    List_Prod_float_float v111;
    chan_recv_buf(c32, &v111, sizeof(List_Prod_float_float));
    List_Prod_float_float v112;
    v112 = baseFFT(v111);
    List_Prod_float_float v113;
    chan_recv_buf(c32, &v113, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v114;
    v114 = (Prod_List_Prod_float_float_List_Prod_float_float) {v112, v113};
    List_Prod_float_float v115;
    v115 = v114.fst;
    chan_send_buf(c33, &v115, sizeof(List_Prod_float_float));
    List_Prod_float_float v116;
    v116 = cmulexp(2, 0, v114.snd);
    chan_send_buf(c33, &v116, sizeof(List_Prod_float_float));
}
void proc25Rt()
{
    List_Prod_float_float v117;
    chan_recv_buf(c33, &v117, sizeof(List_Prod_float_float));
    List_Prod_float_float v118;
    v118 = v117;
    List_Prod_float_float v119;
    chan_recv_buf(c33, &v119, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v120;
    v120 = (Prod_List_Prod_float_float_List_Prod_float_float) {v118, v119};
    Prod_List_Prod_float_float_List_Prod_float_float v121;
    v121 = v120;
    chan_send_buf(c34,
                  &v121,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc26Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v122;
    chan_recv_buf(c34,
                  &v122,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v123;
    v123 = addSub(v122);
    chan_send_buf(c35,
                  &v123,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc27Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v124;
    chan_recv_buf(c35,
                  &v124,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v125;
    v125 = v124;
    Prod_List_Prod_float_float_List_Prod_float_float v126;
    v126 = v125;
    chan_send_buf(c36,
                  &v126,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc28Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v127;
    chan_recv_buf(c31,
                  &v127,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    List_Prod_float_float v128;
    v128 = v127.fst;
    chan_send_buf(c37, &v128, sizeof(List_Prod_float_float));
    List_Prod_float_float v129;
    v129 = baseFFT(v127.snd);
    chan_send_buf(c37, &v129, sizeof(List_Prod_float_float));
}
void proc29Rt()
{
    List_Prod_float_float v130;
    chan_recv_buf(c37, &v130, sizeof(List_Prod_float_float));
    List_Prod_float_float v131;
    v131 = baseFFT(v130);
    List_Prod_float_float v132;
    chan_recv_buf(c37, &v132, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v133;
    v133 = (Prod_List_Prod_float_float_List_Prod_float_float) {v131, v132};
    List_Prod_float_float v134;
    v134 = v133.fst;
    chan_send_buf(c38, &v134, sizeof(List_Prod_float_float));
    List_Prod_float_float v135;
    v135 = cmulexp(2, 0, v133.snd);
    chan_send_buf(c38, &v135, sizeof(List_Prod_float_float));
}
void proc30Rt()
{
    List_Prod_float_float v136;
    chan_recv_buf(c38, &v136, sizeof(List_Prod_float_float));
    List_Prod_float_float v137;
    v137 = v136;
    List_Prod_float_float v138;
    chan_recv_buf(c38, &v138, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v139;
    v139 = (Prod_List_Prod_float_float_List_Prod_float_float) {v137, v138};
    Prod_List_Prod_float_float_List_Prod_float_float v140;
    v140 = v139;
    chan_send_buf(c39,
                  &v140,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc31Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v141;
    chan_recv_buf(c39,
                  &v141,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v142;
    v142 = addSub(v141);
    chan_send_buf(c40,
                  &v142,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc32Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v143;
    chan_recv_buf(c40,
                  &v143,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v144;
    v144 = v143;
    Prod_List_Prod_float_float_List_Prod_float_float v145;
    v145 = v144;
    Prod_List_Prod_float_float_List_Prod_float_float v146;
    chan_recv_buf(c36,
                  &v146,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float v147;
    v147 = (Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float) {v145, v146};
    Prod_List_Prod_float_float_List_Prod_float_float v148;
    v148 = v147.fst;
    chan_send_buf(c41,
                  &v148,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    List_Prod_float_float v149;
    v149 = v147.snd.fst;
    chan_send_buf(c42, &v149, sizeof(List_Prod_float_float));
    List_Prod_float_float v150;
    v150 = cmulexp(4, 1, v147.snd.snd);
    chan_send_buf(c42, &v150, sizeof(List_Prod_float_float));
}
void proc33Rt()
{
    List_Prod_float_float v151;
    chan_recv_buf(c42, &v151, sizeof(List_Prod_float_float));
    List_Prod_float_float v152;
    v152 = cmulexp(4, 0, v151);
    List_Prod_float_float v153;
    chan_recv_buf(c42, &v153, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v154;
    v154 = (Prod_List_Prod_float_float_List_Prod_float_float) {v152, v153};
    chan_send_buf(c43,
                  &v154,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc34Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v155;
    chan_recv_buf(c41,
                  &v155,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v156;
    v156 = v155;
    Prod_List_Prod_float_float_List_Prod_float_float v157;
    chan_recv_buf(c43,
                  &v157,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float v158;
    v158 = (Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float) {v156, v157};
    Prod_List_Prod_float_float_List_Prod_float_float v159;
    v159 = (Prod_List_Prod_float_float_List_Prod_float_float) {v158.fst.fst, v158.snd.fst};
    chan_send_buf(c44,
                  &v159,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v160;
    v160 = (Prod_List_Prod_float_float_List_Prod_float_float) {v158.fst.snd, v158.snd.snd};
    chan_send_buf(c45,
                  &v160,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc35Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v161;
    chan_recv_buf(c44,
                  &v161,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v162;
    v162 = addSub(v161);
    chan_send_buf(c46,
                  &v162,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc36Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v163;
    chan_recv_buf(c45,
                  &v163,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v164;
    v164 = addSub(v163);
    chan_send_buf(c47,
                  &v164,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc37Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v165;
    chan_recv_buf(c46,
                  &v165,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v166;
    chan_recv_buf(c47,
                  &v166,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float v167;
    v167 = (Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float) {v165, v166};
    Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float v168;
    v168 = (Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float) {(Prod_List_Prod_float_float_List_Prod_float_float) {v167.fst.fst, v167.snd.fst}, (Prod_List_Prod_float_float_List_Prod_float_float) {v167.fst.snd, v167.snd.snd}};
    Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float v169;
    chan_recv_buf(c30,
                  &v169,
                  sizeof(Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float_Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float v170;
    v170 = (Prod_Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float_Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float) {v168, v169};
    Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float v171;
    v171 = v170.fst;
    chan_send_buf(c48,
                  &v171,
                  sizeof(Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v172;
    v172 = v170.snd.fst;
    chan_send_buf(c49,
                  &v172,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    List_Prod_float_float v173;
    v173 = v170.snd.snd.fst;
    chan_send_buf(c50, &v173, sizeof(List_Prod_float_float));
    List_Prod_float_float v174;
    v174 = cmulexp(8, 3, v170.snd.snd.snd);
    chan_send_buf(c50, &v174, sizeof(List_Prod_float_float));
}
void proc38Rt()
{
    List_Prod_float_float v175;
    chan_recv_buf(c50, &v175, sizeof(List_Prod_float_float));
    List_Prod_float_float v176;
    v176 = cmulexp(8, 2, v175);
    List_Prod_float_float v177;
    chan_recv_buf(c50, &v177, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v178;
    v178 = (Prod_List_Prod_float_float_List_Prod_float_float) {v176, v177};
    chan_send_buf(c51,
                  &v178,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc39Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v179;
    chan_recv_buf(c49,
                  &v179,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    List_Prod_float_float v180;
    v180 = v179.fst;
    chan_send_buf(c52, &v180, sizeof(List_Prod_float_float));
    List_Prod_float_float v181;
    v181 = cmulexp(8, 1, v179.snd);
    chan_send_buf(c52, &v181, sizeof(List_Prod_float_float));
}
void proc40Rt()
{
    List_Prod_float_float v182;
    chan_recv_buf(c52, &v182, sizeof(List_Prod_float_float));
    List_Prod_float_float v183;
    v183 = cmulexp(8, 0, v182);
    List_Prod_float_float v184;
    chan_recv_buf(c52, &v184, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v185;
    v185 = (Prod_List_Prod_float_float_List_Prod_float_float) {v183, v184};
    Prod_List_Prod_float_float_List_Prod_float_float v186;
    chan_recv_buf(c51,
                  &v186,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float v187;
    v187 = (Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float) {v185, v186};
    chan_send_buf(c53,
                  &v187,
                  sizeof(Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc41Rt()
{
    Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float v188;
    chan_recv_buf(c48,
                  &v188,
                  sizeof(Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float v189;
    v189 = v188;
    Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float v190;
    chan_recv_buf(c53,
                  &v190,
                  sizeof(Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float_Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float v191;
    v191 = (Prod_Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float_Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float) {v189, v190};
    Prod_List_Prod_float_float_List_Prod_float_float v192;
    v192 = (Prod_List_Prod_float_float_List_Prod_float_float) {v191.fst.fst.fst, v191.snd.fst.fst};
    chan_send_buf(c54,
                  &v192,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v193;
    v193 = (Prod_List_Prod_float_float_List_Prod_float_float) {v191.fst.fst.snd, v191.snd.fst.snd};
    chan_send_buf(c55,
                  &v193,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v194;
    v194 = (Prod_List_Prod_float_float_List_Prod_float_float) {v191.fst.snd.fst, v191.snd.snd.fst};
    chan_send_buf(c56,
                  &v194,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v195;
    v195 = (Prod_List_Prod_float_float_List_Prod_float_float) {v191.fst.snd.snd, v191.snd.snd.snd};
    chan_send_buf(c57,
                  &v195,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc42Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v196;
    chan_recv_buf(c54,
                  &v196,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v197;
    v197 = addSub(v196);
    chan_send_buf(c58,
                  &v197,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc43Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v198;
    chan_recv_buf(c55,
                  &v198,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v199;
    v199 = addSub(v198);
    chan_send_buf(c59,
                  &v199,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc44Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v200;
    chan_recv_buf(c56,
                  &v200,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v201;
    v201 = addSub(v200);
    chan_send_buf(c60,
                  &v201,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc45Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v202;
    chan_recv_buf(c57,
                  &v202,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v203;
    v203 = addSub(v202);
    chan_send_buf(c61,
                  &v203,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc46Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v204;
    chan_recv_buf(c58,
                  &v204,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v205;
    chan_recv_buf(c59,
                  &v205,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v206;
    chan_recv_buf(c60,
                  &v206,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v207;
    chan_recv_buf(c61,
                  &v207,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float_Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float v208;
    v208 = (Prod_Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float_Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float) {(Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float) {v204, v205}, (Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float) {v206, v207}};
    Prod_Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float_Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float v209;
    v209 = (Prod_Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float_Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float) {(Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float) {(Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float) {(Prod_List_Prod_float_float_List_Prod_float_float) {v208.fst.fst.fst, v208.fst.snd.fst}, (Prod_List_Prod_float_float_List_Prod_float_float) {v208.fst.fst.snd, v208.fst.snd.snd}}.fst, (Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float) {(Prod_List_Prod_float_float_List_Prod_float_float) {v208.snd.fst.fst, v208.snd.snd.fst}, (Prod_List_Prod_float_float_List_Prod_float_float) {v208.snd.fst.snd, v208.snd.snd.snd}}.fst}, (Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float) {(Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float) {(Prod_List_Prod_float_float_List_Prod_float_float) {v208.fst.fst.fst, v208.fst.snd.fst}, (Prod_List_Prod_float_float_List_Prod_float_float) {v208.fst.fst.snd, v208.fst.snd.snd}}.snd, (Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float) {(Prod_List_Prod_float_float_List_Prod_float_float) {v208.snd.fst.fst, v208.snd.snd.fst}, (Prod_List_Prod_float_float_List_Prod_float_float) {v208.snd.fst.snd, v208.snd.snd.snd}}.snd}};
    Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float v210;
    v210 = v209.fst;
    chan_send_buf(c62,
                  &v210,
                  sizeof(Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v211;
    v211 = v209.snd.fst;
    chan_send_buf(c63,
                  &v211,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    List_Prod_float_float v212;
    v212 = v209.snd.snd.fst;
    chan_send_buf(c64, &v212, sizeof(List_Prod_float_float));
    List_Prod_float_float v213;
    v213 = v209.snd.snd.snd;
    chan_send_buf(c64, &v213, sizeof(List_Prod_float_float));
}
void proc47Rt()
{
    List_Prod_float_float v214;
    chan_recv_buf(c64, &v214, sizeof(List_Prod_float_float));
    List_Prod_float_float v215;
    v215 = v214;
    List_Prod_float_float v216;
    chan_recv_buf(c64, &v216, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v217;
    v217 = (Prod_List_Prod_float_float_List_Prod_float_float) {v215, v216};
    List_Prod_float_float v218;
    v218 = concatenate(v217);
    chan_send_buf(c65, &v218, sizeof(List_Prod_float_float));
}
void proc48Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v219;
    chan_recv_buf(c63,
                  &v219,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    List_Prod_float_float v220;
    v220 = v219.fst;
    chan_send_buf(c66, &v220, sizeof(List_Prod_float_float));
    List_Prod_float_float v221;
    v221 = v219.snd;
    chan_send_buf(c66, &v221, sizeof(List_Prod_float_float));
}
void proc49Rt()
{
    List_Prod_float_float v222;
    chan_recv_buf(c66, &v222, sizeof(List_Prod_float_float));
    List_Prod_float_float v223;
    v223 = v222;
    List_Prod_float_float v224;
    chan_recv_buf(c66, &v224, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v225;
    v225 = (Prod_List_Prod_float_float_List_Prod_float_float) {v223, v224};
    List_Prod_float_float v226;
    v226 = concatenate(v225);
    List_Prod_float_float v227;
    chan_recv_buf(c65, &v227, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v228;
    v228 = (Prod_List_Prod_float_float_List_Prod_float_float) {v226, v227};
    List_Prod_float_float v229;
    v229 = concatenate(v228);
    chan_send_buf(c67, &v229, sizeof(List_Prod_float_float));
}
void proc50Rt()
{
    Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float v230;
    chan_recv_buf(c62,
                  &v230,
                  sizeof(Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v231;
    v231 = v230.fst;
    chan_send_buf(c68,
                  &v231,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    List_Prod_float_float v232;
    v232 = v230.snd.fst;
    chan_send_buf(c69, &v232, sizeof(List_Prod_float_float));
    List_Prod_float_float v233;
    v233 = v230.snd.snd;
    chan_send_buf(c69, &v233, sizeof(List_Prod_float_float));
}
void proc51Rt()
{
    List_Prod_float_float v234;
    chan_recv_buf(c69, &v234, sizeof(List_Prod_float_float));
    List_Prod_float_float v235;
    v235 = v234;
    List_Prod_float_float v236;
    chan_recv_buf(c69, &v236, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v237;
    v237 = (Prod_List_Prod_float_float_List_Prod_float_float) {v235, v236};
    List_Prod_float_float v238;
    v238 = concatenate(v237);
    chan_send_buf(c70, &v238, sizeof(List_Prod_float_float));
}
void proc52Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v239;
    chan_recv_buf(c68,
                  &v239,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    List_Prod_float_float v240;
    v240 = v239.fst;
    chan_send_buf(c71, &v240, sizeof(List_Prod_float_float));
    List_Prod_float_float v241;
    v241 = v239.snd;
    chan_send_buf(c71, &v241, sizeof(List_Prod_float_float));
}
void proc53Rt()
{
    List_Prod_float_float v242;
    chan_recv_buf(c71, &v242, sizeof(List_Prod_float_float));
    List_Prod_float_float v243;
    v243 = v242;
    List_Prod_float_float v244;
    chan_recv_buf(c71, &v244, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v245;
    v245 = (Prod_List_Prod_float_float_List_Prod_float_float) {v243, v244};
    List_Prod_float_float v246;
    v246 = concatenate(v245);
    List_Prod_float_float v247;
    chan_recv_buf(c70, &v247, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v248;
    v248 = (Prod_List_Prod_float_float_List_Prod_float_float) {v246, v247};
    List_Prod_float_float v249;
    v249 = concatenate(v248);
    List_Prod_float_float v250;
    chan_recv_buf(c67, &v250, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v251;
    v251 = (Prod_List_Prod_float_float_List_Prod_float_float) {v249, v250};
    List_Prod_float_float v252;
    v252 = concatenate(v251);
    chan_send_buf(c72, &v252, sizeof(List_Prod_float_float));
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
void * proc11()
{
    proc11Rt();
    return NULL;
}
void * proc12()
{
    proc12Rt();
    return NULL;
}
void * proc13()
{
    proc13Rt();
    return NULL;
}
void * proc14()
{
    proc14Rt();
    return NULL;
}
void * proc15()
{
    proc15Rt();
    return NULL;
}
void * proc16()
{
    proc16Rt();
    return NULL;
}
void * proc17()
{
    proc17Rt();
    return NULL;
}
void * proc18()
{
    proc18Rt();
    return NULL;
}
void * proc19()
{
    proc19Rt();
    return NULL;
}
void * proc20()
{
    proc20Rt();
    return NULL;
}
void * proc21()
{
    proc21Rt();
    return NULL;
}
void * proc22()
{
    proc22Rt();
    return NULL;
}
void * proc23()
{
    proc23Rt();
    return NULL;
}
void * proc24()
{
    proc24Rt();
    return NULL;
}
void * proc25()
{
    proc25Rt();
    return NULL;
}
void * proc26()
{
    proc26Rt();
    return NULL;
}
void * proc27()
{
    proc27Rt();
    return NULL;
}
void * proc28()
{
    proc28Rt();
    return NULL;
}
void * proc29()
{
    proc29Rt();
    return NULL;
}
void * proc30()
{
    proc30Rt();
    return NULL;
}
void * proc31()
{
    proc31Rt();
    return NULL;
}
void * proc32()
{
    proc32Rt();
    return NULL;
}
void * proc33()
{
    proc33Rt();
    return NULL;
}
void * proc34()
{
    proc34Rt();
    return NULL;
}
void * proc35()
{
    proc35Rt();
    return NULL;
}
void * proc36()
{
    proc36Rt();
    return NULL;
}
void * proc37()
{
    proc37Rt();
    return NULL;
}
void * proc38()
{
    proc38Rt();
    return NULL;
}
void * proc39()
{
    proc39Rt();
    return NULL;
}
void * proc40()
{
    proc40Rt();
    return NULL;
}
void * proc41()
{
    proc41Rt();
    return NULL;
}
void * proc42()
{
    proc42Rt();
    return NULL;
}
void * proc43()
{
    proc43Rt();
    return NULL;
}
void * proc44()
{
    proc44Rt();
    return NULL;
}
void * proc45()
{
    proc45Rt();
    return NULL;
}
void * proc46()
{
    proc46Rt();
    return NULL;
}
void * proc47()
{
    proc47Rt();
    return NULL;
}
void * proc48()
{
    proc48Rt();
    return NULL;
}
void * proc49()
{
    proc49Rt();
    return NULL;
}
void * proc50()
{
    proc50Rt();
    return NULL;
}
void * proc51()
{
    proc51Rt();
    return NULL;
}
void * proc52()
{
    proc52Rt();
    return NULL;
}
void * proc53()
{
    proc53Rt();
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
    c13 = chan_init(1);
    c14 = chan_init(1);
    c15 = chan_init(1);
    c16 = chan_init(1);
    c17 = chan_init(1);
    c18 = chan_init(1);
    c19 = chan_init(1);
    c20 = chan_init(1);
    c21 = chan_init(1);
    c22 = chan_init(1);
    c23 = chan_init(1);
    c24 = chan_init(1);
    c25 = chan_init(1);
    c26 = chan_init(1);
    c27 = chan_init(1);
    c28 = chan_init(1);
    c29 = chan_init(1);
    c30 = chan_init(1);
    c31 = chan_init(1);
    c32 = chan_init(1);
    c33 = chan_init(1);
    c34 = chan_init(1);
    c35 = chan_init(1);
    c36 = chan_init(1);
    c37 = chan_init(1);
    c38 = chan_init(1);
    c39 = chan_init(1);
    c40 = chan_init(1);
    c41 = chan_init(1);
    c42 = chan_init(1);
    c43 = chan_init(1);
    c44 = chan_init(1);
    c45 = chan_init(1);
    c46 = chan_init(1);
    c47 = chan_init(1);
    c48 = chan_init(1);
    c49 = chan_init(1);
    c50 = chan_init(1);
    c51 = chan_init(1);
    c52 = chan_init(1);
    c53 = chan_init(1);
    c54 = chan_init(1);
    c55 = chan_init(1);
    c56 = chan_init(1);
    c57 = chan_init(1);
    c58 = chan_init(1);
    c59 = chan_init(1);
    c60 = chan_init(1);
    c61 = chan_init(1);
    c62 = chan_init(1);
    c63 = chan_init(1);
    c64 = chan_init(1);
    c65 = chan_init(1);
    c66 = chan_init(1);
    c67 = chan_init(1);
    c68 = chan_init(1);
    c69 = chan_init(1);
    c70 = chan_init(1);
    c71 = chan_init(1);
    c72 = chan_init(1);
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
    pthread_t th11;
    pthread_create(&th11, NULL, proc11, NULL);
    pthread_t th12;
    pthread_create(&th12, NULL, proc12, NULL);
    pthread_t th13;
    pthread_create(&th13, NULL, proc13, NULL);
    pthread_t th14;
    pthread_create(&th14, NULL, proc14, NULL);
    pthread_t th15;
    pthread_create(&th15, NULL, proc15, NULL);
    pthread_t th16;
    pthread_create(&th16, NULL, proc16, NULL);
    pthread_t th17;
    pthread_create(&th17, NULL, proc17, NULL);
    pthread_t th18;
    pthread_create(&th18, NULL, proc18, NULL);
    pthread_t th19;
    pthread_create(&th19, NULL, proc19, NULL);
    pthread_t th20;
    pthread_create(&th20, NULL, proc20, NULL);
    pthread_t th21;
    pthread_create(&th21, NULL, proc21, NULL);
    pthread_t th22;
    pthread_create(&th22, NULL, proc22, NULL);
    pthread_t th23;
    pthread_create(&th23, NULL, proc23, NULL);
    pthread_t th24;
    pthread_create(&th24, NULL, proc24, NULL);
    pthread_t th25;
    pthread_create(&th25, NULL, proc25, NULL);
    pthread_t th26;
    pthread_create(&th26, NULL, proc26, NULL);
    pthread_t th27;
    pthread_create(&th27, NULL, proc27, NULL);
    pthread_t th28;
    pthread_create(&th28, NULL, proc28, NULL);
    pthread_t th29;
    pthread_create(&th29, NULL, proc29, NULL);
    pthread_t th30;
    pthread_create(&th30, NULL, proc30, NULL);
    pthread_t th31;
    pthread_create(&th31, NULL, proc31, NULL);
    pthread_t th32;
    pthread_create(&th32, NULL, proc32, NULL);
    pthread_t th33;
    pthread_create(&th33, NULL, proc33, NULL);
    pthread_t th34;
    pthread_create(&th34, NULL, proc34, NULL);
    pthread_t th35;
    pthread_create(&th35, NULL, proc35, NULL);
    pthread_t th36;
    pthread_create(&th36, NULL, proc36, NULL);
    pthread_t th37;
    pthread_create(&th37, NULL, proc37, NULL);
    pthread_t th38;
    pthread_create(&th38, NULL, proc38, NULL);
    pthread_t th39;
    pthread_create(&th39, NULL, proc39, NULL);
    pthread_t th40;
    pthread_create(&th40, NULL, proc40, NULL);
    pthread_t th41;
    pthread_create(&th41, NULL, proc41, NULL);
    pthread_t th42;
    pthread_create(&th42, NULL, proc42, NULL);
    pthread_t th43;
    pthread_create(&th43, NULL, proc43, NULL);
    pthread_t th44;
    pthread_create(&th44, NULL, proc44, NULL);
    pthread_t th45;
    pthread_create(&th45, NULL, proc45, NULL);
    pthread_t th46;
    pthread_create(&th46, NULL, proc46, NULL);
    pthread_t th47;
    pthread_create(&th47, NULL, proc47, NULL);
    pthread_t th48;
    pthread_create(&th48, NULL, proc48, NULL);
    pthread_t th49;
    pthread_create(&th49, NULL, proc49, NULL);
    pthread_t th50;
    pthread_create(&th50, NULL, proc50, NULL);
    pthread_t th51;
    pthread_create(&th51, NULL, proc51, NULL);
    pthread_t th52;
    pthread_create(&th52, NULL, proc52, NULL);
    pthread_t th53;
    pthread_create(&th53, NULL, proc53, NULL);
    chan_send_buf(c1, &v0, sizeof(List_Prod_float_float));
    List_Prod_float_float v1;
    chan_recv_buf(c72, &v1, sizeof(List_Prod_float_float));
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
    pthread_join(th11, NULL);
    pthread_join(th12, NULL);
    pthread_join(th13, NULL);
    pthread_join(th14, NULL);
    pthread_join(th15, NULL);
    pthread_join(th16, NULL);
    pthread_join(th17, NULL);
    pthread_join(th18, NULL);
    pthread_join(th19, NULL);
    pthread_join(th20, NULL);
    pthread_join(th21, NULL);
    pthread_join(th22, NULL);
    pthread_join(th23, NULL);
    pthread_join(th24, NULL);
    pthread_join(th25, NULL);
    pthread_join(th26, NULL);
    pthread_join(th27, NULL);
    pthread_join(th28, NULL);
    pthread_join(th29, NULL);
    pthread_join(th30, NULL);
    pthread_join(th31, NULL);
    pthread_join(th32, NULL);
    pthread_join(th33, NULL);
    pthread_join(th34, NULL);
    pthread_join(th35, NULL);
    pthread_join(th36, NULL);
    pthread_join(th37, NULL);
    pthread_join(th38, NULL);
    pthread_join(th39, NULL);
    pthread_join(th40, NULL);
    pthread_join(th41, NULL);
    pthread_join(th42, NULL);
    pthread_join(th43, NULL);
    pthread_join(th44, NULL);
    pthread_join(th45, NULL);
    pthread_join(th46, NULL);
    pthread_join(th47, NULL);
    pthread_join(th48, NULL);
    pthread_join(th49, NULL);
    pthread_join(th50, NULL);
    pthread_join(th51, NULL);
    pthread_join(th52, NULL);
    pthread_join(th53, NULL);
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
    chan_dispose(c18);
    chan_dispose(c19);
    chan_dispose(c20);
    chan_dispose(c21);
    chan_dispose(c22);
    chan_dispose(c23);
    chan_dispose(c24);
    chan_dispose(c25);
    chan_dispose(c26);
    chan_dispose(c27);
    chan_dispose(c28);
    chan_dispose(c29);
    chan_dispose(c30);
    chan_dispose(c31);
    chan_dispose(c32);
    chan_dispose(c33);
    chan_dispose(c34);
    chan_dispose(c35);
    chan_dispose(c36);
    chan_dispose(c37);
    chan_dispose(c38);
    chan_dispose(c39);
    chan_dispose(c40);
    chan_dispose(c41);
    chan_dispose(c42);
    chan_dispose(c43);
    chan_dispose(c44);
    chan_dispose(c45);
    chan_dispose(c46);
    chan_dispose(c47);
    chan_dispose(c48);
    chan_dispose(c49);
    chan_dispose(c50);
    chan_dispose(c51);
    chan_dispose(c52);
    chan_dispose(c53);
    chan_dispose(c54);
    chan_dispose(c55);
    chan_dispose(c56);
    chan_dispose(c57);
    chan_dispose(c58);
    chan_dispose(c59);
    chan_dispose(c60);
    chan_dispose(c61);
    chan_dispose(c62);
    chan_dispose(c63);
    chan_dispose(c64);
    chan_dispose(c65);
    chan_dispose(c66);
    chan_dispose(c67);
    chan_dispose(c68);
    chan_dispose(c69);
    chan_dispose(c70);
    chan_dispose(c71);
    chan_dispose(c72);
    return v1;
}
int main()
{
    Prod_float_float tmp[] = { (Prod_float_float) {1.0, 0.0}, (Prod_float_float) {1.0, 0.0}, (Prod_float_float) {1.0, 0.0}, (Prod_float_float) {1.0, 0.0}, (Prod_float_float) {0.0, 0.0}, (Prod_float_float) {0.0, 0.0}, (Prod_float_float) {0.0, 0.0}, (Prod_float_float) {0.0, 0.0} };
    List_Prod_float_float a = (List_Prod_float_float) {8, tmp};
    double start = get_time();
    debug(proc0(a));
    double end = get_time();
    printf("%lf\n", end - start);
    return 0;
}
