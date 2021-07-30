annpca = DiagrammeR::grViz("digraph G {
    rankdir = LR;
    splines=false;
    edge[style=invis];
    ranksep= 1.4;
    {
    node [shape=circle, color=lightblue, style=filled, fillcolor=lightblue];
    a02 [label=<a<sub>0</sub><sup>(1)</sup>>]; 
    a03 [label=<a<sub>0</sub><sup>(2)</sup>>];
    a04 [label=<a<sub>0</sub><sup>(3)</sup>>];
    }
    {
    node [shape=circle, color=magenta, style=filled, fillcolor=magenta];
    x1 [label=PC1];
    x2 [label=PC2]; 
    x3 [label=PC3];
    x4 [label=PC4];
    x5 [label=PC5];
}
{
    node [shape=circle, color=cyan, style=filled, fillcolor=cyan];
    a12 [label=<a<sub>1</sub><sup>(1)</sup>>];
    a22 [label=<a<sub>2</sub><sup>(1)</sup>>];
    a32 [label=<a<sub>3</sub><sup>(1)</sup>>];
    E1  [shape=none  label='&#8942;' fontsize=30 fillcolor=white];
    a2562 [label=<a<sub>256</sub><sup>(1)</sup>>];
    a13 [label=<a<sub>1</sub><sup>(2)</sup>>];
    a23 [label=<a<sub>2</sub><sup>(2)</sup>>];
    a33 [label=<a<sub>3</sub><sup>(2)</sup>>];
    E2  [shape=none  label='&#8942;' fontsize=30 fillcolor=white];
    a2563 [label=<a<sub>256</sub><sup>(2)</sup>>];
    a14 [label=<a<sub>1</sub><sup>(3)</sup>>];
    a24 [label=<a<sub>2</sub><sup>(3)</sup>>];
    a34 [label=<a<sub>3</sub><sup>(3)</sup>>];
    E3  [shape=none  label='&#8942;' fontsize=30 fillcolor=white];
    a2564 [label=<a<sub>256</sub><sup>(3)</sup>>];
}
{
    node [shape=circle, color=green, style=filled, fillcolor=green];
    O1 [label=<C<sub>ab</sub>>];
    O2 [label=<C<sub>w</sub>>];
    O3 [label=<C<sub>m</sub>>];
    O4 [label=<LAI<sub>s</sub>>];
}
    {
        rank=same;
        x1->x2->x3->x3->x4->x5;
    }
    {
        rank=same;
        a02->a12->a22->a32->E1->a2562;
    }
    {
        rank=same;
        a03->a13->a23->a33->E2->a2563;
    }
    {
        rank=same;
        a04->a14->a24->a34->E3->a2564;
    }
    {
        rank=same;
        O1->O2->O3->O4;
    }
    a02->a03->a04;  // prevent tilting
    l0 [shape=plaintext, label='Input layer'];
    l0->x1;
    {rank=same; l0;x1};
    l1 [shape=plaintext, label='Hidden layer 1'];
    l1->a02;
    {rank=same; l1;a02};
    l2 [shape=plaintext, label='Hidden layer 2'];
    l2->a03;
    {rank=same; l2;a03};
    l3 [shape=plaintext, label='Hidden layer 3'];
    l3->a04;
    {rank=same; l3;a04};
    l4 [shape=plaintext, label='Output layer'];
    l4->O1;
    {rank=same; l4;O1};
    edge[style=solid, tailport=e, headport=w];
    {x1; x2; x3; x4; x5} -> {a12;a22;a32;E1;a2562};
    {a02;a12;a22;a32;E1;a2562} -> {a13;a23;a33;E2;a2563};
    {a03;a13;a23;a33;E2;a2563} -> {a14;a24;a34;E3;a2564};
    {a04;a14;a24;a34;E3;a2564} -> {O1,O2,O3,O4};
}", height="100%", width="100%")
