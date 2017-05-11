/*
 * Examples of gnuplot_i.c usage
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <math.h>

#define PI 3.14159

#define _GNU_SOURCE
#define MAX 50
#define MAX_NODE_STORE 1000
/*
%struct.15 = type { { %struct.6*, i32 }, { %struct.10*, i32 }, %struct.9 }
%struct.6 = type { i32 }
%struct.10 = type { %struct.6*, %struct.6*, i1, %struct.9 }
%struct.9 = type { i32 }
*/

typedef struct {
    int key;
}node;

typedef struct {
    int w;
}attr;

typedef struct {
    node* n1;
    node* n2;
    int directed;
    attr weight;
}edge;

typedef struct {
    node* node_list;
    int size;
}lst_node;

typedef struct {
    edge* edge_list;
    int size;
}lst_edge;

typedef struct {
    lst_node nodes;
    lst_edge edges;
    attr def_weight;
}graph;

typedef struct {
    int  key[MAX_NODE_STORE];
    int count;
}node_tbl;

node_tbl lookup;
typedef struct {
    int nodes[MAX];
    int to[MAX];
    int from[MAX];
    int weights[MAX];
    int num_nodes;
    int num_edges;
    int directed;
} Node_info;

int display_graph(Node_info* info, int directed) 
{
    FILE* fp = fopen("pnts.dat","w");
    FILE* fe = fopen("edges.dat","w");
    float x, y ;
    
     
     
    for(int i = 0; i <5; i++) 
    {
       x = (cos(PI/2 + ((2*PI)/5)*i));
       y = (sin(PI/2 + ((2*PI)/5)*i))    ;
       fprintf(fp,"%d\t%f\t%f\n",i,x,y);
      
    }
    for(int i = 5; i < info->num_nodes - 1 ; i++) 
    {
       x =(0.5*cos(PI/2 + ((2*PI)/5)*i));
       y =(0.5*sin(PI/2 + ((2*PI)/5)*i));
       fprintf(fp,"%d\t%f\t%f\n",i,x,y);
      
    }
   
    for(int i = 0; i < info->num_edges; i++) 
    {
        fprintf(fe,"%d\t%d\t%d\t%d\t%d\n", info->from[i] - 1, 
        info->to[i] - 1,info->weights[i],-1,1);
    }    

    fclose(fp);
    fclose(fe);
    if(directed)
        system("gnuplot gnuplot_dir.sh -persist");
   else
        system("gnuplot gnuplot.sh -persist");
        
   return 0 ;
}

int set_mapping_node_addr(node* n1, int size) 
{
     int found = 0;
     for(int i = 0; i < size; i++) 
     {
        found = 0;
        for(int j = 0; j < lookup.count; j++) 
        { 
            if(n1[i].key == lookup.key[j])
                found = 1;
        }
     
        if(!found)
            lookup.key[lookup.count++] = n1[i].key;
      }
      return 0;
}

int get_mapping_node_addr(node* n1) {
     int i = 0;
      for(i = 0; i < lookup.count; i++) 
      { 
         if(n1->key == lookup.key[i])
            return i;
      }
      return -1;
}

/*
int sample_display(int x) 
{
    Node_info n1;
    n1.num_nodes = 4;
    n1.num_edges = 3;
    int arr[] = {0,1,2,3};
    memcpy(n1.nodes,arr,sizeof(arr));
    int arr2[] = {0,1,3};
    memcpy(n1.from,arr2,sizeof(arr2));
    int arr3[] = {1,3,2};
    memcpy(n1.to,arr3,sizeof(arr3));
    int arr4[] = {20,50,30};
    memcpy(n1.weights,arr4,sizeof(arr4));
    return display_graph(&n1);
    
}
*/
int fill_edge_info(int* to, int* from, int* weight, edge* edges, int size,int default_weight) {

   int directed = 0;
   for(int i = 0; i < size; i++) {
       
        to[i] = get_mapping_node_addr(edges[i].n1);
        from[i] = get_mapping_node_addr(edges[i].n2);
        weight[i] = edges[i].weight.w;
        if(weight[i] == 0)
            weight[i] = default_weight;
        if(edges[i].directed == 1)
            directed = 1;   
   }

   return directed;
}

int display(graph g) {

    int directed = 0;
    node d_nodes[MAX];
    edge d_edges[MAX];
    memcpy(d_nodes,g.nodes.node_list,g.nodes.size*sizeof(node));
    memcpy(d_edges,g.edges.edge_list,g.edges.size*sizeof(edge));
    printf("EDGES - %d\n",g.edges.size);
    printf("NODES - %d\n",g.nodes.size);
    printf("DEFAULT WEIGHT %d\n",g.def_weight.w);
    Node_info n1;
    n1.num_nodes = g.nodes.size;
    n1.num_edges = g.edges.size;
    set_mapping_node_addr(d_nodes,g.nodes.size);
    directed = fill_edge_info(n1.to,n1.from,n1.weights,d_edges,g.edges.size,g.def_weight.w);
 
    /*
    for(int k = 0; k < g.nodes.size; k++) {
        printf("\n - node - %p, key - %d\n",&d_nodes[k],d_nodes[k].key);
     }
     */

    for(int k = 0; k < g.edges.size; k++) {
        printf("\nfrom key1 - %d -> to key2 - %d\n",
        d_edges[k].n1->key,d_edges[k].n2->key);
     }

    return display_graph(&n1,directed);
}
