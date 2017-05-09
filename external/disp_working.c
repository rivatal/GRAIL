/*
 * Examples of gnuplot_i.c usage
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

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
    node* node_addr[MAX_NODE_STORE];
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
    int count = 5; 
    for(int i = 0; i < info->num_nodes; i++) 
    {
      if(count % 2)
            fprintf(fp,"%d\t%d\t%d\n",i,count,count+5);
       else     
            fprintf(fp,"%d\t%d\t%d\n",i,count + 5,count);
      
       count += 5;
    }
    for(int i = 0; i < info->num_edges; i++) 
    {
        fprintf(fe,"%d\t%d\t%d\t%d\t%d\n", info->from[i], 
        info->to[i],info->weights[i],0,1);
    }    

    fclose(fp);
    fclose(fe);
    if(directed)
        system("gnuplot gnuplot_dir.sh -persist");
   else
        system("gnuplot gnuplot_dir.sh -persist");
        
   return 0 ;
}

int set_mapping_node_addr(lst_node* nodes) {
      for(int i = 0; i < nodes->size; i++) 
      { 
           lookup.node_addr[i] = nodes->node_list;
           nodes->node_list += sizeof(node*);
           lookup.count++;
      }
     
    return 0;
}

int get_mapping_node_addr(node* n1) {
     int i = 0;
      for(i = 0; i < lookup.count; i++) 
      { 
         if(n1 == lookup.node_addr[i])
            return i;
      }
      
      lookup.node_addr[i] = n1;
      lookup.count++;
      return i;
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
int fill_edge_info(int* to, int* from, int* weight, lst_edge* edges, int default_weight) {

   int directed = 0;
   for(int i = 0; i < edges->size; i++) {
       
        edge* e = edges->edge_list;
        to[i] = get_mapping_node_addr(e->n1);
        from[i] = get_mapping_node_addr(e->n2);
        weight[i] = e->weight.w;
        if(weight[i] == 0)
            weight[i] = default_weight;
        if(e->directed == 1)
            directed = 1;   
        edges->edge_list += sizeof(edge*);
   }

   return directed;
}

int display(graph g) {

    int directed = 0;
    printf("EDGES - %d\n",g.edges.size);
    printf("NODES - %d\n",g.nodes.size);
    printf("DEFAULT WEIGHT %d\n",g.def_weight.w);
    Node_info n1;
    n1.num_nodes = g.nodes.size;
    n1.num_edges = g.edges.size;
    //set_mapping_node_addr(&g.nodes);
    directed = fill_edge_info(n1.to,n1.from,n1.weights,&g.edges,g.def_weight.w);
  
  /*
    for(int k = 0; k < g.nodes.size; k++) {
        node* n = g.nodes.node_list;
        printf("\n - node - %p, key - %d\n",n,n->key);
        g.nodes.node_list += sizeof(node*);
     }
    for(int k = 0; k < g.edges.size; k++) {
        edge* e = g.edges.edge_list;
        printf("\nfrom - %p, to - %p,key1 - %d -> key2 - %d\n",e->n1,e->n2,e->n1->key,e->n2->key);
     }
 */
    return display_graph(&n1,directed);
}
