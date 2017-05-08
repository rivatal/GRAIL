
/*
 * Examples of gnuplot_i.c usage
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX 50
typedef struct {
    int nodes[MAX];
    int to[MAX];
    int from[MAX];
    int weights[MAX];
    int num_nodes;
    int num_edges;
} Node_info;

int display_graph(Node_info* info) 
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
    system("gnuplot gnuplot.sh -persist");
    return 0 ;
}

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
