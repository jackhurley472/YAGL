
/*
 *  Standard library functions in C for YAGL language
 */

#include <stdio.h>
#include <fcntl.h>    /* For O_RDWR */
#include <unistd.h>   /* For open(), creat() */
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

int check_null(void *x) {
	int nullfd = open("/dev/random", O_WRONLY);
	if (!(write(nullfd, x, 1) < 0))
		return 1;
	return 0;
}

char *sconcat(char *s1, char *s2) {
	char *s3 = malloc(strlen(s1) + strlen(s2) + 1);
	strcpy(s3, s1);
	strcat(s3, s2);
	return s3;
}


struct node {
	int id;  // for hash table
	char* name;
	bool visited;
	int curr_dist;
};

struct graph {
	int n_size;
	int n_pos;
	int e_pos;
	struct node **nodes;
	struct edge_list **edges;
};

struct edge {
	struct node *from_node;
	struct node *to_node;
	int val;
	int deleted; // 0 is no 1 is yes
};

struct edge_list {
	struct edge *edge;
	struct edge_list *next_edge;
};

void copy_graph(struct graph *, struct graph *);
void insert_node(struct graph *, struct node*);
struct edge_list *new_edge_list(struct node *, struct node *, int);
void insert_into_edge_list(struct edge_list *, struct edge_list *);
int g_contain_n(struct graph *, struct node *);
char *node_to_string(struct node *);
char *edge_to_string(struct edge *);
void print_graph(struct graph *);
struct node *update_node(struct node *, char *);

struct node *update_node(struct node *n, char *name) {
	char *node_name = malloc(strlen(name) + 1);
	strcpy(node_name, name);

	char *old_name = n->name;
	n->name = node_name;
	return n;
}

/* For Jack's testing */
struct edge *make_edge(struct node *from, struct node *to, int v) {

	struct edge *edge = malloc(sizeof(struct edge));
	edge->from_node = from;
	edge->to_node = to;
	edge->val = v;
	edge->deleted = 0;
	return edge;
}

void insert_edge(struct graph *g, struct node *from, int v, struct node *to) {
	if (!g_contain_n(g, from) || !g_contain_n(g, to)) {
		printf("Nodes don't exist in given graph.\n");
		return;
	}

	// first find where to insert
	struct edge_list *list;
	int pos = g->e_pos;
	int from_id = from->id;
	for (int e = 0; e < g->e_pos; e++) {
		struct edge_list *at_e = g->edges[e];
		if (at_e != NULL) {
			struct edge *edge = at_e->edge;
			if (edge && edge->from_node && edge->from_node->id == from_id) {
				pos = e;
				edge->deleted = 0; // mark undeleted
			}
		}
	}

	// Next: insert
	struct edge_list *holder = NULL;
	if (pos == g->e_pos) {
		g->e_pos ++;
		holder = new_edge_list(from, to, v);
		g->edges[pos] = holder;
		return;
	}
	holder = g->edges[pos];
	struct edge_list *new_el = new_edge_list(from, to, v);
	insert_into_edge_list(holder, new_el);
}

void insert_into_edge_list(struct edge_list *e, struct edge_list *new) {
	struct edge_list *curr = e;
	if (curr->edge->to_node->id == new->edge->to_node->id) {
		// update cost
		curr->edge->val = new->edge->val;
		return;
	}
	while (curr->next_edge) {
		curr = curr->next_edge;
		if (curr->edge->to_node->id == new->edge->to_node->id) {
			// update cost
			curr->edge->val = new->edge->val;
			return;
		}
	}
	curr->next_edge = new;
}

struct edge_list *new_edge_list(struct node *from, struct node *to, int v) {
	struct edge_list *e = malloc(sizeof(struct edge_list));
	struct edge *edge = malloc(sizeof(struct edge));
	edge->from_node = from;
	edge->to_node = to;
	edge->val = v;
	e->edge = edge;
	e->next_edge = NULL;
	return e;
}
struct graph *make_graph(int n_size) {
	struct graph *g = malloc(sizeof(struct graph));
	g->n_size = n_size;
	g->n_pos = 0;
	g->e_pos = 0;
	g->nodes = (struct node **) malloc(sizeof(struct node *) * g->n_size);
	
	struct edge_list **edges = malloc(g->n_size * sizeof(struct edge_list *));
	g->edges = edges;
	return g;
}

void insert_node(struct graph *g, struct node *n) {
	if (g_contain_n(g, n)) {
		printf("Already inserted into graph.\n");
		return;
	}
	if (g->n_pos < g->n_size) {
		struct node **nodes = g->nodes;
		nodes[g->n_pos++] = n;
	} else {
		struct graph *g_new = make_graph(g->n_size*2); // double space
		copy_graph(g, g_new); // copy values of g to g_new
		memcpy(g, g_new, sizeof(*g)); // copy g_new to g in memory
		free(g_new); // clean up space
		insert_node(g, n); // insert new node
	}
	
}
char *node_to_string(struct node *n) {
	int name_length = strlen(n->name);
	int additional_space_safe = 30;
	char *s = malloc(additional_space_safe 
			+ name_length + 1);
	sprintf(s, "(%d) : %s", n->id, n->name);
	return s;
}
char *edge_to_string(struct edge *e) {
	char *to = node_to_string(e->to_node);
	char *from = node_to_string(e->from_node);
	int val = e->val;
	int name_length = strlen(to) + strlen(from);
	int additional_space_safe = 50;
	char *s = malloc(additional_space_safe 
			+ name_length + 1);
	sprintf(s, "[%s] ---(%d)---> [%s]", from, val, to);
	return s;
}
void remove_edge(struct graph *g, struct node *from, struct node *to) {
	if (!from || !to || !g)
		return;
	for (int i = 0; i < g->e_pos; i++) {
		struct edge_list *e = g->edges[i];
		struct edge_list *prev = 0;
		struct edge_list **home = &g->edges[i];
		while (e && e->edge) {
			struct edge *edge = e->edge;
			struct edge_list *next = e->next_edge;
			if (edge && edge->to_node && edge->from_node && 
			    edge->to_node->id == to->id && edge->from_node->id == from->id) {
				edge->deleted = 1;
				/*if (prev) {
					prev->next_edge = next;
				} else if (next) {
					g->edges[i] = next;
				} else {
					if (i != g->e_pos - 1)
						g->edges[i] = g->edges[i+1];
					else
						g->edges[i] = 0;
					g->e_pos--;
				}
				free(e);
				return;
			}*/

			}
			prev = e;
			e = next;
		}
	}
}
void remove_node(struct graph *g, struct node *n) {
	if (!g_contain_n(g, n)) {
		printf("Graph does not contain node %s\n", node_to_string(n));
		return;
	}

	// First remove all edges that contain that node
	for (int i = 0; i < g->e_pos; i++) {
		struct edge_list *e = g->edges[i];
		struct edge_list *prev = 0;
		while (e != NULL && e->edge != NULL) {
			struct edge *edge = e->edge;
			struct edge_list *next = e->next_edge;
			if (edge->to_node->id == n->id || edge->from_node->id == n->id) {
				remove_edge(g, edge->from_node, edge->to_node);
			}
			prev = e;
			e = next;
		}
	}

	// Second remove node
	int c = 0;
	for (c ; c < g->n_pos; c++) {
		struct node *curr = g->nodes[c];
		if (curr->id == n->id)
			break;
	}

	while (c + 1 < g->n_pos) {
		memcpy(g->nodes + c, g->nodes + c+1, sizeof(g->nodes[c+1]));
		c++;
	}
	struct node *curr = g->nodes[g->n_pos - 1];
	curr = NULL;
	g->n_pos--;
}

void copy_graph(struct graph *g, struct graph *g_new) {
	for (int i = 0; i < g->n_size; i++) {
		insert_node(g_new, g->nodes[i]);
	}
	for (int n = 0; n < g->e_pos; n++) {
		struct edge_list *e = g->edges[n];
		while (e != NULL && e->edge != NULL) {
			struct edge *edge = e->edge;
			e = e->next_edge;
			insert_edge(g_new, edge->from_node, edge->val, edge->to_node);
		}
	}
}
int g_contain_n(struct graph *g, struct node *n) {
	int on = 0;
	while (on < g->n_pos) {
		struct node *x = g->nodes[on++];
		if (x->id == n->id) 
			return 1;
	}
	return 0;
}

struct edge *g_contain_e(struct graph *g, struct edge *ed) {
	for (int n = 0; n < g->e_pos; n++) {
		struct edge_list *e = g->edges[n];
		while (e != NULL && e->edge != NULL) {
			struct edge *edge = e->edge;
			if (edge == ed)
				return edge;
			e = e->next_edge;
		}
	}
	return NULL;
}

static int id = 0;
struct node *make_node(char *name) {

	struct node *n = malloc(sizeof(struct node));

	char *node_name = malloc(strlen(name) + 1);
	strcpy(node_name, name);

	n->id = id++;
	n->name = node_name;
	n->visited = false;
	n->curr_dist = 0;
	return n;
}

bool is_visited(struct node *n) {
	return n->visited;
}

void update_visited(struct node *n, bool b) {
	n->visited = b;
}

int get_distance(struct node *n) {
	return n->curr_dist;
}

void update_distance(struct node *n, int i) {
	n->curr_dist = i;
}

char *update_node_name(struct node *n, char *new_name) {

	free(n->name);

	char *node_name = malloc(strlen(new_name) + 1);
	strcpy(node_name, new_name);

	n->id = id++;
	n->name = node_name;

	return n->name;
} 

void print_node(struct node *n) {
	printf("%s\n", n->name);
}

int get_graph_size(struct graph *g) {
	return g->n_pos;
}

char *get_name_node(struct node *n) {
	return n->name;
}

struct node *get_neighbor(struct graph *g, struct node *n, int pos) {
	int on = 0;
	for (int nn = 0; nn < g->e_pos; nn++) {
		struct edge_list *e = g->edges[nn];
		if (e->edge->from_node->id == n->id) {
			
			int nullfd = open("/dev/random", O_WRONLY);
			while (!(write(nullfd, e, sizeof(e)) < 0)) {
				struct edge *edge = e->edge;
				if (edge->deleted == 0) {
					if (on == pos) 
						return edge->to_node;
					on += 1;
				}
				e = e->next_edge;
			}	

		}
	}
	// Fail case: return n
	return n;
}
int get_num_neighbors(struct graph *g, struct node *n) {
	int on = 0;
	for (int nn = 0; nn < g->e_pos; nn++) {
		struct edge_list *e = g->edges[nn];
		if (e->edge->from_node->id == n->id) {
			
			int nullfd = open("/dev/random", O_WRONLY);
			while (!(write(nullfd, e, sizeof(e)) < 0)) {
				struct edge *edge = e->edge;
				if (edge->deleted == 0) {
					on += 1;
				}
				e = e->next_edge;
			}	

		}
	}
	// Fail case: return n
	return on;
}
struct node *get_node(struct graph *g, int pos) {
	return g->nodes[pos];
}
int get_weight(struct graph *g, struct node *n, struct node *n2) {
	for (int nn = 0; nn < g->e_pos; nn++) {
		struct edge_list *e = g->edges[nn];
		if (e->edge->from_node->id == n->id) {
			
			int nullfd = open("/dev/random", O_WRONLY);
			while (!(write(nullfd, e, sizeof(e)) < 0)) {
				struct edge *edge = e->edge;
				if (edge->deleted == 0 && edge->to_node->id == n2->id) {
					return edge->val;
				}
				e = e->next_edge;
			}	

		}
	}
	return -100000;
}

void print_graph(struct graph *g) {
	printf("============== Graph Print ===============\n");
	printf("\tStats: \t%d\t%d\t%d\n", g->n_size, g->n_pos, g->e_pos);
	int iter = 1;
	int per_line = 3;
	printf("\nNodes:\n");
	for (int n = 0; n < g->n_pos; n++)
		printf("Node %s%s", node_to_string(g->nodes[n]),
				iter++ % per_line == 0 ? "\n": 
				n == g->n_pos - 1 ? "" : " --- ");
	if (g->n_pos == 0)
		printf("There aren't any nodes in this graph.\n");
	printf("\n\n");
	printf("Edges:\n");
	int printed = 0;
	for (int n = 0; n < g->e_pos; n++) {
		struct edge_list *e = g->edges[n];
		int nullfd = open("/dev/random", O_WRONLY);
		while (!(write(nullfd, e, sizeof(e)) < 0)) {
			struct edge *edge = e->edge;
			if (edge->deleted == 0) {
				printed ++;
				printf("%s\n", edge_to_string(edge));
			}
			e = e->next_edge;
		}
	}
	if (printed == 0)
		printf("There aren't any edges in this graph.\n");

	printf("============ End Graph Print =============\n");

}
#ifdef BUILD_TEST
int main()
{
	return 0;
}
#endif
