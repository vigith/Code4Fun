// Question:
// Given a binary tree, return true or false based on whether the tree is a BST (Binary Search Tree) or not. (left >= right)

/* LOGIC: 
 * 1. Since in-order traversal of a BST is sorted, just keep a static variable and check whether the root->data is less 
 *    that the static variable (static variable will have the previous value) 
 *
 * 2a. left >= root and right < root. (this is opposite of the convention) 
 * 2b. min in the left subtree will be greater than the root and max in the right subtree will be less than the root 
 */

#include <stdio.h>
#include <stdlib.h>

int prev = 9999;

struct node *node_alloc(void);

struct node {
  int data;
  struct node *left;
  struct node *right;
};

// creates a node
struct node *node_alloc(void) {
  struct node *node = (struct node *) malloc(sizeof(struct node));
  if (node == NULL)
    exit(255);
  return node;
}

// create a good BST
struct node *create_good_bst(void) {
  struct node *root = node_alloc();

  struct node *left1 = node_alloc();
  struct node *right1 = node_alloc();

  struct node *left2 = node_alloc();
  struct node *right2 = node_alloc();

  struct node *left3 = node_alloc();
  struct node *right3 = node_alloc();
  root->data  = 10;
  root->left  = left1;
  root->right = right1;
  
  left1->data = 20;
  left1->left = left2;
  left1->right = right2;

  right1->data = 5;
  right1->left = NULL;
  right1->right = NULL;

  left2->data = 21;
  left2->left = NULL;
  left2->right = NULL;

  right2->data = 18;
  right2->left = NULL;
  right2->right = NULL;

  return root;
}

// create a bad BST
struct node *create_bad_bst(void) {
  struct node *root = node_alloc();

  struct node *left1 = node_alloc();
  struct node *right1 = node_alloc();

  struct node *left2 = node_alloc();
  struct node *right2 = node_alloc();

  struct node *left3 = node_alloc();
  struct node *right3 = node_alloc();
  root->data  = 10;
  root->left  = left1;
  root->right = right1;
  
  left1->data = 20;
  left1->left = left2;
  left1->right = right2;

  right1->data = 5;
  right1->left = NULL;
  right1->right = NULL;

  left2->data = 21;
  left2->left = NULL;
  left2->right = NULL;

  right2->data = 8;
  right2->left = NULL;
  right2->right = NULL;

  return root;
}

int min_left(struct node *root) {
  if (root->right == NULL)
    return root->data;

  return min_left(root->right);
}

int max_right(struct node *root) {
  if (root->left == NULL)
    return root->data;

  return max_right(root->left);
}

short int is_bst_l(struct node *root) {
  if (root->left == NULL)
    return 1;

  if (is_bst_l(root->left)) {
    if (min_left(root->left) >= root->data) {
      return 1;
    }
  }
  
  return 0;
}


short int is_bst_r(struct node *root) {
  if (root->left == NULL)
    return 1;

  if (is_bst_r(root->right)) {
    if ( max_right(root->right) <= root->data) {
      return 1;
    }
  }
  
  return 0;
}

short int is_bst(struct node *root) {
  if (root->left == NULL)
    return 1;

  if (is_bst_r(root) && is_bst_l(root)) {
    if ( max_right(root->right) <= root->data) {
      return 1;
    } 
  }

  return 0;
}


// in-order traversal (it should be ordered)
int traverse_bst(struct node *root) {
  if (root == NULL)
    return 1;
  traverse_bst(root->left);
  if (root->data > prev) {
    printf("false\n");
    //exit(255);
  }
  printf("%d\n", root->data);
  prev = root->data;
  traverse_bst(root->right);
}

int main(void) {
  struct node *bst = create_good_bst();
  printf("==1st==\n");
  traverse_bst(bst);
  printf("%s\n", (is_bst(bst) ? "good" : "bad") );
  bst = create_bad_bst();

  printf("==2nd==\n");
  traverse_bst(bst);
  printf("%s\n", (is_bst(bst) ? "good" : "bad") );
  return 0;  
}
