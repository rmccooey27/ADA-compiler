/*************************************************************
 * binTree.h
 * Author: Regan McCooey
 * Date: 9/7/14
 * Binary Tree Assignment
 * CSCI 364 Professor King
 ************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>

typedef struct node node;
typedef struct btree btree;
typedef struct symbol symbol;
typedef struct eNode eNode;


struct symbol {
   char *kind;
   char *name;
   btree *parent_type;
   int value;
   char *mode;
   btree *next_param;
   btree *component_type;
   int lower;
   int upper;
   int size;
   int offSet;
   int iCount;
   int ARsize; 
   int isParam;
   eNode *eList;
   
};

struct btree {
   symbol data;
   btree *left;
   btree *right;
};

struct node {
   char *name;
   int pcVal;
   int offset;
   btree *root;
};

struct eNode {
   char *str;
   int index;
   eNode *next;
};


void pushTree( char* );
void popTree( );
int addSymbol( char* );
void searchSymbol( char* );
btree* searchTree( char*, btree* );
void insert( char*, btree** );
void printTree( btree* );
char* mallocCopy( char *str1 );

//symbol table
node stack[ 1000 ];
int stackCtr = 0;
int offset = 4;
int iCtr = 0;
int registerCt = 0;
int mainsOff = 0;
int mainBeg = 0;

//exceptions
int next_exception = 3;
int in_exception_part = 0;
int handler_done = 0;
int next_offset = 1;

//enums
eNode *tempE;
char *eTypeArr[ 100 ];
int eCt = 0;

/*
 * pushTree( ) 
 * Pre: The stack and the stackCtr are declared 
 * Post: A new node in the stack will be initalized and stackCtr 
 * will be incremented 
 */
void pushTree( char *fnName ) 
{
   if ( stackCtr < 999 ) {
      printf( "pushing scope for %s\n", fnName );
      stack[ stackCtr ].root = NULL;
      stack[ stackCtr ].name = mallocCopy( fnName );
      stack[ stackCtr ].pcVal = iCtr;
      stackCtr++;
   } 
   else {
      printf( "ERROR: the stack is full!\n" );
   }
}

/* 
 * popTree()
 * Pre: The stack and the stackCtr are declared
 * Post: stackCtr will be decremented or the top most node will be
 * popped off the stack
 */ 
void popTree( )
{
   if ( stackCtr == 0 ) {
      printf( "ERROR: the stack is empty!\n" );
   } else {
      stackCtr--;
   }
}

/*
 * addSymbol( int symbol )
 * Pre: The stack and stackCtr are declared
 * Post: A symbol will be added in the appropriate position of the tree 
 */
int addSymbol( char *symbol )
{
   if ( stackCtr <= 0 ) {
      printf( "ERROR: the stack is empty!\n" );
      return -1;
   }
   btree *found = searchTree( symbol, stack[ stackCtr - 1 ].root );
   if ( found != NULL ) {
       printf( "ERROR: symbol is already present in the local tree\n" );
       return -1;
   } else {
      insert( symbol, &( stack[ stackCtr-1 ].root ) );
      return 0;
   }
}

/*
 * searchSymbol( int symbol )
 * Pre: The stack and stackCtr are declared
 * Post: If the symbol is present within the stack of trees, a message will 
 * will be displayed contianing the name of the tree. Else: the message will 
 * state that the symbol has not been found. 
 */
void searchSymbol( char *symbol ) 
{
   if ( stackCtr <= 0 ) {
      printf( "ERROR: the stack is empty\n" );
      return;
   }

   char *location = "\0";
   int i;
   for ( i = stackCtr; i >=0; i-- ) {
      btree *found = searchTree( symbol, stack[ i-1 ].root );
      if ( found != NULL ) {
	 location = stack[ i-1 ].name;
	 int depth = stackCtr - i;
	 printf( "%s found in %s at depth %d\n", symbol, location, depth );
	 return;
      }
   }
   if ( location == "\0" ) {
      printf( "ERROR: %s was not found\n", symbol );
   }
}

/* 
 * insert( int symbol, btree *tree ) 
 * Pre: The stack and stackCtr are declared 
 * Post: A new symbol will be inserted in the appropriate 
 * position of the binary tree and that tree will be returned 
 */
void insert( char *name, btree **tree ) {
   
   if ( *tree == NULL ) {
      *tree = (  btree* )malloc( sizeof(  btree ) );
      ( *tree )->left = NULL;
      ( *tree )->right = NULL;
      ( *tree )->data.name = mallocCopy( name );
   } else {
      int comp = strncmp( name, ( *tree )->data.name, 1000 );
      if ( comp < 0 ) {
	 insert( name, &( ( *tree )->left ) );
      } else {
	 insert( name, &( ( *tree )->right ) );
      }
   }
}

/*
 * searchTree( int symbol, btree *tree ) 
 * Pre: The stack and stackCtr are declared 
 * Post: A pointer to the tree where the symbol was found
 * will be returned, if not found zero will be returned.
 */
btree* searchTree( char *name, btree *tree ) {
   if ( tree != NULL ) {
      int comp = strcmp( name, tree->data.name );
      if ( comp < 0 ) {
	 return searchTree( name, tree->left );
      } else if ( comp > 0 ) {
	 return searchTree( name, tree->right ); 
      } else {
	 return tree;
      }
   } 
   return NULL;
}

void printTree( btree *tree )
{
   if ( tree == NULL ) {
      return;
   } else {
      if ( tree->data.mode != NULL ) {
	 printTree( tree->left );
	 printf( "Parameter: %s : type: %s : mode: %s - offset = %d\n", tree->data.name, tree->data.parent_type->data.name, tree->data.mode, tree->data.offSet );
	 printTree( tree->right );
      } else if ( tree->data.parent_type != NULL ) {
	 printTree( tree->left );
	 printf( "%s : %s - offset = %d\n", tree->data.name, tree->data.parent_type->data.name, tree->data.offSet );
	 printTree( tree->right );
      } else {
	 printTree( tree->left );
	 printf( "%s : %s\n", tree->data.name, tree->data.kind );
	 printTree( tree->right );
      }
   }
   return;
}


char* mallocCopy( char *str1 ) 
{
   char *str2 = malloc( sizeof( str1 )+1 );
   strcpy( str2, str1 );
   return str2;
}

void destroyTree( btree *tree ) 
{
   if ( tree == NULL ) {
      return;
   } else {
      destroyTree( tree->right );
      destroyTree( tree->left );
      free( tree );
   }      
}
