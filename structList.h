/*************************************************************
 * structList.c
 * Author: Regan McCooey
 * Date: 9/14/14
 * List of Structs Assignment
 * CSCI 364 Professor King
 ************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>
#include <string.h>

typedef struct idnode {
   char *name;
   struct idnode *next;
} *idnodeptr;

idnodeptr addName( char *input, idnodeptr list );
idnodeptr addInteger( int data, idnodeptr list );
void print( idnodeptr list );


idnodeptr addName ( char *input, idnodeptr list ) 
{  
   idnodeptr tempNode = ( idnodeptr )malloc( sizeof( struct idnode ) );
   tempNode->next = list;
   list = tempNode;
   tempNode->name = mallocCopy( input );
   return list;
}

void print ( idnodeptr list ) 
{
   idnodeptr current = list;
   while ( current != NULL ) {
      if ( current->name != NULL ) {
	 printf( "%s\n", current->name );
      }
      current = current->next;
   }
}




