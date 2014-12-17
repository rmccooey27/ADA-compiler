/*************************************************************
 * outerContext.c
 * Author: Regan McCooey
 * Date: 10/30/14
 * Symbol Table Implementation
 * CSCI 364 Professor King
 ************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>
      
void createOuterContext( )
{
   pushTree( "Outer Context" );

   insert( "integer", &( stack[ stackCtr-1 ].root ) );
   btree *newContext = searchTree( "integer", stack[ stackCtr-1 ].root );
   newContext->data.kind = mallocCopy( "type" );
   newContext->data.size = 1;

   insert( "boolean", &( stack[ stackCtr-1 ].root ) );
   newContext = searchTree( "boolean",  stack[ stackCtr-1 ].root ); 
   newContext->data.kind = mallocCopy( "type" );
   newContext->data.size = 1;

   insert( "true", &( stack[ stackCtr-1 ].root ) );
   newContext = searchTree ( "true", stack[ stackCtr-1 ].root );
   newContext->data.kind = mallocCopy( "value" );
   newContext->data.value = 1;
   newContext->data.size = 1;
  
   insert( "false", &( stack[ stackCtr-1 ].root ) );
   newContext = searchTree ( "false", stack[ stackCtr-1 ].root );
   newContext->data.kind = mallocCopy( "value" );
   newContext->data.value = 0;
   newContext->data.size = 1;

   insert( "read", &( stack[ stackCtr-1 ].root ) );
   newContext = searchTree ( "read", stack[ stackCtr-1 ].root );
   newContext->data.kind = mallocCopy( "read_routine" );
   
   insert( "write", &( stack[ stackCtr-1 ].root ) );
   newContext = searchTree( "write", stack[ stackCtr-1 ].root );
   newContext->data.kind = mallocCopy( "write_routine" );

   insert( "constraint_error", &( stack[ stackCtr-1 ].root ) );
   newContext = searchTree( "constraint_error", stack[ stackCtr-1 ].root );
   newContext->data.kind = mallocCopy( "exception" );
   newContext->data.iCount = 1;
   
   insert( "numeric_error", &( stack[ stackCtr-1 ].root ) );
   newContext = searchTree( "numeric_error", stack[ stackCtr-1 ].root );
   newContext->data.kind = mallocCopy( "exception" );
   newContext->data.iCount = 2;

   printf( "%s:\n", stack[ stackCtr-1 ].name );
   printTree( stack[ stackCtr-1 ].root );


}
