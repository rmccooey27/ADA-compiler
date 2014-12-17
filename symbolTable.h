/*************************************************************
 * symbolTable.h
 * Author: Regan McCooey
 * Date: 10/30/14
 * Symbol Table Implementation
 * CSCI 364 Professor King
 ************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>


btree* searchForParent( char *type );

void parseVarIdList( idnodeptr list, char *typeName )
{
   printf( "in parse Var id\n" );
   idnodeptr temp = NULL;
   btree *findInserted;
   btree *typeSearch;
   while ( list != NULL ) {
      int check = addSymbol( list->name );
      if ( check != -1 ) {
	 findInserted = searchTree( list->name, stack[ stackCtr-1 ].root );
	 typeSearch = searchForParent( typeName );
	 findInserted->data.isParam = 0;
	 findInserted->data.kind = mallocCopy( typeSearch->data.kind );	
	 int comp = strcmp( findInserted->data.kind, "array" );
	 if ( typeSearch != NULL ) {
	    findInserted->data.parent_type = typeSearch;
	    findInserted->data.size = typeSearch->data.size;
	    
	    if ( comp == 0 ) {
	       findInserted->data.offSet = typeSearch->data.offSet;
	       offset += findInserted->data.size;
	    } else {
	       findInserted->data.offSet = offset;
	       offset += findInserted->data.size;
	    }
	 }
      }
	 temp = list;
	 list = list->next;
	 free( temp );
	 findInserted = NULL;
	 typeSearch = NULL;
   }
}

void parseExceptionList( idnodeptr list )
{
   idnodeptr temp = NULL;
   btree *findInserted;
   btree *typeSearch;
   while ( list != NULL ) {
      int check = addSymbol( list->name );
      if ( check != -1 ) {
	 findInserted = searchTree( list->name, stack[ stackCtr-1 ].root );
	 findInserted->data.kind = mallocCopy( "exception" );
	 findInserted->data.size = 1;
	 findInserted->data.isParam = 0;
	 findInserted->data.iCount = next_exception;
	 next_exception++;
	 //findInserted->data.offSet = offset;
      }
	 temp = list;
	 list = list->next;
	 free( temp );
	 findInserted = NULL;
	 typeSearch = NULL;
   }
}



btree* parseParamList ( idnodeptr list, char *mode, char *typeName ) 
{
   btree *previousNode = NULL;
   btree *returnNode = NULL;
   idnodeptr temp = NULL;
   btree *findInserted;
   btree *typeSearch;
   int comp;
   int comp2;
   while ( list != NULL ) {
      int check = addSymbol( list->name );
      if ( check != -1 ) {
	 findInserted = searchTree( list->name, stack[ stackCtr-1 ].root );
	 findInserted->data.kind = mallocCopy( "param" );
	 typeSearch = searchForParent( typeName );
	 if ( typeSearch == NULL ) {
	    printf( "ERROR: type does not exist\n" );
	    return;
	 }
	 findInserted->data.parent_type = typeSearch;
	 findInserted->data.isParam = 1;
	 findInserted->data.size = typeSearch->data.size;
	 findInserted->data.mode = mallocCopy( mode );
	 findInserted->data.offSet = offset;
	 offset += findInserted->data.size;
	 comp = strcmp( mode, "in out" );
	 comp2 = strcmp( mode, "out");
	 if ( ( comp == 0 ) || ( comp2 == 0 ) ) {
	    offset += findInserted->data.size;
	 }
	 if ( previousNode != NULL ) {
	    previousNode->data.next_param = findInserted; 
	 } 
	 returnNode = findInserted;
	 previousNode = findInserted; 
      }
	 temp = list;
	 list = list->next;
	 findInserted = NULL;
	 typeSearch = NULL;
	 free( temp );
   }   
   if ( returnNode != NULL ) {
      return returnNode;
   }
   
}

btree* searchForParent( char *type ) 
{
   int i;
   btree* found = NULL;
   for ( i = stackCtr-1; i >= 0; i-- ) {
      btree *temp = stack[ i ].root;
      found = searchTree( type, temp );
      if ( found != NULL ) {
	 return found;
      }
   }
   return NULL;
}

