#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>
#include <string.h>

typedef struct PLnode {
   int line;
   int val;
   struct PLnode *next;
} *PLnodeptr;

PLnodeptr toPatch[ 100 ];
int regJump[ 100 ];
int iJump[ 100 ];
int caseExprReg[ 100 ];
int caseReg = -1;
int baseN = -1;
int patchT = -1;
int regJumpT = -1;
PLnodeptr PLhead = NULL;

//exceptions
PLnodeptr raise_list = NULL;
int jumpSize = 50;
int jump_table[ 50 ];
int jumpTableAdd = -1;

void printPL( PLnodeptr list, int p );
void bSort( PLnodeptr start );

PLnodeptr appendPL( int line, int val, int p, PLnodeptr list ) 
{
   PLnodeptr temp = ( PLnodeptr )malloc( sizeof( struct PLnode* ) );
   temp->line = line;

   if ( p == 1 ) {
      temp->val = val;
   }

   if ( list == NULL ) {
      list = temp;
   } else {
      PLnodeptr current = list;
  
      while ( current->next != NULL ) {
	 current = current->next;
      }
      current->next = temp;
   }
   return list;
}

void printPL( PLnodeptr list, int p )
{
   PLnodeptr current = list;
   while ( current != NULL ) {
      printf( "%d ", current->line );
      if ( p == 1 ) {
	 printf( ": %d", current->val );
      }
      printf( "\n" );
      current = current->next;
   }

}

void pushPatch() 
{
   if ( patchT < 100 ) {
      patchT++;
      toPatch[ patchT ] = NULL;
   }
}
 
void popPatch() 
{
   if ( patchT >= 0 ) {
      patchT--; 
      printf( "patchT is now %d\n", patchT );
   }
}

PLnodeptr popAndPatch( PLnodeptr patch, int value ) 
{
   PLnodeptr current = patch;
   PLnodeptr deleteN = NULL;
   PLnodeptr currentPatch = PLhead;
   printPL( patch, 0 );
   while ( current != NULL ) {
      currentPatch = appendPL( current->line, value, 1, currentPatch ); 
      current = current->next;
   }
   bSort( currentPatch );
   popPatch();
   return currentPatch;
}

PLnodeptr patch( PLnodeptr patch, int value ) 
{
   PLnodeptr current = patch;
   PLnodeptr deleteN = NULL;
   PLnodeptr currentPatch = PLhead;
   printPL( patch, 0 );
   while ( current != NULL ) {
      printf( "patching line %d with %d\n", current->line, value );
      currentPatch = appendPL( current->line, value, 1, currentPatch ); 
      current = current->next;
   }
   bSort( currentPatch );
   return currentPatch;
}

/* function to swap data of two nodes a and b*/
void swap( PLnodeptr a, PLnodeptr b )
{
    int temp = a->line;
    int temp2 = a->val;
    a->line = b->line;
    a->val = b->val;
    b->line = temp;
    b->val = temp2;
}

void bSort( PLnodeptr start )
{
    int swapped, i;
    PLnodeptr ptr1;
    PLnodeptr lptr = NULL;

    /* Checking for empty list */
    if (ptr1 == NULL)
        return;

    do
    {
        swapped = 0;
        ptr1 = start;

        while (ptr1->next != lptr)
        {
            if (ptr1->line > ptr1->next->line)
            { 
                swap(ptr1, ptr1->next);
                swapped = 1;
            }
            ptr1 = ptr1->next;
        }
        lptr = ptr1;
    }
    while (swapped);
}

void patchFile ( FILE *fp, FILE *out ) 
{
   char buffer[ 300 ];
   size_t length = 0;
   char *line = NULL;
   int currL = 0;
   int pVal = 0;
   while ( getline( &line, &length, fp ) != -1 ) {
      strcpy( buffer, line );
      currL = buffer[0];
      int i;
      for ( i = 0; i < length; i++ ) {
	 if ( buffer[i] == '?' ) {
	    pVal = searchPList( currL );
	    if ( pVal != -1 )
	       fprintf( out, "%d", pVal );
	 } else {
	    fprintf( out, "%c", buffer[i] );
	 }
      }
   }


}

int searchPList ( int lineNum ) 
{
   PLnodeptr current = PLhead;
   while ( current != NULL ) {
      if ( current->line == lineNum ) {
	 return current->val;
      }
      current = current->next;
   }
   return -1;
}
