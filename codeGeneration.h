#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>


struct exprNode {
   int value;
   int baseReg;
   int offset;
   int isVar;
   int regNum;
   int isLocal;
   int inReg;
   int isBool;
   int isArray;
   int isParamProc;
   int iVal;
   int lowerReg;
   int upperReg;
   int isRange;
   int isEnum;
   struct exprNode *next;
   struct exprNode *indexExpr;
};
   
typedef struct exprNode exprNode;
exprNode *exprList = NULL; 

void generatePrologue( FILE *outputFile )
{
   fprintf( outputFile, "0 b := ?\n" );
   pushPatch();
   toPatch[ patchT ] = appendPL( 0, 0, 0, toPatch[ patchT ] );
   fprintf( outputFile, "1 contents b, 0 := ?\n" );
   pushPatch();
   toPatch[ patchT ] = appendPL( 1, 0, 0, toPatch[ patchT ] );
   fprintf( outputFile, "2 contents b, 1 := 4\n" );
   fprintf( outputFile, "3 r%d := 0\n", getRegisterCt() );
   fprintf( outputFile, "4 pc := ?\n" );
   pushPatch();
   toPatch[ patchT ] = appendPL( 4, 0, 0, toPatch[ patchT ] );
   fprintf( outputFile, "5 halt\n" );
   iCtr = 6;
  
}

btree* walkBack( char *var, int *location, int reg, FILE *f, int isFunction ) 
{
   
   btree *found = NULL; 
   int i;

   for ( i = stackCtr-1; i >= 0; i-- ) {
      btree *temp = stack[ i ].root;
      found = searchTree( var, temp );
      if ( found != NULL ) {
	 *location = stackCtr-1-i;
	 break;
      }
   }
   
   if ( found == NULL ) {
      return found;
   }

   int comp = strcmp( "value", found->data.kind );
   if ( comp == 0 ) {
      return found;
   }
   comp = strcmp( "procedure", found->data.kind );
   if ( comp == 0 ) {
      isFunction = 1;
   }

   if ( ( *location > 0 ) && ( isFunction == 0 ) ) {
      reg = getRegisterCt();
      fprintf( f, "%d r%d := contents b, 2\n", getICount(), reg ); 
   } else if ( ( *location > 0 ) && ( isFunction == 1 ) ) {
      reg = getRegisterCt();
      fprintf( f, "%d r%d := contents r%d, 2\n", getICount(), reg, reg );
   } 

   int l = 0;
   if ( isFunction == 1 ) {
      l = 1;
   }
   for ( i = l; i < *location-1; i++ ) {
      reg = registerCt;
      fprintf( f, "%d r%d := contents r%d, 2\n", getICount(), reg, reg );
   }
  
   reg = registerCt;
   if ( isFunction == 1 && *location == 1 ) {
      fprintf( f, "%d contents b, 2 := r%d\n", getICount(), reg );
   }else if ( isFunction == 1 && *location > 0 ) {
      fprintf( f, "%d contents b, 2 := contents r%d, 2\n", getICount(), reg );
   }
   return found;
}

int getRegisterCt( )
{
   return ++registerCt;
}

int getICount( )
{
   return iCtr++;
}

void emitWrite( FILE *f, exprNode *list ) 
{
   exprNode *current = list;
   
   while( current != NULL ) {
      fprintf( f, "%d write ", getICount() );
      if ( current->inReg == 1 ) {
	 fprintf( f, "r%d\n", current->regNum );
      } else {
	 if ( current->isLocal == 1 ) {
	    fprintf( f, "contents b, %d\n", current->offset );
	 } else {
	    fprintf( f, "contents r%d, %d\n", current->regNum, current->offset );
	 }
      }
      current = current->next;
   }
}

void emitRead( FILE *f, exprNode *list ) 
{
   exprNode *current = list;
   while( current != NULL ) {
      if ( current->isBool == 1 ) {
	 fprintf( f, "%d read_boolean ", getICount() );
      } else {
	 fprintf( f, "%d read_integer ", getICount() );
      }
      if ( current->inReg == 1 ) {
	 yyerror( "ERROR: must read into a memory location not a register!\n" );
      } else {
	 if ( current->isLocal == 1 ) {
	    fprintf( f, "contents b, %d\n", current->offset );
	 } else {
	    fprintf( f, "contents r%d, %d\n", current->regNum, current->offset );
	 }
      }
      current = current->next;
   }

}
