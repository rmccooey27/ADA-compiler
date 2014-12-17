%{ 

   /*
   Regan McCooey
   9/24/14
   Grammar for Ada
   CSCI 364
   Creates the grammar for the Ada subset 
  */
   #include <stdio.h>
   #include "include.h"
   int yydebug = 1;
   extern char linebuff[ 500 ];
   idnodeptr idData;

 %}

%token IS BEG END PROCEDURE ID NUMBER TYPE ARRAY RAISE OTHERS EXPONENT
%token RECORD IN OUT RANGE CONST ASSIGN EXCEPTION NULLWORD LOOP IF
%token THEN ELSEIF ELSE EXIT WHEN AND OR EQ NEQ LT GT GTE LTE TICK
%token NOT EXP ARROW OF DOTDOT ENDIF ENDREC ENDLOOP EXITWHEN CASE
%type <integer> NUMBER
%type <integer> constant
%type <var> ID
%type <var> identifier
%type <expr_nodeptr> formal_param_list
%type <expr_nodeptr> formal_param
%type <var> procedure_header
%type <id_listptr> id_list
%type <var> type_name
%type <var> mode
%type <var> procedure_specification
%type <integer> constant
%type <integer> adding_op
%type <integer> multiplying_op
%type <integer> relational_op
%type <exprN> expression
%type <exprN> relation
%type <exprN> simple_expr
%type <exprN> term
%type <exprN> factor
%type <exprN> choice_list
%type <exprN> choice
%type <exprN> primary
%type <exprN> assignment
%type <exprN> enum_op
%type <exprN> condition
%type <exprN> function_params
%type <exprN> optional_params
%type <exprN> optional_assign
%type <exprN> procedure_call
%type <integer> boolean_op
%type <integer> beg_loop
%type <eNodeL> enum_id_list
%union {
   int integer;
   char *var;
   struct btree *expr_nodeptr; 
   struct node *expr_list;
   struct symbol *symbol_table_ptr;
   struct idnode *id_listptr;
   struct exprNode *exprN;
   struct eNode *eNodeL;
   
}
%%

main_decl : procedure_decl ';'
{
   printf( "**DONE**\n" ); 
}       
;

procedure_decl :              
procedure_specification IS          
 declarative_part         
BEG 
{
   if ( stackCtr > 2 ) {
      btree *search = searchForParent( $1 );
      //search->data.ARsize = offset;
      search->data.iCount = iCtr;
   } 
   mainBeg = iCtr;
   
   in_exception_part = 0;
   raise_list = NULL;
}
   statement_sequence
   exception_part
END                                         
{ 
   


   char *previous;
  
   if ( stackCtr > 2 ) {
      //check for in out/ out params
      btree *params;
      int offset;
      int reg;
      int comp; 
      int comp2;
      params = stack[ stackCtr-1 ].root;
      while ( ( params != NULL ) && ( params->data.isParam == 1 ) ) {
	 comp = strcmp( "out", params->data.mode );
	 comp2 = strcmp( "in out", params->data.mode );
	 if ( ( comp == 0 ) || ( comp2 == 0 ) ) {
	    offset = params->data.offSet;
	    reg = getRegisterCt();
	    fprintf( outputFile, "%d r%d := contents b, %d\n", getICount(), reg,  offset + params->data.size );
	    fprintf( outputFile, "%d contents r%d := contents b, %d\n", getICount(), reg, offset );

	 }
	 params = params->data.next_param;
      }
      

   }
   if ( stackCtr > 1 ) {
      printf( "Vars in scope %s: \n", stack[ stackCtr-1 ].name );
      printTree( stack[ stackCtr-1 ].root );
      popTree();  
   }

   if ( stackCtr > 1 ){
      int currentReg = getRegisterCt();
      fprintf( outputFile, "%d r%d := contents b, 1\n", getICount(), currentReg );
      fprintf( outputFile, "%d b := contents b, 3\n", getICount() );
      fprintf( outputFile, "%d pc := r%d\n", getICount(), currentReg );
   } else if ( stackCtr > 0 ) {
      int currentReg = getRegisterCt();
      fprintf( outputFile, "%d r%d := contents b, 1\n", getICount(), currentReg );
      fprintf( outputFile, "%d b := contents b, 3\n", getICount() );
      int i = getICount();
      fprintf( outputFile, "%d pc := r%d\n", i, currentReg );
      PLhead = popAndPatch( toPatch[ patchT ], mainBeg );
      PLhead = popAndPatch( toPatch[ patchT ], i+1+mainsOff );
      PLhead = popAndPatch( toPatch[ patchT ], i+1 );
      printf( "final patch list\n" );
      printPL( PLhead, 1 );
   }
}
;

procedure_specification : procedure_header  '(' formal_param_list ')' 
                        {  
			   $$ = $1;
			   if ( stackCtr > 2 ) {
			      insert( $1, &( stack[ stackCtr-2 ].root ) );
			      btree *find_inserted = searchTree( $1, stack[ stackCtr-2 ].root );
			      find_inserted->data.kind = mallocCopy( "procedure" );
			      find_inserted->data.next_param = $3;
			      find_inserted->data.iCount = iCtr;
			   }
			}
                        | procedure_header 
			{ 
			   $$ = $1;
			   if ( stackCtr > 2 ) {
			      insert( $1, &( stack[ stackCtr-2 ].root ) );
			      btree *find_inserted = searchTree( $1, stack[ stackCtr-2 ].root );
			      find_inserted->data.kind = mallocCopy( "procedure" );
			      find_inserted->data.iCount = iCtr;
			   }
			}
		
;

procedure_header : PROCEDURE identifier
                 {
		    $$ = $2;
		    offset = 4;
		    pushTree( $2 );
		 }

formal_param_list : formal_param ';' formal_param_list 
                  { 
		     $$ = $1;
		     $1->data.next_param = $3;
		  }
                  | formal_param 
                  {
		     $$ = $1;
		  }
;
 
formal_param : id_list ':' mode type_name 
{
   $$ = parseParamList( $1, $3, $4 );
   idData = NULL;
}
; 

mode: IN  { $$ = mallocCopy( "in" ); }
    | OUT { $$ = mallocCopy( "out" ); }
    | IN OUT { $$ = mallocCopy( "in out" ); }
    | { $$ = mallocCopy( "in" ); }
;

declarative_part : decl_list procedure_decl_list 
;


decl_list : decl ';' decl_list 
                 | 
{
   if ( stackCtr > 2 ) {
      btree *search = searchForParent( stack[ stackCtr-1 ].name );
      search->data.offSet = offset;
      search->data.ARsize = offset;
   } else {
      mainsOff = offset;
   }
}
;

procedure_decl_list : procedure_decl ';' procedure_decl_list
                    | 
;

decl : record
     | array 
     | constant_var
     | object_var
     | exception 
     | type_dec
     | enum_type_def
;

array : TYPE identifier IS ARRAY '(' constant DOTDOT constant ')' OF type_name 
{
   int check = addSymbol( $2 );
   if ( check != -1 ) {
      btree* newArr = searchTree( $2, stack[ stackCtr-1 ].root );
      newArr->data.kind = mallocCopy( "array" );
      newArr->data.lower = $6;
      newArr->data.upper = $8;
      btree *findType = searchForParent( $11 );
      newArr->data.component_type = findType;
      int length = $8-$6+1;
      newArr->data.size = length * findType->data.size;
      newArr->data.offSet = offset-$6;
   }
}
;

enum_type_def : TYPE identifier IS '(' enum_id_list ')'
{
   eTypeArr[ eCt ] = $2;
   eCt++;
   int check = addSymbol( $2 );
   if ( check != -1 ) {
      btree *newEnum = searchTree( $2, stack[ stackCtr-1 ].root );
      newEnum->data.kind = mallocCopy( "enum" );
      newEnum->data.eList = $5;
      newEnum->data.offSet = offset;
      int count = 0;
      eNode *current = newEnum->data.eList;
      while ( current != NULL ) {
	 current->index = count;
	 count++;
	 current = current->next;
      }
      newEnum->data.upper = count;
      newEnum->data.lower = 0;
      newEnum->data.size = 1;
   }
   tempE = NULL;
}
;


enum_id_list : identifier ',' enum_id_list
{
   eNode *temp = ( eNode* )malloc( sizeof( struct eNode ) );
   temp->next = tempE;
   tempE = temp;
   temp->str = mallocCopy( $1 );
   tempE->next = ( eNode* )malloc( sizeof( struct eNode ) );
   tempE->next = $3;
   $$ = tempE;
   eNode *current = tempE;
   
}
| identifier
{
   eNode *temp = ( eNode* )malloc( sizeof( struct eNode ) );
   temp->next = tempE;
   tempE = temp;
   temp->str = mallocCopy( $1 );
  
   eNode *current = tempE;
   $$ = tempE;
}
;



constant : NUMBER    
;

record : TYPE identifier IS RECORD component_list ENDREC
;

component_list : object_var ';' component_list 
               | object_var ';'
;

type_dec : TYPE identifier IS RANGE constant DOTDOT constant 
{
   int check = addSymbol( $2 );
   if ( check != -1 ) {
      btree* newRange = searchTree( $2, stack[ stackCtr-1 ].root ); 
      newRange->data.kind = mallocCopy( "range" );
      newRange->data.lower = $5;
      newRange->data.upper = $7;
      newRange->data.size = 1;
      newRange->data.offSet = offset;
   }
}
;

type_name : identifier { $$ = $1; }
;

id_list : identifier ',' id_list 
        { 
	   idData = addName( $1, idData ); 
	   idData->next = ( struct idnode* )malloc( sizeof( struct idnode ) );
	   $$ = idData;
	   idData->next = $3; 
	}
        | identifier  
	{ 
	   idData = addName( $1, idData ); 
	   $$ = idData;
	}
;

object_var : id_list ':' type_name
{
   parseVarIdList( $1, $3 );
   idData = NULL;
}
;

constant_var : id_list ':' CONST ASSIGN constant_expr
;

constant_expr : NUMBER
;

exception : id_list ':' EXP
{
   parseExceptionList( $1 );
   idData = NULL;
}
;

statement_sequence : statement ';' statement_sequence 
| statement ';' 
;

statement : NULLWORD
          | assignment
          | procedure_call
          | loop
          | if_cond
          | raise 
          | case_statement
;


case_statement : case_begin  IS when_list END CASE
{
   PLhead = popAndPatch( toPatch[ patchT ], iCtr );
   baseN--;
   caseReg--;

}
;

case_begin : CASE expression
{
   pushPatch();
   baseN++;
   iJump[ baseN ] = patchT;
   caseReg++;
   if ( $2->inReg == 1 ) {
      caseExprReg[ caseReg ] = $2->regNum;
   } else {
      int reg = getRegisterCt();
      if ( $2->isLocal == 1 ) {
	 fprintf( outputFile, "%d r%d := contents b, %d\n", getICount(), reg, $2->offset );
      } else {
	 fprintf( outputFile, "%d r%d := contents r%d, %d\n", getICount(), reg, $2->regNum, $2->offset );
      }
      caseExprReg[ caseReg ] = reg;
   }
}
;

when_list : single_when when_list
          | single_when others_option
; 

single_when : when_choice_begin ARROW case_statement_sequence  
{
   int i = getICount();
   fprintf( outputFile, "%d pc := ?\n", i );
   toPatch[ iJump[ baseN ] ] = appendPL( i, 0, 0, toPatch[ iJump[ baseN ] ] );
   PLhead = popAndPatch( toPatch[ patchT ], i+1 );
}
;

when_choice_begin : WHEN choice_list
{

   exprNode *current = $2;
   int reg;
   int orCount = 0;
   int regJ = 0;

   while ( current != NULL ) {
      orCount++;
      current = current->next;
   }
   current = $2;
   int prevReg = -1;
   int orReg = -1;
   while ( current != NULL ) {
      if ( current->isRange == 1 ) {
	 int reg1 = getRegisterCt();
	 int reg2 = getRegisterCt();
	 fprintf( outputFile, "%d r%d := r%d <= r%d\n", getICount(), reg1, current->lowerReg, caseExprReg[ caseReg ] );	 
	 fprintf( outputFile, "%d r%d := r%d <= r%d\n", getICount(), reg2, current->upperReg, caseExprReg[ caseReg ] );
	 reg = getRegisterCt();
	 fprintf( outputFile, "%d r%d := r%d and r%d\n", getICount(), reg, reg1, reg2 );
	 
      } else {
	 if ( current->inReg == 0 ) {
	    if ( current->isLocal == 1 ) {
	       reg = getRegisterCt();
	       fprintf( outputFile, "%d r%d := contents b, %d\n", getICount(), reg, current->offset );
	    } else {
	       reg = getRegisterCt();
	       fprintf( outputFile, "%d r%d := contents r%d, %d\n", getICount(), reg, current->regNum, current->offset );
	    }
	    current->inReg = 1;
	    current->regNum = reg;
	 }

	 if ( current->inReg == 1 ) {
	    reg = getRegisterCt();
	    fprintf( outputFile, "%d r%d := r%d = r%d\n", getICount(), reg, caseExprReg[ caseReg ], current->regNum );
	 }
      }
      int i;
      if ( orCount == 1 ) {
	 if ( orReg == -1 ) {
	    i = getICount();
	    fprintf( outputFile, "%d pc := ? if not r%d\n", i, reg );
	 } else {
	    int orReg2 = getRegisterCt();
	    fprintf( outputFile, "%d r%d := r%d or r%d\n", getICount(), orReg2, orReg, reg ); 
	    i = getICount();
	    fprintf( outputFile, "%d pc := ? if not r%d\n", i, orReg2 );
	 }
	 pushPatch();
	 toPatch[ patchT ] = appendPL( i, 0, 0, toPatch[ patchT ] );
      } else {
	 if ( prevReg > -1 ) {
	    orReg = getRegisterCt();
	    fprintf( outputFile, "%d r%d := r%d or r%d\n", getICount(), orReg, reg, prevReg );
	    prevReg = orReg;
	 } else {
	    prevReg = reg;
	 }
      }
      current = current->next;
      orCount--;
   }
   exprList = NULL;
}
;

others_option : WHEN OTHERS ARROW case_statement_sequence
{
   int i = getICount();
   fprintf( outputFile, "%d pc := ?\n", i );
   toPatch[ iJump[ baseN ] ] = appendPL( i, 0, 0, toPatch[ iJump[ baseN ] ] );
}
              |
;

choice_list : choice '|' choice_list
{
   $$ = ( exprNode* )malloc( sizeof( exprNode* ) );
   exprNode *temp = ( exprNode* )malloc( sizeof( exprNode* ) );
   temp = $1;
   temp->next = exprList;
   exprList = temp;
   exprList->next = ( exprNode* )malloc( sizeof( exprNode* ) );
   exprList->next = $3;
   $$ = exprList;

}
            | choice 
{
   $$ = ( exprNode* )malloc( sizeof( exprNode* ) );
   exprNode *temp = ( exprNode* )malloc( sizeof( exprNode* ) );
   temp = $1;
   temp->next = exprList;
   exprList = temp;
   $$ = exprList;				
}

;

choice : expression 
       | expression DOTDOT expression
{
   $$ = ( exprNode* )malloc( sizeof( exprNode* ) );
   $$->isRange = 1;
   int reg;
   if ( $1->inReg == 1 ) {
      $$->lowerReg = $1->regNum;
   } else {
       reg = getRegisterCt();
      if ( $1->isLocal == 1 ) {
	 fprintf( outputFile, "%d r%d := contents b, %d\n", getICount(), reg, $1->offset );
	 $$->lowerReg = reg;
      } else {
	 fprintf( outputFile, "%d r%d := contents r%d, %d\n", getICount(), reg, $1->regNum, $1->offset );
	 $$->lowerReg = reg;
      }
   }

   if ( $3->inReg == 1 ) {
      $$->upperReg = $3->regNum;
   } else {
      reg = getRegisterCt();
      if ( $3->isLocal == 1 ) {
	 fprintf( outputFile, "%d r%d := contents b, %d\n", getICount(), reg, $3->offset );
	 $$->upperReg = reg;
      } else {
	 fprintf( outputFile, "%d r%d := contents r%d, r%d\n", getICount(), reg, $3->regNum, $3->offset );
	 $$->upperReg = reg;
      }
   }
}
;

case_statement_sequence : statement ';' case_statement_sequence
                        | statement ';'
;

assignment : identifier ASSIGN enum_op 
{
    $$ = ( exprNode* )malloc( sizeof( exprNode* ) );
   int d;
   int reg;
   btree *w = walkBack( $1, &d, reg, outputFile, 0 );
   if ( d > 0 )
      reg = registerCt;
    $$->isVar = 1;
   if ( w == NULL ) {
      printf( "ERROR: %s not declared in scope\n", $1 );
      return 1;
   } 
   
    fprintf( outputFile, "%d", getICount() );
   if ( d == 0 ) {
      fprintf( outputFile, " contents b, %d", w->data.offSet );
      $$->isLocal = 1;
   } else {
      fprintf( outputFile, " contents r%d, %d", reg, w->data.offSet );
      $$->isLocal = 0;
   }
   fprintf( outputFile, " := r%d \n", $3->regNum );

}
| identifier ASSIGN expression
{
   $$ = ( exprNode* )malloc( sizeof( exprNode* ) );
   int d;
   int reg;
   btree *w = walkBack( $1, &d, reg, outputFile, 0 );
   if ( d > 0 )
      reg = registerCt;

   $$->isVar = 1;
   if ( w == NULL ) {
      printf( "ERROR: %s not declared in scope\n", $1 );
      return 1;
   } 

   int indReg;
   if ( $3->isArray == 1 ) {
   indReg = getRegisterCt();
  
      fprintf( outputFile, "%d r%d := %d + ", getICount(), indReg, $3->offset );
      if ( $3->indexExpr->inReg == 1 ) {
	 fprintf( outputFile, "r%d\n", $3->indexExpr->regNum );
      } else {
	  if ( $3->indexExpr->isLocal == 1 ) {
	    fprintf( outputFile, "contents b, %d\n", $3->indexExpr->offset );
	 } else {
	    fprintf( outputFile, "contents r%d, %d\n", $3->indexExpr->regNum, $3->indexExpr->offset );
	 }
      }
   } 

   fprintf( outputFile, "%d", getICount() );
   if ( d == 0 ) {
      fprintf( outputFile, " contents b, %d", w->data.offSet );
      $$->isLocal = 1;
   } else {
      fprintf( outputFile, " contents r%d, %d", reg, w->data.offSet );
      $$->isLocal = 0;
   }
   fprintf( outputFile, " := " );
   

   if ( $3->isEnum == 1 ) {
      w->data.value = $3->value;
   }

   if ( $3->inReg == 1  || $3->isBool == 1 ) {
      fprintf( outputFile, "r%d\n", $3->regNum );
   } else if ( $3->isArray == 1 ) {
      if ( $3->isLocal == 1 ) {
	 fprintf( outputFile, " contents b, r%d\n", indReg ); 

      } else {
	 fprintf( outputFile, " contents r%d, r%d\n", $3->regNum, indReg );
      }
   } else {
      if ( $3->isLocal == 1 ) {
	 fprintf( outputFile, " contents b, %d\n", $3->offset );
      } else {
	  fprintf( outputFile, " contents r%d, %d\n", $3->regNum, $3->offset );
      }
   }
}
;

enum_op : identifier TICK identifier '(' enum_op ')'
{
   $$ = ( exprNode* )malloc( sizeof( exprNode* ) );
   btree *eType = searchForParent( $1 );
   int comp = strcmp( "enum", eType->data.kind );
   if ( comp != 0 ) {
      yyerror( "id needs to be of type enum\n" );
      return;
   }

   eNode *list = eType->data.eList;
   comp = strcmp( "pos", $3 );
   int comp2 = strcmp( "succ", $3 );
   int comp3 = strcmp( "pred", $3 );

   if ( comp == 0 ) {
      if ( $5->inReg == 1 ) {
	 $$->inReg = 1;
	 $$->regNum = $5->regNum;
      }

   } else if ( comp2 == 0 ) {
      if ( $5->value >= eType->data.upper ) {
	 yyerror( "successor doesn't exist\n" );
	 return;
      }
      if ( $5->inReg == 1 ) {
	 int reg = getRegisterCt();
	 fprintf( outputFile, "%d r%d := r%d + 1\n", getICount(), reg );
	 $$->inReg = 1;
	 $$->regNum = reg;
      }

   } else if ( comp3 == 0 ) {
      if ( $5->value <= eType->data.lower ) {
	 yyerror( "pred doesn't exist\n" );
	 return;
      }
      if ( $5->inReg == 1 ) {
	 int reg = getRegisterCt();
	 fprintf( outputFile, "%d r%d := r%d - 1\n", getICount(), reg );
	 $$->inReg = 1;
	 $$->regNum = reg;
      }

   } else {
      yyerror( "id needs to be an enum function\n" );
      return;
   }


}
        | identifier TICK identifier '(' identifier ')'
{
   $$ = ( exprNode* )malloc( sizeof( exprNode* ) );
   btree *eType = searchForParent( $1 );
   int comp = strcmp( "enum", eType->data.kind );
   if ( comp != 0 ) {
      yyerror( "id needs to be of type enum\n" );
      return;
   }

   eNode *list = eType->data.eList;
   comp = strcmp( "pos", $3 );
   int comp2 = strcmp( "succ", $3 );
   int comp3 = strcmp( "pred", $3 );

   eNode *current = list;
   int val = -1;
   int nameComp;
   while ( current != NULL ) {
      nameComp = strcmp( $5, current->str );
      if ( nameComp == 0 ) {
	 val = current->index;
	 break;
      }
      current = current->next;
   }
  

   if ( comp == 0 ) {
     
      if ( val == -1 ) {
	 int reg;
	 int d;
	 btree *w = walkBack( $5, &d, reg, outputFile, 0 );
	 int tComp = strcmp( "enum", w->data.kind );
	 if ( tComp != 0 ) {
	    yyerror( "id not of type enum\n" );
	    return;
	 }
	 $$->value = w->data.value;
	 int reg2 = getRegisterCt();

	 fprintf( outputFile, "%d r%d := ", getICount(), reg2 );
	 if ( d == 0 ) {
	    fprintf( outputFile, "contents b, %d\n", w->data.offSet );
	 } else {
	    fprintf( outputFile, "contents r%d, %d\n", reg, w->data.offSet );
	 }

	 $$->inReg = 1;
	 $$->regNum = reg2;
      } else {

	 int reg = getRegisterCt();
	 fprintf( outputFile, "%d r%d := %d\n", getICount(), reg, val );
	 $$->inReg = 1;
	 $$->regNum = reg;
	 $$->value = val;
      }

   } else if ( comp2 == 0 ) {
      if ( val == -1 ) {
	 int reg;
	 int d;
	 btree *w = walkBack( $5, &d, reg, outputFile, 0 );
	 int tComp = strcmp( "enum", w->data.kind );
	 if ( tComp != 0 ) {
	    yyerror( "id not of type enum\n" );
	    return;
	 }
	
	 
	 if ( w->data.value >= w->data.parent_type->data.upper ) {
	    yyerror( "successor does not exist\n" );
	    return;
	 }
	 $$->value = w->data.value + 1;
	 int reg2 = getRegisterCt();

	 fprintf( outputFile, "%d r%d := ", getICount(), reg2 );
	 if ( d == 0 ) {
	    fprintf( outputFile, "contents b, %d + 1\n", w->data.offSet );
	 } else {
	    fprintf( outputFile, "contents r%d, %d + 1\n", reg, w->data.offSet );
	 }

	 $$->inReg = 1;
	 $$->regNum = reg2;
      } else {

	 if ( val >= eType->data.upper ) {
	    yyerror( "successor does not exist\n" );
	    return;
	 }
	 int reg = getRegisterCt();
	 fprintf( outputFile, "%d r%d := %d + 1\n", getICount(), reg, val );
	 $$->inReg = 1;
	 $$->regNum = reg;
	 $$->value = val + 1;

      }

   } else if ( comp3 == 0 ) {
      if ( val == -1 ) {
	 int reg;
	 int d;
	 btree *w = walkBack( $5, &d, reg, outputFile, 0 );
	 int tComp = strcmp( "enum", w->data.kind );
	 if ( tComp != 0 ) {
	    yyerror( "id not of type enum\n" );
	    return;
	 }
	
	 
	 if ( w->data.value <= w->data.parent_type->data.lower ) {
	    yyerror( "pred does not exist\n" );
	    return;
	 }

	 int reg2 = getRegisterCt();

	 fprintf( outputFile, "%d r%d := ", getICount(), reg2 );
	 if ( d == 0 ) {
	    fprintf( outputFile, "contents b, %d - 1\n", w->data.offSet );
	 } else {
	    fprintf( outputFile, "contents r%d, %d - 1\n", reg, w->data.offSet );
	 }

	 $$->inReg = 1;
	 $$->regNum = reg2;
	 $$->value = w->data.value - 1;
      } else {

	 if ( val <= eType->data.lower ) {
	    yyerror( "pred does not exist\n" );
	    return;
	 }
	 int reg = getRegisterCt();
	 fprintf( outputFile, "%d r%d := %d - 1\n", getICount(), reg, val );
	 $$->inReg = 1;
	 $$->regNum = reg;
	 $$->value = val - 1;
      }
      
   } else {
      yyerror( "not a enum function!\n");
   }
			    

}

;


procedure_call : identifier optional_params optional_assign 
{
   $$ = ( exprNode* )malloc( sizeof( exprNode* ) );
   if ( stackCtr > 0 ) {
     
      btree *fnTree = searchForParent( $1 );
      
      int comp = strcmp( fnTree->data.kind, "read_routine" );
      int comp2 = strcmp( fnTree->data.kind, "write_routine" );

      int distance;
      int currentR = getRegisterCt();
      registerCt -= 1;
      int opt = 0;
      if ( $3 == NULL ) {
	 opt = 1;
      }
  
      if ( comp == 0 ) {
	 if ( $2 == NULL ) {
	    yyerror( "ID MISSING PARAMS\n" );
	    return;
	 } else {
	    emitRead( outputFile, $2 );
	 }
      } else if ( comp2 == 0 ) {
	 if ( $2 == NULL ) {
	    yyerror( "ID MISSING PARAMS\n" );
	    return;
	 } else {
	    emitWrite( outputFile, $2 );
	 }
      } else {
	 btree *w = walkBack( $1, &distance, currentR, outputFile, opt );
	 if ( w == NULL ) {
	    printf( "NOT FOUND!\n" );
	    return;
	 }
	 int arr = strcmp ( w->data.kind, "array" );
	 int proc = strcmp ( w->data.kind, "procedure" );
	 if ( arr == 0 ) {
	

	    if ( distance == 0 ) {
	       $$->isLocal = 1;
	       $$->offset = w->data.offSet;
	       $$->isBool = 0;
	       $$->indexExpr = ( exprNode* )malloc( sizeof( exprNode* ) );
	       $$->indexExpr = $2;
	    } else {
	       $$->isLocal = 0;
	       $$->offset = w->data.offSet;
	       $$->isBool = 0;
	       $$->indexExpr = ( exprNode* )malloc( sizeof( exprNode* ) );
	       $$->indexExpr = $2;
	       $$->regNum = registerCt;
	    }
	    int reg;
	    int indReg;
	    int indReg2;
	    indReg = getRegisterCt();
	    fprintf( outputFile, "%d r%d := %d + ", getICount(), indReg, $$->offset );
	
	    if ( $$->indexExpr->isLocal == 1 ) {
	       fprintf( outputFile, "contents b, %d\n", $$->indexExpr->offset );
	    } else if ( $$->indexExpr->inReg == 1 ) {
	       fprintf( outputFile, "r%d\n", $$->indexExpr->regNum );
	    } else {
	       fprintf( outputFile, "contents r%d, %d\n", $$->indexExpr->regNum, $$->indexExpr->offset );
	    }
	 

	    if ( $3->isArray == 1 ) {
	       indReg2 = getRegisterCt();
	       fprintf( outputFile, "%d r%d := %d + ", getICount(), indReg2, $3->offset );
	       if ( $3->indexExpr->inReg == 1 ) {
		  fprintf( outputFile, "r%d\n", $3->indexExpr->regNum );
	       } else {
		  if ( $3->indexExpr->isLocal == 1 ) {
		     fprintf( outputFile, "contents b, %d\n", $3->indexExpr->offset );
		  }  else {
		     fprintf( outputFile, "contents r%d, %d\n", $3->indexExpr->regNum, $3->indexExpr->offset );
		  }
	       }
	    }
	 
	    if ( $$->isLocal == 1 ) {
	       fprintf( outputFile, "%d contents b, r%d := ", getICount(), indReg );
	    } else {
	       fprintf( outputFile, "%d contents r%d, r%d := ", getICount(), $$->regNum, indReg );
	    }

	    if ( $3->inReg == 1 ) {
	       fprintf( outputFile, "r%d\n", $3->regNum );
	    } else if ( $3->isArray == 1 ) { 
	       if ( $3->isLocal == 1 ) {
		  fprintf( outputFile, " contents b, r%d\n", indReg2 );
	       } else {
		  fprintf( outputFile, " contents r%d, r%d\n", $3->regNum, indReg2 );
	       }
	    } else {
	       if ( $3->isLocal == 1 ) {
		  fprintf( outputFile, " contents b, %d\n", $3->offset );
	       } else {
		  fprintf( outputFile, " contents r%d, %d\n", $3->regNum, $3->offset );
	       }
	    }

	 } else if ( proc == 0 ) {
	    

	    if ( distance == 0 ) {
	       $$->isLocal = 1;
	       $$->offset = w->data.offSet;
	       $$->isBool = 0;
	       $$->isParamProc = 1;
	    } else {
	       $$->isLocal = 0;
	       $$->offset = w->data.offSet;
	       $$->isBool = 0;
	       $$->regNum = registerCt;
	       $$->isParamProc = 1;
          }

	    int ARsize = fnTree->data.ARsize;
	    //build AR
	    int aReg = getRegisterCt();
	    fprintf( outputFile, "%d r%d := b\n", getICount(), aReg );
	    fprintf( outputFile, "%d b := contents r%d, 0\n", getICount(), aReg );
	    fprintf( outputFile, "%d contents b, 3 := r%d\n", getICount(), aReg );
	    fprintf( outputFile, "%d contents b, 2 := r%d\n", getICount(), aReg );
	    int r = getRegisterCt();
	    fprintf( outputFile, "%d r%d := %d\n", getICount(), r, ARsize );
	    fprintf( outputFile, "%d contents b, 0 := b + r%d\n", getICount(), r );

	    btree *currentP = w->data.next_param;
	    
	    exprNode *currentE = $2;
	    int inC;
	    int inOC; 
	    int offReg;
	    int addReg;
	    while ( currentP != NULL ) { 
	       inC = strcmp( currentP->data.mode, "in" );
	       inOC = strcmp( currentP->data.mode, "in out" );
	       if ( ( inC == 0 ) || ( inOC == 0 ) ) {
		  fprintf( outputFile, "%d contents b, %d := ", getICount(), currentP->data.offSet );
		  if ( currentE->inReg == 1 ) {
		     fprintf( outputFile, "r%d\n", currentE->regNum );
		  } else {
		     if ( currentE->isLocal == 1 ) {
			fprintf( outputFile, "contents r%d, %d\n", aReg, currentE->offset );
		     } else {
			fprintf( outputFile, "contents r%d, %d\n", currentE->regNum, currentE->offset );
		     }
		     
		     offReg = getRegisterCt();
		     addReg = getRegisterCt();
		     fprintf( outputFile, "%d r%d := %d\n", getICount(), offReg, currentE->offset );
		     if ( currentE->isLocal == 1 ) {
			fprintf( outputFile, "%d r%d := r%d + r%d\n", getICount(), addReg, aReg, offReg );
		     } else {
			fprintf( outputFile, "%d r%d := r%d + r%d\n", getICount(), addReg, currentE->regNum, offReg );
		     }
		     int off = currentP->data.offSet;
		     if ( inOC == 0 ) {
			off += currentP->data.size;
		     }
		     fprintf( outputFile, "%d contents b, %d := r%d\n", getICount(), off, addReg );
		  }

	       }
	       currentP = currentP->data.next_param;
	       currentE = currentE->next;
	    }
	    int rAdd = getICount();
	    currentR = getRegisterCt();
	    fprintf( outputFile, "%d r%d := %d\n", rAdd, currentR, rAdd+3 );
	    fprintf( outputFile, "%d contents b, 1 := r%d\n", getICount(), currentR );
	    fprintf( outputFile, "%d pc := %d\n", getICount(), w->data.iCount ); 

	 }
      }
   }
}
| identifier

{
   if ( stackCtr > 0 ) {
     
      btree *fnTree = searchForParent( $1 );
      int ARsize;
      if ( fnTree != NULL ) {
	 ARsize = fnTree->data.ARsize;
      } else {
	 printf( "ERROR: id: %s is not declared in tree\n", $1 );
	 return;
      }

      int currentR = getRegisterCt();
      fprintf( outputFile, "%d r%d := b\n", getICount(), currentR );
      fprintf( outputFile, "%d b := contents r%d, 0\n", getICount(), currentR );
      fprintf( outputFile, "%d contents b, 3 := r%d\n", getICount(), currentR );


      int distance;
      registerCt -= 1;
      btree *walk = walkBack( $1, &distance, currentR, outputFile, 1 );
      if ( walk == NULL ) {
	 printf( "NOT FOUND!\n" );
	 return;
      }
      if ( distance == 0 ) {
	 fprintf( outputFile, "%d contents b, 2 := r%d\n", getICount(), currentR );
      }
      currentR = getRegisterCt();
      fprintf( outputFile, "%d r%d := %d\n", getICount(), currentR, ARsize );
      fprintf( outputFile, "%d contents b, 0 := b + r%d\n", getICount(), currentR );
      int rAdd = getICount();
      currentR = getRegisterCt();
      fprintf( outputFile, "%d r%d := %d\n", rAdd, currentR, rAdd+3 );
      fprintf( outputFile, "%d contents b, 1 := r%d\n", getICount(), currentR );
      fprintf( outputFile, "%d pc := %d\n", getICount(), walk->data.iCount );
      int i = getICount();
      fprintf( outputFile, "%d pc := ? if r1\n", i );
      raise_list = appendPL( i, 0, 0, raise_list );
      
   }


}
   
;

optional_params : '(' function_params ')' 
{
   $$ = ( exprNode* )malloc( sizeof( exprNode* ) );
   $$ = $2;
}
;

function_params : expression ',' function_params 
{
   $$ = ( exprNode* )malloc( sizeof( exprNode* ) );
   $$ = $1;
   $$->next = ( exprNode* )malloc( sizeof( exprNode* ) );
   $$->next = $3;
}
| expression
{
   $$ = ( exprNode* )malloc( sizeof( exprNode* ) );
   $$ = $1;
}
;

optional_assign : ASSIGN expression 
      { 
	 $$ = ( exprNode* )malloc( sizeof( exprNode* ) );
	 $$ = $2;
	 $$->isParamProc = 0;
      }
| 
{
   $$ = ( exprNode* )malloc( sizeof( exprNode* ) );
   $$->isParamProc = 1;

}
;


loop : beg_loop 

         loop_body 
ENDLOOP 
{
   int i = getICount();
   fprintf( outputFile, "%d pc := %d\n", i, $1 );
   PLhead = popAndPatch( toPatch[ patchT ], i+1 );
} 
;


beg_loop: LOOP
{ 
   $$ = iCtr; 
   pushPatch();
}
;

loop_body : statement ';' loop_body 
          | exit loop_body
          | exit
          | statement ';'
;

exit : EXITWHEN condition ';'
{
   int i = getICount();
   fprintf( outputFile, "%d pc := ? if r%d\n", i, $2->regNum );
   toPatch[ patchT ] = appendPL( i, 0, 0, toPatch[ patchT ] );
}
     | EXIT ';'
{
   int i = getICount();
   fprintf( outputFile, "%d pc := ?\n", i );
   toPatch[ patchT ] = appendPL( i, 0, 0, toPatch[ patchT ] );
}
;

if_cond : if_beg cond_statements else_if_list else_cond ENDIF 
{
   baseN--;
   printPL( PLhead, 1 );
}
;

if_beg: IF condition THEN
{
   pushPatch();
   baseN++;
   iJump[ baseN ] = patchT;
   
   int i = getICount();
   pushPatch();
   toPatch[ patchT ] = appendPL( i, 0, 0, toPatch[ patchT ] );   
   fprintf( outputFile, "%d pc := ? if not r%d\n", i, $2->regNum );
}
;

cond_statements: statement ';' cond_statements
               | statement ';'
{
   int i = getICount();  
   int base = iJump[ baseN ];
   toPatch[ base ] = appendPL( i, 0, 0, toPatch[ base ] );
   fprintf( outputFile, "%d pc := ?\n", i );
   PLhead = popAndPatch( toPatch[ patchT ], i+1 );
   
   
}
;

else_if_list : else_if else_if_list
             | 
;

else_if : ELSEIF condition
{
   int i = getICount();
   pushPatch();
   fprintf( outputFile, "%d pc := ? if not r%d\n", i, $2->regNum );
   toPatch[ patchT ] = appendPL( i, 0, 0, toPatch[ patchT ] );
}

THEN cond_statements
;


else_cond : ELSE cond_statements
         |
{
    PLhead = popAndPatch( toPatch[ patchT ], iCtr );
}
;

raise : RAISE identifier
{
   int d;
   int reg = registerCt;
   btree *w = walkBack( $2, &d, reg, outputFile, 0 );
   //registerCt--;
   if ( w == NULL ) {
      yyerror( "ERROR EXCEPTION NOT FOUND" );
   }

   int c = strcmp( w->data.kind, "exception" );
   if ( c != 0 ) {
      yyerror( "ERROR NOT AN EXCEPTION KIND\n" );
   } else {
      fprintf( outputFile, "%d r1 := %d\n", getICount(), w->data.iCount );
      int i = getICount();
      fprintf( outputFile, "%d pc := ?\n", i );
      raise_list = appendPL( i, 0, 0, raise_list );
   }
}
| RAISE
{
   if ( in_exception_part == 0 ) {
      yyerror( "INVALID RAISE\n" );
      return;
   }
   int i = getICount();
   fprintf( outputFile, "%d pc := ?\n", i );
   raise_list = appendPL( i, 0, 0, raise_list );
}
      
;

expression : relation 
           | expression boolean_op relation
{
   if ( ( $1->isVar == 0 ) && ( $3->isVar == 0 ) ) {
      if ( $2 == 0 ) {
	 $$->value = $1 && $2;
      } else {
	 $$->value = $1 || $2;
      }
      $$->isVar = 0;
   }
   int reg;
   int indReg;
   if ( ( $1->isArray == 1 ) && ( $3->isArray ) ) {
      int indReg2;
	 indReg = getRegisterCt();
	 fprintf( outputFile, "%d r%d := %d + ", getICount(), indReg, $1->offset );
	 if ( $1->indexExpr->inReg == 1 ) {
	    fprintf( outputFile, "r%d\n", $1->indexExpr->regNum );
	 } else {
	    if ( $1->indexExpr->isLocal == 1 ) {
	       fprintf( outputFile, "contents b, %d\n", $1->indexExpr->offset );
	    }  else if ( $1->indexExpr->inReg == 1 ) {
	       fprintf( outputFile, "r%d\n", $$->indexExpr->regNum );
	    }else {
	       fprintf( outputFile, "contents r%d, %d\n", $1->indexExpr->regNum, $1->indexExpr->offset );
	    }
	 }
		     
      

	 indReg2 = getRegisterCt();
	 fprintf( outputFile, "%d r%d := %d + ", getICount(), indReg, $3->offset );
	 if ( $3->indexExpr->inReg == 1 ) {
	    fprintf( outputFile, "r%d\n", $3->indexExpr->regNum );
	 } else {
	    if ( $3->indexExpr->isLocal == 1 ) {
	       fprintf( outputFile, "contents b, %d\n", $3->indexExpr->offset );
	    } else {
	       fprintf( outputFile, "contents r%d, %d\n", $3->indexExpr->regNum, $3->indexExpr->offset );
	    }
	 }
	 reg = getRegisterCt();
	 if ( $1->isLocal == 1 ) {
	    fprintf( outputFile, "%d r%d := contents b, r%d", getICount(), reg, indReg );
	 } else {
	    fprintf( outputFile, "%d r%d := contents r%d, r%d", getICount, reg, $1->regNum, indReg );
	 }
	 if ( $2 == 1 ) 
	    fprintf( outputFile, " and " );
	 else 
	    fprintf( outputFile, " or " );
	
	 if ( $3->isLocal == 1 ) {
	    fprintf( outputFile, "contents b, r%d\n", indReg2 );
	 } else {
	    fprintf( outputFile, "contents r%d, r%d\n", $1->regNum, indReg2 );
	 }
	 
      
		     
   } else if ( $1->isArray == 1 ) {
      indReg = getRegisterCt();
      fprintf( outputFile, "%d r%d := %d + ", getICount(), indReg, $1->offset );
      if ( $1->indexExpr->inReg == 1 ) {
	 fprintf( outputFile, "r%d\n", $1->indexExpr->regNum );
      } else {
	 if ( $1->indexExpr->isLocal == 1 ) {
	    fprintf( outputFile, "contents b, %d\n", $1->indexExpr->offset );
	 } else {
	    fprintf( outputFile, "contents r%d, %d\n", $1->indexExpr->regNum, $1->indexExpr->offset );
	 }
      }
      reg = getRegisterCt();
      if ( $1->isLocal == 1 ) {
	 fprintf( outputFile, "%d r%d := contents b, r%d", getICount(), reg, indReg );
      } else {
	 fprintf( outputFile, "%d r%d := contents r%d, r%d", getICount, reg, $1->regNum, indReg );
      }
      
      if ( $2 == 1 ) 
	 fprintf( outputFile, " and " );
      else 
	 fprintf( outputFile, " or " );

      if  ( $3->inReg == 1 )  {
	 fprintf( outputFile, "r%d\n", $3->regNum );
      } else {
	 if ( $3->isLocal == 1 ) {
	    fprintf( outputFile, " contents b, %d\n", $3->offset );
	 } else {
	    fprintf( outputFile, " contents r%d, %d\n", $3->regNum, $3->offset ); 
	 }
      }
   } else if ( $3->isArray == 1 ) {
      if ( $3->isLocal == 1 ) {
	 indReg = getRegisterCt();
	 fprintf( outputFile, "%d r%d := %d + ", getICount(), indReg, $3->offset );
	 if ( $3->indexExpr->inReg == 1 ) {
	    fprintf( outputFile, "r%d\n", $3->indexExpr->regNum );
	 } else {
	    if ( $3->indexExpr->isLocal == 1 ) {
	       fprintf( outputFile, "contents b, %d\n", $3->indexExpr->offset );
	    } else {
	       fprintf( outputFile, "contents r%d, %d\n", $3->indexExpr->regNum, $3->indexExpr->offset );
	    }
	 }
      }
      reg = getRegisterCt();
      fprintf( outputFile, "%d r%d := ", getICount(), reg );
      if  ( $1->inReg == 1 )  {
	 fprintf( outputFile, "r%d", $1->regNum );
      } else {
	 if ( $1->isLocal == 1 ) {
	    fprintf( outputFile, " contents b, %d", $1->offset );
	 } else {
	    fprintf( outputFile, " contents r%d, %d", $1->regNum, $1->offset ); 
	 }
      }

      if ( $2 == 1 ) 
	 fprintf( outputFile, " and " );
      else 
	 fprintf( outputFile, " or " );

      if ( $1->isLocal == 1 ) {
	 fprintf( outputFile, "contents b, r%d\n", indReg );
      } else {
	 fprintf( outputFile, "contents r%d, r%d\n", $1->regNum, indReg );
      }

   } else {

      reg = getRegisterCt();
      fprintf( outputFile, "%d r%d := ", getICount(), reg );
	    
      if ( $1->inReg == 1 ) {
	 fprintf( outputFile, "r%d", $1->regNum );
      } else {
	 if ( $1->isLocal == 1 ) {
	    fprintf( outputFile, " contents b, %d", $1->offset );
	 } else {
	    fprintf( outputFile, " contents r%d, %d", $1->regNum, $1->offset ); 
	 }
      }

      if ( $2 == 0 ) {
	 fprintf( outputFile, " and " );
      } else {
	 fprintf( outputFile, " or " );
      }
      if ( $3->isVar == 0  || $3->inReg == 1 ) {
	 fprintf( outputFile, "r%d\n", $3->regNum );
      } else {
	 if ( $3->isLocal == 1 ) {
	    fprintf( outputFile, " contents b, %d\n", $3->offset );
	 } else {
	    fprintf( outputFile, " contents r%d, %d\n", $3->regNum, $3->offset ); 
	 }
      }
   }
   $$->inReg = 1;
   $$->regNum = reg;
}

 
;

relation : simple_expr 
         | relation relational_op simple_expr
         {
	     $$ = ( exprNode* )malloc( sizeof( exprNode* ) );
	     
	     
	     int reg;
	     if ( $1->inReg == 0 ) {
		reg = getRegisterCt();
		if ( $1->isLocal == 1 && $1->isArray == 0 ) {
		   fprintf( outputFile, "%d r%d := contents b, %d\n", getICount(), reg, $1->offset );
		} else if ( $1->isArray == 1 ) {
		      int indReg = getRegisterCt();
		      fprintf( outputFile, "%d r%d := %d + ", getICount(), indReg, $1->offset );
		      if ( $1->indexExpr->inReg == 1 ) {
			 fprintf( outputFile, "r%d\n", $1->indexExpr->regNum );
		      } else {
			 if ( $1->indexExpr->isLocal == 1 ) {
			    fprintf( outputFile, "contents b, %d\n", $1->indexExpr->offset );
			 } else {
			    fprintf( outputFile, "contents r%d, %d\n", $1->indexExpr->regNum, $1->indexExpr->offset );
			 }
		      }
		      reg = getRegisterCt();
		      if ( $1->isLocal == 1 ) {
			 fprintf( outputFile, "%d r%d := contents b, r%d\n", getICount(), reg, indReg );
		      } else {
			 fprintf( outputFile, "%d r%d := contents r%d, r%d\n", getICount(), reg, indReg );
		      }
		   

		} else {
		   fprintf( outputFile, "%d r%d := contents r%d, %d\n", getICount(), reg, $1->regNum, $1->offset ); 
		}
		$1->regNum = reg;
	     }

	     if ( $3->inReg == 0 ) {
		reg = getRegisterCt();
		if ( $3->isLocal == 1 ) {
		   fprintf( outputFile, "%d r%d := contents b, %d\n", getICount(), reg, $3->offset );
		} else if ( $3->isArray == 1 ) {
		   if ( $3->isLocal == 1 ) {
		      int indReg = getRegisterCt();
		      fprintf( outputFile, "%d r%d := %d + ", getICount(), indReg, $3->offset );
		      if ( $3->indexExpr->inReg == 1 ) {
			 fprintf( outputFile, "r%d\n", $3->indexExpr->regNum );
		      } else {
			 if ( $3->indexExpr->isLocal == 1 ) {
			    fprintf( outputFile, "contents b, %d\n", $3->indexExpr->offset );
			 } else {
			    fprintf( outputFile, "contents r%d, %d\n", $3->indexExpr->regNum, $3->indexExpr->offset );
			 }
		      }
		      reg = getRegisterCt();
		      if ( $3->isLocal == 1 ) {
			 fprintf( outputFile, "%d r%d := contents b, r%d", getICount(), reg, indReg );
		      } else {
			 fprintf( outputFile, "%d r%d := contents r%d, r%d", getICount, reg, $3->regNum, indReg );
		      }
		   }

		}else {
		   fprintf( outputFile, "%d r%d := contents r%d, %d\n", getICount(), reg, $3->regNum, $3->offset ); 
		}
		$3->regNum = reg;
	     }

	     reg = getRegisterCt();
	     fprintf( outputFile, "%d r%d := ", getICount(), reg );
	    
	     if ( $2 == 1 ) {
		fprintf( outputFile, "r%d = r%d\n", $1->regNum, $3->regNum );
	     } else if ( $2 == 2 ) {
		fprintf( outputFile, "r%d /= r%d\n", $1->regNum, $3->regNum );
	     } else if ( $2 == 3 ) {
		fprintf( outputFile, "r%d <= r%d\n", $1->regNum, $3->regNum );
	     } else if ( $2 == 4 ) {
		fprintf( outputFile, "r%d <= r%d\n", $3->regNum, $1->regNum );
	     } else if ( $2 == 5 ) {
		fprintf( outputFile, "r%d < r%d\n", $3->regNum, $1->regNum );
	     } else if ( $2 == 6 ) {
		fprintf( outputFile, "r%d < r%d\n", $1->regNum, $3->regNum );
	     } 
	     $$->regNum = reg;
	     $$->inReg = 1;
	 }
;

simple_expr : '-' term 
{ 
   $$ = ( exprNode* )malloc( sizeof( exprNode* ) );
   int neg1 = getRegisterCt();
   fprintf( outputFile, "%d r%d := -1\n", getICount(), neg1 );
   int reg;
   if ( $2->isVar == 0 ) {
      $$->value = $2->value * -1;
      $$->isVar = 0;
      if ( $2->inReg == 0 ) {
	 reg = getRegisterCt();
	 fprintf( outputFile, "%d r%d := %d\n", getICount(), reg, $2->value );
	 $2->regNum = reg;
	 $2->inReg = 1;
      }
      reg = getRegisterCt();
      fprintf( outputFile, "%d r%d := r%d * r%d\n", getICount(), reg, neg1, $2->regNum );
		  
   } else if ( $2->isArray == 1 ) {
      int indReg = getRegisterCt();
      fprintf( outputFile, "%d r%d := %d + ", getICount(), indReg, $2->offset );
      if ( $2->indexExpr->inReg == 1 ) {
	 fprintf( outputFile, "r%d\n", $2->indexExpr->regNum );
      } else {
	 if ( $2->indexExpr->isLocal == 1 ) {
	    fprintf( outputFile, "contents b, %d\n", $2->indexExpr->offset );
	 } else {
	    fprintf( outputFile, "contents r%d, %d\n", $2->indexExpr->regNum, $2->indexExpr->offset );
	 }
      }
      reg = getRegisterCt();
      if ( $2->isLocal == 1 ) {
	 fprintf( outputFile, "%d r%d := r%d * contents b, r%d\n", getICount(), reg, neg1, indReg );
      } else {
	 fprintf( outputFile, "%d r%d := r%d * contents r%d, r%d\n", getICount(), reg, neg1, indReg );
      }
      

   } else {
      reg = getRegisterCt();
      $$->regNum = reg;
      fprintf( outputFile, "%d r%d := r%d * ", getICount(), reg, neg1 );
         $$->inReg = 1;
	 $$->regNum = reg;
      if ( $2->isLocal == 1 ) {
	 fprintf( outputFile, " contents b, %d\n", $2->offset );
      } else {
	 fprintf( outputFile, " contents r%d, %d\n", $2->regNum, $2->offset ); 
      }
   }
   $$->inReg = 1;
   $$->regNum = reg;
}
            | term  
            | simple_expr adding_op term 
	    {
	       $$ = ( exprNode* )malloc( sizeof( exprNode* ) );
		  
	       if ( ( $1->isVar == 0 ) && ( $3->isVar == 0 ) ) {
		  if ( $2 == 1 )
		     $$->value = $1->value + $3->value;
		  else
		     $$->value = $1->value - $3->value;
		  $$->isVar = 0;
		  
	       } 
	       
	       int reg;
	       int indReg;
	       if ( ( $1->isArray == 1 ) && ( $3->isArray ) ) {
		  int indReg2;
		  if ( $1->isLocal == 1 ) {
		     indReg = getRegisterCt();
		     fprintf( outputFile, "%d r%d := %d + ", getICount(), indReg, $1->offset );
		     if ( $1->indexExpr->inReg == 1 ) {
			fprintf( outputFile, "r%d\n", $1->indexExpr->regNum );
		     } else {
			if ( $1->indexExpr->isLocal == 1 ) {
			   fprintf( outputFile, "contents b, %d\n", $1->indexExpr->offset );
			} else {
			   fprintf( outputFile, "contents r%d, %d\n", $1->indexExpr->regNum, $1->indexExpr->offset );
			}
		     }
		     
		  }

		  indReg2 = getRegisterCt();
		  fprintf( outputFile, "%d r%d := %d + ", getICount(), indReg, $3->offset );
		  if ( $3->indexExpr->inReg == 1 ) {
		     fprintf( outputFile, "r%d\n", $3->indexExpr->regNum );
		  } else {
		     if ( $3->indexExpr->isLocal == 1 ) {
			fprintf( outputFile, "contents b, %d\n", $3->indexExpr->offset );
		     } else {
			fprintf( outputFile, "contents r%d, %d\n", $3->indexExpr->regNum, $3->indexExpr->offset );
		     }
		  }
		  reg = getRegisterCt();
		  if ( $1->isLocal == 1 ) {
		     fprintf( outputFile, "%d r%d := contents b, r%d", getICount(), reg, indReg );
		  } else {
		     fprintf( outputFile, "%d r%d := contents r%d, r%d", getICount, reg, $1->regNum, indReg );
		  }
		  if ( $2 == 1 ) 
		     fprintf( outputFile, " + " );
		  else 
		     fprintf( outputFile, " - " );

		  if ( $3->isLocal == 1 ) {
		     fprintf( outputFile, "contents b, r%d\n", indReg2 );
		  } else {
		     fprintf( outputFile, "contents r%d, r%d\n", $3->regNum, indReg2 );
		  }			

	       } else if ( $1->isArray == 1 ) {
		  indReg = getRegisterCt();
		  fprintf( outputFile, "%d r%d := %d + ", getICount(), indReg, $1->offset );
		  if ( $1->indexExpr->inReg == 1 ) {
		     fprintf( outputFile, "r%d\n", $1->indexExpr->regNum );
		  } else {
		     if ( $1->indexExpr->isLocal == 1 ) {
			fprintf( outputFile, "contents b, %d\n", $1->indexExpr->offset );
		     } else {
			fprintf( outputFile, "contents r%d, %d\n", $1->indexExpr->regNum, $1->indexExpr->offset );
		     }
		  }
		  reg = getRegisterCt();
		  if ( $1->isLocal == 1 ) {
		     fprintf( outputFile, "%d r%d := contents b, r%d", getICount(), reg, indReg );
		  } else {
		     fprintf( outputFile, "%d r%d := contents r%d, r%d", getICount, reg, $1->regNum, indReg );
		  }
		  
		  if ( $2 == 1 ) 
		     fprintf( outputFile, " + " );
		  else 
		     fprintf( outputFile, " - " );

		  if  ( $3->inReg == 1 )  {
		     fprintf( outputFile, "r%d\n", $3->regNum );
		  } else {
		     if ( $3->isLocal == 1 ) {
			fprintf( outputFile, " contents b, %d\n", $3->offset );
		     } else {
			fprintf( outputFile, " contents r%d, %d\n", $3->regNum, $3->offset ); 
		     }
		  }
	       } else if ( $3->isArray == 1 ) {
		     indReg = getRegisterCt();
		     fprintf( outputFile, "%d r%d := %d + ", getICount(), indReg, $3->offset );
		     if ( $3->indexExpr->inReg == 1 ) {
			fprintf( outputFile, "r%d\n", $3->indexExpr->regNum );
		     } else {
			if ( $3->indexExpr->isLocal == 1 ) {
			   fprintf( outputFile, "contents b, %d\n", $3->indexExpr->offset );
			} else {
			   fprintf( outputFile, "contents r%d, %d\n", $3->indexExpr->regNum, $3->indexExpr->offset );
			}
		     }
		  
		  reg = getRegisterCt();
		  fprintf( outputFile, "%d r%d := ", getICount(), reg );
		  if  ( $1->inReg == 1 )  {
		     fprintf( outputFile, "r%d", $1->regNum );
		  } else {
		     if ( $1->isLocal == 1 ) {
			fprintf( outputFile, " contents b, %d", $1->offset );
		     } else {
			fprintf( outputFile, " contents r%d, %d", $1->regNum, $1->offset ); 
		     }
		  }

		  if ( $2 == 1 ) 
		     fprintf( outputFile, " + " );
		  else 
		     fprintf( outputFile, " - " );

		  if ( $3->isLocal == 1 ) {
		     fprintf( outputFile, "%d r%d := contents b, r%d\n", getICount(), reg, indReg );
		  } else {
		     fprintf( outputFile, "%d r%d := contents r%d, r%d\n", getICount, reg, $3->regNum, indReg );
		  }

	       } else {
		  

		  if  ( $1->inReg == 1 ) {
		     reg = getRegisterCt();
		     fprintf( outputFile, "%d r%d := ", getICount(), reg );
		     fprintf( outputFile, "r%d ", $1->regNum );
		  } else {
		     reg = getRegisterCt();
		     fprintf( outputFile, "%d r%d := ", getICount(), reg );
		     if ( $1->isLocal == 1 ) {
			fprintf( outputFile, " contents b, %d", $1->offset );
		     } else {
			fprintf( outputFile, " contents r%d, %d", $1->regNum, $1->offset ); 
		     }
		  }
		  if ( $2 == 1 ) 
		     fprintf( outputFile, " + " );
		  else 
		     fprintf( outputFile, " - " );
	       

		  if  ( $3->inReg == 1 )  {
		     fprintf( outputFile, "r%d\n", $3->regNum );
		  }  else {
		     if ( $3->isLocal == 1 ) {
			fprintf( outputFile, " contents b, %d\n", $3->offset );
		     } else {
			fprintf( outputFile, " contents r%d, %d\n", $3->regNum, $3->offset ); 
		     }
		  }

	       }
	       $$->regNum = reg;
	       $$->inReg = 1;
	    }
   ;

 term : factor 
      | term multiplying_op factor
 {
    $$ = ( exprNode* )malloc( sizeof( exprNode* ) );
	
    if ( ( $1->isVar == 0 ) && ( $3->isVar == 0 ) ) {
       if ( $2 == 1 )
	  $$->value = $1->value * $3->value;
       else 
	  $$->value = $1->value / $3->value;
       $$->isVar = 0;
    } 

    int reg;
    int indReg;
    if ( ( $1->isArray == 1 ) && ( $3->isArray ) ) {
       int indReg2;
     
       indReg = getRegisterCt();
       fprintf( outputFile, "%d r%d := %d + ", getICount(), indReg, $1->offset );
       if ( $1->indexExpr->inReg == 1 ) {
	  fprintf( outputFile, "r%d\n", $1->indexExpr->regNum );
       } else {
	  if ( $1->indexExpr->isLocal == 1 ) {
	     fprintf( outputFile, "contents b, %d\n", $1->indexExpr->offset );
	  } else {
	     fprintf( outputFile, "contents r%d, %d\n", $1->indexExpr->regNum, $1->indexExpr->offset );
	  }
       }
		     
       indReg2 = getRegisterCt();
       fprintf( outputFile, "%d r%d := %d + ", getICount(), indReg2, $3->offset );
       if ( $3->indexExpr->inReg == 1 ) {
	  fprintf( outputFile, "r%d\n", $3->indexExpr->regNum );
       } else {
	  if ( $3->indexExpr->isLocal == 1 ) {
	     fprintf( outputFile, "contents b, %d\n", $3->indexExpr->offset );
	  } else {
	     fprintf( outputFile, "contents r%d, %d\n", $3->indexExpr->regNum, $3->indexExpr->offset );
	  }
       }
       reg = getRegisterCt();
       if ( $1->isLocal == 1 ) {
	  fprintf( outputFile, "%d r%d := contents b, r%d", getICount(), reg, indReg );
       } else {
	  fprintf( outputFile, "%d r%d := contents r%d, r%d", getICount, reg, $1->regNum, indReg );
       }
       if ( $2 == 1 ) 
	  fprintf( outputFile, " * " );
       else 
	  fprintf( outputFile, " / " );

       if ( $3->isLocal == 1 ) {
	  fprintf( outputFile, "contents b, r%d", indReg2 );
       } else {
	  fprintf( outputFile, "contents r%d, r%d", $3->regNum, indReg2 );
       }	      
		     
    } else if ( $1->isArray == 1 ) {
       indReg = getRegisterCt();
       fprintf( outputFile, "%d r%d := %d + ", getICount(), indReg, $1->offset );
       if ( $1->indexExpr->inReg == 1 ) {
	  fprintf( outputFile, "r%d\n", $1->indexExpr->regNum );
       } else {
	  if ( $1->indexExpr->isLocal == 1 ) {
	     fprintf( outputFile, "contents b, %d\n", $1->indexExpr->offset );
	  } else {
	     fprintf( outputFile, "contents r%d, %d\n", $1->indexExpr->regNum, $1->indexExpr->offset );
	  }
       }
       reg = getRegisterCt();
       if ( $1->isLocal == 1 ) {
	  fprintf( outputFile, "%d r%d := contents b, r%d", getICount(), reg, indReg );
       } else {
	  fprintf( outputFile, "%d r%d := contents r%d, r%d", getICount, reg, $1->regNum, indReg );
       }
       
       if ( $2 == 1 ) 
	  fprintf( outputFile, " * " );
       else 
	  fprintf( outputFile, " / " );

       if  ( $3->inReg == 1 )  {
	  fprintf( outputFile, "r%d\n", $3->regNum );
       } else {
	  if ( $3->isLocal == 1 ) {
	     fprintf( outputFile, " contents b, %d\n", $3->offset );
	  } else {
	     fprintf( outputFile, " contents r%d, %d\n", $3->regNum, $3->offset ); 
	  }
       }
    } else if ( $3->isArray == 1 ) {
       indReg = getRegisterCt();
       fprintf( outputFile, "%d r%d := %d + ", getICount(), indReg, $3->offset );
       if ( $3->indexExpr->inReg == 1 ) {
	  fprintf( outputFile, "r%d\n", $3->indexExpr->regNum );
       } else {
	  if ( $3->indexExpr->isLocal == 1 ) {
	     fprintf( outputFile, "contents b, %d\n", $3->indexExpr->offset );
	  } else {
	     fprintf( outputFile, "contents r%d, %d\n", $3->indexExpr->regNum, $3->indexExpr->offset );
	  }
       }
       
       reg = getRegisterCt();
       fprintf( outputFile, "%d r%d := ", getICount(), reg );
	  
       
       if  ( $1->inReg == 1 )  {
	  fprintf( outputFile, "r%d", $1->regNum );
       } else {
	  if ( $1->isLocal == 1 ) {
	     fprintf( outputFile, " contents b, %d", $1->offset );
	  } else {
	     fprintf( outputFile, " contents r%d, %d", $1->regNum, $1->offset ); 
	  }
       }

       if ( $2 == 1 ) 
	  fprintf( outputFile, " * " );
       else 
	  fprintf( outputFile, " / " );


       if ( $3->isLocal == 1 ) {
	  fprintf( outputFile, "contents b, r%d\n", getICount(), reg, indReg );
       } else {
	  fprintf( outputFile, "contents r%d, r%d\n", getICount, reg, $3->regNum, indReg );
       }
       
    } else {
	 
	
       if ( $1->inReg == 1 ) {
	  reg = getRegisterCt();
	  fprintf( outputFile, "%d r%d := ", getICount(), reg );
	  fprintf( outputFile, "r%d ", $1->regNum );
       } else {
	  reg = getRegisterCt();
	  fprintf( outputFile, "%d r%d := ", getICount(), reg );
	  if ( $1->isLocal == 1 ) {
	     fprintf( outputFile, " contents b, %d", $1->offset );
	  } else {
	     fprintf( outputFile, " contents r%d, %d", $1->regNum, $1->offset ); 
	  }
       }      
       if ( $2 == 1 ) 
	  fprintf( outputFile, " * " );
       else 
	  fprintf( outputFile, " / " );

       if ( $3->inReg == 1 ) {
	  fprintf( outputFile, "r%d\n", $3->regNum );
       } else {
	  if ( $3->isLocal == 1 ) {
	     fprintf( outputFile, " contents b, %d\n", $3->offset );
	  } else {
	     fprintf( outputFile, " contents r%d, %d\n", $3->regNum, $3->offset ); 
	  }
       }
      
    }
    $$->regNum = reg;
    $$->inReg = 1;
 }
;


factor : primary 
       | primary EXPONENT primary 
       {

	  int baseR;
	  int exp;


	  if ( $1->inReg == 1 )  {
	     baseR = $1->regNum;    
	  } else if ( $1->isArray == 1 ) {
	     
	     int indReg = getRegisterCt();
	     fprintf( outputFile, "%d r%d := %d + ", getICount(), indReg, $1->offset );
	     if ( $1->indexExpr->inReg == 1 ) {
		fprintf( outputFile, "r%d\n", $1->indexExpr->regNum );
	     } else {
		if ( $1->indexExpr->isLocal == 1 ) {
		   fprintf( outputFile, "contents b, %d\n", $1->indexExpr->offset );
		} else {
		   fprintf( outputFile, "contents r%d, %d\n", $1->indexExpr->regNum, $1->indexExpr->offset );
		}
	     }
	     baseR = getRegisterCt();
	     if ( $1->isLocal == 1 ) {
		fprintf( outputFile, "%d r%d := contents b, r%d\n", getICount(), baseR, indReg );
	     } else {
		fprintf( outputFile, "%d r%d := contents, r%d, r%d\n", getICount(), baseR, $1->regNum, indReg );
	     }
	    

	  } else {
	     baseR = getRegisterCt();
	     if ( $1->isLocal == 1 ) {
		fprintf( outputFile, "%d r%d := contents b, %d\n", getICount(), baseR, $1->offset );
	     } else {
		fprintf( outputFile, "%d r%d := contents r%d, %d\n", getICount(), baseR, $1->regNum, $1->offset ); 
	     }
	  }

	  
	  if ( $3->inReg == 1 )  {
	     exp = $3->regNum;
	  }  else if ( $3->isArray == 1 ) {
	    
	     int indReg = getRegisterCt();
	     fprintf( outputFile, "%d r%d := %d + ", getICount(), indReg, $3->offset );
	     if ( $3->indexExpr->inReg == 1 ) {
		fprintf( outputFile, "r%d\n", $3->indexExpr->regNum );
	     } else {
		if ( $3->indexExpr->isLocal == 1 ) {
		   fprintf( outputFile, "contents b, %d\n", $3->indexExpr->offset );
		} else {
		   fprintf( outputFile, "contents r%d, %d\n", $3->indexExpr->regNum, $3->indexExpr->offset );
		}
	     }
	     baseR = getRegisterCt();
	     if ( $3->isLocal == 1 ) {
		fprintf( outputFile, "%d r%d := contents b, r%d\n", getICount(), baseR, indReg );
	     } else {
		fprintf( outputFile, "%d r%d := contents r%d, r%d\n", getICount(), baseR, $3->regNum, indReg );
	     }

	  } else {
	     exp = getRegisterCt();
	     if ( $3->isLocal == 1 ) {
		fprintf( outputFile, "%d r%d := contents b, %d\n", getICount(), exp, $3->offset );
	     } else {
		fprintf( outputFile, "%d r%d :=  contents r%d, %d\n", getICount(), exp, $3->regNum, $3->offset ); 
	     }
	  }
	   
	  int original = getRegisterCt();
	  int zero = getRegisterCt();
	  int one = getRegisterCt();
	  int sol = getRegisterCt();
	  int comp = getRegisterCt();
	  int neg1 = getRegisterCt();
	  int ct = getRegisterCt();
	  $$->inReg = 1;
	  $$->regNum = sol;
	 
	 
	  

	  fprintf( outputFile, "%d r%d := r%d\n", getICount(), original, exp ); //store original value of exp
	  fprintf( outputFile, "%d r%d := 0\n", getICount(), zero );
	  fprintf( outputFile, "%d r%d := 1\n", getICount(), one );
	  
	  
	  
	  //if exp is zero return 1
	  fprintf( outputFile, "%d r%d := 1\n", getICount(), sol );
	  fprintf( outputFile, "%d r%d := r%d = r%d\n", getICount(), comp, exp, zero );
	  int e = getICount();
	  fprintf( outputFile, "%d pc := %d if r%d\n", e, e + 16, comp );

	  //change exp to positive if negative and store the original
	  fprintf( outputFile, "%d r%d := r%d\n", getICount(), original, exp );
	  fprintf( outputFile, "%d r%d := r%d > r%d\n", getICount(), comp, exp, zero );
	  e = getICount();
	  fprintf( outputFile, "%d pc := %d if r%d\n", e, e+3, comp );
	  fprintf( outputFile, "%d r%d := -1\n", getICount(), neg1 );
	  fprintf( outputFile, "%d r%d := r%d * r%d\n", getICount(), exp, neg1, exp );
	  
	  
	  fprintf( outputFile, "%d r%d := r%d\n", getICount(), ct, exp );
	  fprintf( outputFile, "%d r%d := r%d >= r%d\n", getICount(), comp, zero, ct );
	  e = getICount();
	  fprintf( outputFile, "%d pc := %d if r%d\n", e, e + 5 , comp );

	  int loop = getICount();
	  fprintf( outputFile, "%d r%d := r%d * r%d \n", loop, sol, sol, baseR );
	  fprintf( outputFile, "%d r%d := r%d - r%d \n", getICount(), ct, ct, one );
	  fprintf( outputFile, "%d r%d := r%d < r%d\n", getICount(), comp, zero, ct );
	  fprintf( outputFile, "%d pc := %d if r%d\n", getICount(), loop, comp );


	  fprintf( outputFile, "%d r%d := r%d <= r%d\n", getICount(), comp, zero, original );
	  e = getICount();
	  fprintf( outputFile, "%d pc := %d if r%d\n", e, e+2, comp );
	  fprintf( outputFile, "%d r%d := r%d / r%d\n", getICount(), sol, one, sol );
	  
       }
       | NOT primary
       {
	  int reg;
	  $$->inReg = 1;
	  if ( $2->inReg == 1 ) {
	     reg = getRegisterCt();
	     fprintf( outputFile, "%d r%d := not ", getICount(), reg );
	     $$->inReg = 1;
	     $$->regNum = reg;
	     fprintf( outputFile, "r%d\n", $2->regNum );
	  } else if ( $2->isArray == 1 ) {
	    
	     int indReg = getRegisterCt();
	     fprintf( outputFile, "%d r%d := %d + ", getICount(), indReg, $2->offset );
	     if ( $2->indexExpr->inReg == 1 ) {
		fprintf( outputFile, "r%d\n", $2->indexExpr->regNum );
	     } else {
		if ( $2->indexExpr->isLocal == 1 ) {
		   fprintf( outputFile, "contents b, %d\n", $2->indexExpr->offset );
		} else {
		   fprintf( outputFile, "contents r%d, %d\n", $2->indexExpr->regNum, $2->indexExpr->offset );
		}
	     }
	     reg = getRegisterCt();
	     if ( $2->isLocal == 1 ) {
		fprintf( outputFile, "%d r%d := not contents b, r%d\n", getICount(), reg, indReg );
	     } else {
		fprintf( outputFile, "%d r%d := not contents r%d, r%d\n", getICount(), reg, $2->regNum, indReg );
	     }
	     $$->regNum = reg;
	     
			 	 
	  } else {
	     
	     if ( $2->isLocal == 1 ) {
		fprintf( outputFile, "contents b, %d\n", $2->offset );
	     } else {
		fprintf( outputFile, "contents r%d, %d\n", $2->regNum, $2->offset ); 
	     }
	  }
       }
;

primary : NUMBER 
        { 
	   $$ = ( exprNode* )malloc( sizeof( exprNode* ) );
	   $$->value = $1;
	   $$->isVar = 0;
	   $$->inReg = 1;
	   $$->regNum = getRegisterCt();
	   fprintf( outputFile, "%d r%d := %d\n", getICount(), $$->regNum, $1 );
	   $$->isArray = 0;
	   
        }
        | identifier 
	{
	   $$ = ( exprNode* )malloc( sizeof( exprNode* ) );
	   $$->inReg = 0;
	   $$->isArray = 0;
	   int reg;
	   int d;
	   btree *w = walkBack( $1, &d, reg, outputFile, 0 );
	   if ( w == NULL ) {
	      int i;
	      btree *p = NULL;
	      for ( i = 0; i < eCt; i++ ) {
		 p = searchForParent( eTypeArr[ i ] );
	      }
	      if ( p == NULL ) {
		 yyerror( "ERROR ID NOT FOUND\n" );
		 return;
	      } 
	      
	      eNode *currentE = p->data.eList;

	      int eComp;
	      int val = -1;

	      while ( currentE != NULL ) {
		 eComp = strcmp( $1, currentE->str );
		 if ( eComp == 0 ) {
		    val = currentE->index;
		    break;
		 }
		 currentE = currentE->next;
	      }
		 
	      if ( val == -1 ) {
		 yyerror( "invalid enum id\n" );
		 return;
	      }
	     
	      reg = getRegisterCt();
	      fprintf( outputFile, "%d r%d := %d\n", getICount(), reg, val );
	      $$->inReg = 1;
	      $$->regNum = reg;
	      $$->isVar = 0;
	      $$->isArray = 0;
	      $$->isEnum = 1;
	      $$->value = val;
	      
	   } else {
	      int comp = strcmp( "value", w->data.kind );
	      if ( comp == 0 ) {
		 $$->value = w->data.value;
		 $$->inReg = 1;
		 $$->isBool = 1;
		 $$->isVar = 0;
		 $$->regNum = getRegisterCt();
		 fprintf( outputFile, "%d r%d := %d\n", getICount(), $$->regNum, w->data.value );
	      } else {
		 $$->offset = w->data.offSet;
		 $$->isBool = 0;
		 if ( d == 0 ) {
		    $$->isLocal = 1;
		 } else {
		    $$->regNum = registerCt;
		    $$->isLocal = 0;
		    char *s = stack[ stackCtr-1-d ].name;
		    btree *f = searchForParent( s );
		    if ( f != NULL ) {
		       $$->baseReg = f->data.ARsize;
		    }
		 }
	      }
	   }
	}
        | '(' expression ')'
	{
	   $$ = ( exprNode* )malloc( sizeof( exprNode* ) );
	   $$ = $2;
	}

        | identifier '(' expression ')'

	{
	    $$ = ( exprNode* )malloc( sizeof( exprNode* ) );
	   $$->isVar = 1;
	   $$->inReg = 0;
	   int reg;
	   int d;
	   btree *w = walkBack( $1, &d, reg, outputFile, 0 );
	   if ( strcmp( "array", w->data.kind ) == 0 ) {
	      $$->isArray = 1;
	   } else {
	      $$->isArray = 0; 
	   }

	   if ( d == 0 ) {
	      $$->isLocal = 1;
	      $$->offset = w->data.offSet;
	      $$->isBool = 0;
	      $$->indexExpr = ( exprNode* )malloc( sizeof( exprNode* ) );
	      $$->indexExpr = $3;
	   } else {
	      $$->isLocal = 0;
	      $$->offset = w->data.offSet;
	      $$->isBool = 0;
	      $$->indexExpr = ( exprNode* )malloc( sizeof( exprNode* ) );
	      $$->indexExpr = $3;
	      $$->regNum = registerCt;
	      char *s = stack[ stackCtr-1-d ].name;
	      btree *f = searchForParent( s );
	      if ( f != NULL ) {
		 $$->baseReg = f->data.ARsize;
	      }
	   }
	}
;

boolean_op : AND { $$ = 0; }
           | OR  { $$ = 1; }
;

relational_op : EQ { $$ = 1; }
              | NEQ { $$ = 2; }
              | LTE { $$ = 3; }
              | GTE { $$ = 4; }
              | GT { $$ = 5; }
              | LT { $$ = 6; }
;

adding_op : '+' { $$ = 1; }
          | '-' { $$ = -1; }
;

multiplying_op : '*' { $$ = 1; }
               | '/' { $$ = -1; }
;

exception_part : exception_begin excpt_list
{
   PLhead = appendPL( jumpTableAdd, iCtr-1, 1, PLhead );
  
   int currentI = iCtr;
   int i;
   for ( i = 1; i < next_exception; i++ ) {
      if ( jump_table[ i ] == 0 ) {
	 jump_table[ i ] = currentI + next_exception - 1;
      }
      fprintf( outputFile, "%d pc := %d\n", getICount(), jump_table[ i ] );
   }


   PLhead = patch( raise_list, iCtr );

}
               |  
{
   if ( raise_list != NULL ) {
      PLhead = patch( raise_list, iCtr );
      raise_list = NULL;
   }

}
;

exception_begin: EXP
{

   int i = getICount();
   fprintf( outputFile, "%d pc := ?\n", i );
   PLhead = patch( raise_list, iCtr );
   raise_list = NULL;
   in_exception_part = 1;
   handler_done = 0;
   raise_list = appendPL( i, 0, 0, raise_list );
   i = getICount();
   fprintf( outputFile, "%d pc := r1, ?\n", i );
   jumpTableAdd = i;
   int x;
   for ( x = 0; x < jumpSize; x++ ) {
      jump_table[ x ] = 0;
   }

}

excpt_list : exp excpt_list 
| exp
; 

exp : WHEN choice_sequence ARROW statement_sequence 
{
   
   fprintf( outputFile, "%d r1 := 0\n", getICount() );
   int i = getICount();
   fprintf( outputFile, "%d pc := ?\n", i );
   raise_list = appendPL( i, 0, 0, raise_list );
}
;

choice_sequence : identifier 
{
   if ( handler_done == 1 ) {
      printf( "abort\n" );
   } else {
      btree *w = searchForParent( $1 );
      int comp = strcmp( "exception", w->data.kind );
      if ( ( w == NULL ) || ( comp != 0 ) ) {
	 yyerror( "exception type does not exist\n" );
	 return;
      }
      jump_table[ w->data.iCount ] = iCtr;

   }
}
| choice_sequence '|' identifier
{
   if ( handler_done == 1 ) {
      printf( "abort\n" );
   } else {
      btree *w = searchForParent( $3 );
      if ( ( w == NULL ) || ( strcmp( "exception", w->data.kind ) != 0 ) ) {
	 yyerror( "exception type does not exist\n" );
	 return;
      }

      jump_table[ w->data.iCount ] = iCtr;

   }

}
| OTHERS 
{
   if ( handler_done == 1 ) {
      printf( "abort" );
   } else {
      handler_done = 1;
      int i;
      for ( i = 0; i < jumpSize; i++ ) {
	 if ( jump_table[ i ] == 0 ) {
	    jump_table[ i ] = iCtr;
	 }
      }
   }

}
;

condition : expression
{
   $$ = ( exprNode* )malloc( sizeof( exprNode* ) );
   $$ = $1;
   int reg; 
   if ( $1->inReg == 0 ) {
      if ( $1->isLocal == 1 ) {
	 reg = getRegisterCt();
	 fprintf( outputFile, "%d r%d := contents b, %d\n", getICount(), reg, $1->offset );
	 $$->regNum = reg;
      } else {
	 fprintf( outputFile, "%d r%d := contents r%d, %d\n", getICount(), reg, $1->regNum, $1->offset );
	 $$->regNum = reg;
      }
      $$->inReg = 1;
   }
}
;

identifier : ID { $$ = $1; }
;

%%
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <ctype.h>

extern char linebuf[ 500 ];
FILE *outputFile;
FILE *pFile;

main() {
   createOuterContext();
   outputFile = fopen( "machineCode.txt", "w" );
   generatePrologue( outputFile );
   if ( outputFile == NULL ) {
      printf( "ERROR: COULD NOT OPEN FILE\n" );
      return;
   }
   printf( "About to parse\n" );
   yyparse();   
   /* fclose( outputFile ); */
   /* outputFile = fopen( "machineCode.txt", "r" ); */
   /* pFile = fopen( "pachedCode.txt", "w" ); */
   /* patchFile( outputFile, pFile ); */
   /* fclose( outputFile ); */
   /* fclose( pFile ); */
}
