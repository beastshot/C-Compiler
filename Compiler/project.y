%{
	#define _GNU_SOURCE/*for asprintf ...(for files actions)*/
	#include <stdio.h>
	#include <stdlib.h>
	#include <string.h>
	#include "lex.yy.c"
	#define CloseDown 111

	typedef enum {false,true}bool;
	typedef struct node
	{
		char *token;
		struct node *left;
		struct node *right;
                int sum;
		char *My_code;
                char *LblT;
		char *LblF;
		char *var;
		char *label;
		
		

	} node;

	typedef struct Arguments
	{
		char * name;
                char * ext;
		char * type;
		
	}Arguments;

    typedef struct Function 
	{
                char * name;
		struct Arguments * args;
                char *returnType; 
		int ArgsNum;
		int get_ret;
    } Function;
	

	typedef struct Varaiables
	{	int isArg;
		char *name;
		char *value;
		char *type;
		char * ext;
	}Varaiable;

		typedef struct code
	{	char *name;
		int place;
                Function ** func;
		Varaiable * var;
		struct code * nextcode;
		struct code * oldcode;
                int countvar;
		int countfunc;
	}code;
        node* mknode(char* token, node *left, node *right);
	void Printtree(node *tree);
	void printTabs(int num);
        int count=0;
	int yylex();
	int yyerror(char *e);
	char * Type_expr(node *,code*);
	Arguments * mkArgs(node *,int *);
	void New_Func(char * name,Arguments * args,node *returnType,int ArgsNum,code*);
	void New_Var(Arguments * args,int,int,code * Current_Scoope);
	code* Pr_Code(char *);
	void push(code* from,char*);
	code* Start_Code=NULL;
	code* CodeBegin(code * codey);
	static int scope=0;
	void test(node *tree,code * scope);
	char* Search_Func(node * tree,code * Current_Scoope,int* count);
        char* Load_Var();
        void InsertCode2(node* node,char *code,char *var,char *label,char *LblT,char *LblF);
	char* Load_Lbl();
	char* Many_G(char*,char*,char*,char*,char*);
	char *Search_Var(node * tree,code * Current_Scoope);
	Arguments * Args_cFunc(code *,node *tree,int * count);
	int flagMain=false;
	static int t=0;
	static int l=0;
	static int line=0;
	static node * grandparent;
	int POPParams(Arguments * args,int count);
        char * mkspace(char *label);
	void InsertCode(node* node,char *code,char *var,char *label,char *LblT,char *LblF);
	char* F_Stract(char*des,char*src);
	char *Replace(const char *s, const char *oldW, const char *newW);
	void OutputOf3AC(node * tree);
	


	
	
%}
%union
{
    struct node *node;
    char *string;
}



%token <string> COMMENT WHILE IF ELSE 
%token <string> RETURN
%token <string> BOOL STRING CHARPTR CHAR INT INTPTR PROC
%token <string> AND ADDRESS EQUAL ASSUME OR PIPE BIGGEREQ BIGGER LESSEQ LESS NOTEQ NOT
%token <string> DIV PLUS MINUS MULTI VAR
%token <string> STRINGPAR REALPAR CHARPAR NULLNULL
%token <string> MAIN ID SEMICOLON COMMA STARTPAREN ENDPAREN FIRSTINDEX LASTINDEX STARTBLOCK ENDBLOCK
%token <string> DECIMALPAR HEXPAR B_TRUE B_FALSE  REAL REALPTR FUNC COLON  DEREFRENCE 

%left ID OR AND 
%left  NOTEQ LESS LESSEQ BIGGEREQ BIGGER 
%left PLUS MINUS RETURN
%left MULTI DIV
%left SEMICOLON EQUAL
%right NOT ENDBLOCK


%nonassoc STARTPAREN
%nonassoc IF
%nonassoc ELSE 


%type <node> address_gram statments_gram block_Stmnt_gramm derefrence_gram exprs_gram call_func 
%type <node> expr lhs assmnt_stmnt Block_Inside 
%type <node> statment func_type id_type var_id declearation paren_expr
%type <node> pro_body para_list para_pro procedure procedures
%type <node> main startcode start declearations 
%%
 
start: com startcode {grandparent = $2; test($2,Start_Code);  OutputOf3AC($2);  }; 

startcode: procedures main{$$=mknode("CODE",$1,$2);  }

com: COMMENT com {;}| ;

main: PROC MAIN STARTPAREN ENDPAREN com STARTBLOCK pro_body ENDBLOCK
{
$$=mknode("Main",mknode("ARGS",NULL,$7),NULL);t=0;

};


procedures: procedures  procedure {$$=mknode("procedures",$1,$2);}
	| {$$=NULL;};


procedure: FUNC ID STARTPAREN para_pro ENDPAREN com RETURN func_type  STARTBLOCK  pro_body ENDBLOCK
{ 
		$$=mknode("FUNC",mknode($2,mknode("",NULL,NULL),mknode("ARGS",$4,mknode("Return",$8,NULL))),mknode("",$10,NULL));t=0; 
}
| PROC ID STARTPAREN para_pro ENDPAREN  STARTBLOCK  pro_body ENDBLOCK
{
	$$=mknode("PROC",mknode($2,mknode("",NULL,NULL),NULL),mknode("ARGS",$4,$7));
	t=0; 
};


para_pro: para_list {$$=$1;}
| {$$=NULL;};



para_list: var_id COLON id_type {$$=mknode("(",$3,mknode("",$1,mknode(")",NULL,NULL)));}
	|  para_list SEMICOLON com  para_list {$$=mknode("",$1,mknode("",$4,NULL));}	;


pro_body: com  procedures declearations statments_gram 
{
	$$=mknode("BODY", mknode(" ",$2,NULL),mknode(" ",$3,mknode(" ",$4,mknode(" ",NULL,NULL))));
	
};



declearations: declearations declearation  {$$=mknode("",$1,$2);} | {$$=NULL;}  ;

declearation: VAR var_id COLON id_type com SEMICOLON com
{
	$$=mknode("var", $4,$2);
};


var_id: ID COMMA var_id {$$=mknode($1, mknode(" ", $3, NULL),NULL);}
	| ID {$$=mknode($1, NULL, NULL);} ;


id_type: BOOL {$$=mknode("boolean", NULL, NULL);}
	| STRING FIRSTINDEX DECIMALPAR LASTINDEX {$$=mknode("string", mknode("[",mknode("$3",NULL,NULL),NULL), NULL);}
	| CHAR {$$=mknode("char", NULL, NULL);}
	| INT {$$=mknode("int", NULL, NULL);}
	| REAL {$$=mknode("real", NULL, NULL);}
	| INTPTR {$$=mknode("int*", NULL, NULL);}
	| CHARPTR {$$=mknode("char*", NULL, NULL);}
	| REALPTR {$$=mknode("real*", NULL, NULL);};



func_type: BOOL {$$=mknode("boolean", NULL, NULL);}
 	| STRING {$$=mknode("string", NULL, NULL);}
	| CHAR {$$=mknode("char", NULL, NULL);}
	| INT {$$=mknode("int", NULL, NULL);}
	| REAL {$$=mknode("real", NULL, NULL);}
	| INTPTR {$$=mknode("int*", NULL, NULL);}
	| CHARPTR {$$=mknode("char*", NULL, NULL);}
	| REALPTR {$$=mknode("real*", NULL, NULL);};
	

statments_gram: statments_gram statment   { $$=mknode("stmnts",$1,$2); if(strcmp($2->token, "if") == 0||strcmp($2->token, "if-else") == 0||strcmp($2->token, "while") == 0){ if($$->sum==0) {InsertCode($2,NULL,NULL,Load_Lbl(),NULL,NULL); $$->sum=1;}}   }| {$$=NULL;};


block_Stmnt_gramm: statment {$$=$1; if(strcmp($1->token, "if") == 0||strcmp($1->token, "if-else") == 0||strcmp($1->token, "while") == 0) InsertCode($1,NULL,NULL,Load_Lbl(),NULL,NULL);}|declearation {$$=$1;}|procedure {$$=$1;} |SEMICOLON  {$$=mknode("",NULL,NULL);};


Block_Inside: STARTBLOCK procedures com declearations statments_gram ENDBLOCK com
{
	$$=mknode("{",$2,mknode("", $4,mknode("", $5,("}",NULL,NULL))));
	
}


statment: IF STARTPAREN expr ENDPAREN  block_Stmnt_gramm 
{
	$$=mknode("if",
	mknode("(", $3, 
	mknode(")",NULL,NULL)),$5);
	InsertCode($3,NULL,NULL,NULL,Load_Lbl(),NULL);
	
	
}%prec IF
| IF STARTPAREN expr ENDPAREN   block_Stmnt_gramm    ELSE  block_Stmnt_gramm  
{
	$$=mknode("if-else",
	mknode("", $3, 
	mknode("",NULL,NULL)),
	mknode("",$5,
	mknode("",$7,NULL)));
	InsertCode($3,NULL,NULL,NULL,Load_Lbl(),NULL);
	InsertCode($3,NULL,NULL,NULL,NULL,Load_Lbl());
	
}
| WHILE com STARTPAREN expr ENDPAREN  block_Stmnt_gramm  
{
	$$=mknode("while",
	mknode("(", $4, 
	mknode(")",NULL,NULL)),$6);
InsertCode($$,NULL,NULL,NULL,Load_Lbl(),NULL);
	InsertCode($$,NULL,NULL,NULL,NULL,Load_Lbl());
}
| assmnt_stmnt SEMICOLON com {$$=mknode("assmnt_stmnt",$1,NULL);  }
| expr SEMICOLON com {$$=$1;}
| RETURN expr SEMICOLON com {$$=mknode("return",$2,NULL);}
| Block_Inside{$$=$1;} ;





assmnt_stmnt: lhs ASSUME expr 
{
	$$=mknode("=",$1,$3);
};



lhs: ID FIRSTINDEX expr LASTINDEX 
{
	$$=mknode($1, mknode("[",$3,mknode("]",NULL,NULL)), NULL);
} 
| ID {$$=mknode($1,NULL,NULL);}
| address_gram {$$=$1;}
| derefrence_gram{$$=$1;} ;


	

expr:  STARTPAREN expr ENDPAREN {$$=mknode("(",$2,mknode(")",NULL,NULL));}
        | expr EQUAL expr {$$=mknode("==",$1,$3); }
	| expr NOTEQ expr {$$=mknode("!=",$1,$3);}
	| expr BIGGEREQ expr {$$=mknode(">=",$1,$3);}
	| expr BIGGER expr {$$=mknode(">",$1,$3);}
	| expr LESSEQ expr {$$=mknode("<=",$1,$3);}
	| expr LESS expr {$$=mknode("<",$1,$3);}
	| expr AND expr {$$=mknode("&&",$1,$3); InsertCode($1,NULL,NULL,NULL,Load_Lbl(),NULL); }
	| expr OR expr {$$=mknode("||",$1,$3); InsertCode($1,NULL,NULL,NULL,NULL,Load_Lbl());  }
	| expr PLUS expr {$$=mknode("+",$1,$3); }
	| expr MINUS expr {$$=mknode("-",$1,$3); }
	| expr MULTI expr {$$=mknode("*",$1,$3); }
	| expr DIV expr {$$=mknode("/",$1,$3);}
	| NOT expr {$$=mknode("!",$2,NULL);}
	| address_gram {$$=$1;}
	| derefrence_gram {$$=$1;}
	| call_func com {$$=$1;}
	| DECIMALPAR {$$=mknode($1,mknode("INT",NULL,NULL),NULL);}
	| HEXPAR {$$=mknode($1,mknode("HEX", NULL, NULL),NULL);}
	| CHARPAR {$$=mknode($1,mknode("CHAR", NULL, NULL),NULL);}
	| REALPAR {$$=mknode($1,mknode("REAL", NULL, NULL),NULL);}
	| STRINGPAR {$$=mknode($1,mknode("STRING", NULL, NULL),NULL);}
	| B_FALSE {$$=mknode($1,mknode("BOOLEAN", NULL, NULL),NULL);}
	| B_TRUE {$$=mknode($1,mknode("BOOLEAN", NULL, NULL),NULL); }
	| PIPE ID PIPE 
	{
		$$=mknode("|",
		mknode($2,NULL,NULL),
		mknode("|",NULL,NULL));
		
	}
	| ID FIRSTINDEX expr LASTINDEX 
	{$$=mknode("solovar",mknode($1,mknode("[",$3,mknode("]",NULL,NULL)),NULL),NULL);}
	| ID {$$=mknode("solovar",mknode($1,NULL,NULL),NULL);}
	| NULLNULL {$$=mknode("null",NULL,NULL);};

address_gram: ADDRESS ID {$$=mknode("&",mknode($2,NULL,NULL),NULL);}
	| ADDRESS STARTPAREN ID ENDPAREN {$$=mknode("&",mknode("(",mknode($3,NULL,NULL),NULL),mknode(")",NULL,NULL));}
	| ADDRESS ID FIRSTINDEX expr LASTINDEX 
	{$$=mknode("&", mknode($2,mknode("[",$4,mknode("]",NULL,NULL)),NULL),NULL);
	}
	| ADDRESS STARTPAREN ID FIRSTINDEX expr LASTINDEX ENDPAREN 
	{
		$$=mknode("&",
		mknode("(", 
		mknode($3,mknode("[",$5,mknode("]",NULL,NULL)),NULL)
		,mknode(")",NULL,NULL)),NULL);
	};

	derefrence_gram: DEREFRENCE ID {$$=mknode("^",mknode($2,NULL,NULL),NULL);InsertCode($$,"",F_Stract("*",$2),NULL,NULL,NULL);}
	| DEREFRENCE STARTPAREN expr ENDPAREN {$$=mknode("^",mknode("(",$3,NULL),mknode(")",NULL,NULL));InsertCode($$,$3->My_code,F_Stract("*",$3->var),NULL,NULL,NULL);}
	| DEREFRENCE ID FIRSTINDEX expr LASTINDEX 
	{$$=mknode($1, mknode($2,mknode("[",$4,mknode("]",NULL,NULL)),NULL), NULL);
	};


exprs_gram: expr COMMA exprs_gram {$$=mknode("exprs_gram",$1,mknode(",",$3,NULL));} 
	| expr {$$=mknode("exprs_gram",$1,NULL);}
	| {$$=NULL;};

paren_expr:STARTPAREN exprs_gram ENDPAREN {$$=$2;};

call_func: ID paren_expr {$$=mknode("Call func",mknode($1,NULL,NULL),mknode("ARGS",$2,NULL)); 

} ;
%%



int main()
{
	FILE * f=fopen("FinalResult.txt","w+");
        int correct= yyparse();
        if(correct==0)
        {
        printf("\n********************\n Part 2 Output\n********************  \n");
        printf(" Correct Syntax and Semantics \n");
        printf("\n********************\n Part 3 Output\n********************  \n");
        }
        fprintf(f,"%s",grandparent->My_code);
        printf("%s",grandparent->My_code);
        return correct;
}
Arguments * Args_cFunc(code * Current_Scoope,node *tree,int * count)
{
	Arguments  *arr=NULL,ar[50];
	char* type,*ext;
	while(tree!=NULL)
	{
		ar[(*count)++].type=Type_expr(tree->left,Current_Scoope);
		if(tree->right!=NULL)
			tree=tree->right->left;
		else
			tree=NULL;

	}
	arr=(Arguments*)malloc(sizeof(Arguments)*(*count));
	for(int i=0;i<*count;i++)
		arr[i].type=ar[i].type;
	return arr;
}
char* Search_Func(node * tree,code * Current_Scoope,int * countParams)
{
	code*temp=Current_Scoope;
	Arguments* args;
	int find=false,flag=true;
	while(temp!=NULL)
	{
		for(int i=0;i<temp->countfunc;i++)
		if(strcmp(tree->left->token,temp->func[i]->name)==0)
		{
			find=true;
			flag=true;
			int count=0;
			args=Args_cFunc(Current_Scoope,tree->right->left,&count);
			if(count==temp->func[i]->ArgsNum)
			{
				for(int j=0,t=count-1;j<count;j++,t--)
				{
					if(strcmp(args[j].type,temp->func[i]->args[t].type)!=0)
						flag=false;
				}
				if(flag==true){
					if(countParams!= NULL)
						*countParams = POPParams(args,count);
					return temp->func[i]->returnType;
				}
			}
		}
		temp=temp->oldcode;
	}
        if(find!=true)
	printf("ERROR,you can not use func/proc ['%s'] in ['%s %s'] because it does not defined yet\n",tree->left->token,Current_Scoope->name,Start_Code->func[Start_Code->countfunc-1]->name);
	if(find==true)
		printf("ERROR,func/proc ['%s'] in ['%s %s'] get other args from what is defined to get\n",tree->left->token,Current_Scoope->name,Start_Code->func[Start_Code->countfunc-1]->name);
	exit(1);
}
char *Search_Var(node * tree,code * Current_Scoope)
{
	code*temp=Current_Scoope;
	if(strcmp(tree->token,"solovar")==0)
		tree=tree->left;
	while(temp!=NULL)
	{
		for(int i=0;i<temp->countvar;i++)
		if(strcmp(tree->token,temp->var[i].name)==0)
		{
			
			if(tree->left!=NULL && strcmp(tree->left->token,"[")==0)
			{
				if(strcmp(temp->var[i].type,"string")==0)
					if(strcmp(Type_expr(tree->left->left,Current_Scoope),"int")==0)
					{
						return "char";
					}
					else
					{
						printf("ERROR,in ['%s  %s']  String index must be type of int\n",Current_Scoope->name,Start_Code->func[Start_Code->countfunc-1]->name);
						exit(1);
					}
				else
				{
					printf("ERROR,in ['%s  %s']  Var ' %s ' cant have index because its not a string var\n",Current_Scoope->name,Start_Code->func[Start_Code->countfunc-1]->name,tree->token);
						exit(1);
				}

			}
			else
			return temp->var[i].type;

		}
		temp=temp->oldcode;
	}
	printf("ERROR,Var ' %s 'does not defined in func/proc [ ' %s ' ]\n ",tree->token,Start_Code->func[Start_Code->countfunc-1]->name);
	exit(1);
	
}
char * Type_expr(node * tree,code* Current_Scoope){
	char* msg=(char*)malloc(sizeof(char)*7);
	msg="";
	if(strcmp(tree->token,"null")==0)
		msg="NULL";
	else
	if(tree->left!=NULL){
		if(strcmp(tree->left->token,"INT")==0)
			msg= "int";
		if(strcmp(tree->left->token,"HEX")==0)
			msg= "hex";
		if(strcmp(tree->left->token,"CHAR")==0)
			msg= "char";
		if(strcmp(tree->left->token,"REAL")==0)
			msg= "real";
		if(strcmp(tree->left->token,"STRING")==0)
			msg= "string";
		if(strcmp(tree->left->token,"BOOLEAN")==0)
			msg= "boolean";
		if(strcmp(tree->token,"!")==0)
		if(strcmp(Type_expr(tree->left,Current_Scoope),"boolean")==0)
			msg="boolean";
		else{
			printf("ERROR, '!' used just for bool type\n");
			exit(1);
		}
		if(strcmp(tree->token,"|")==0)
		if(strcmp(Type_expr(tree->left,Current_Scoope),"string")==0)
		msg="int";
		else{
			printf("ERROR, '| |' in proc or func %s must used just with var from string type \n",Start_Code->func[Start_Code->countfunc-1]->name);
			exit(1);
		}
		if(strcmp(tree->token,"==")==0||strcmp(tree->token,"!=")==0)
		{
			if(strcmp(Type_expr(tree->left,Current_Scoope),Type_expr(tree->right,Current_Scoope))==0&&strcmp(Type_expr(tree->right,Current_Scoope),"string")!=0)
			msg="boolean";
			else{
				printf("ERROR, you cant compare using '%s' between [%s] and [%s] in func/proc ' %s '\n",tree->token,Type_expr(tree->left,Current_Scoope),Type_expr(tree->right,Current_Scoope),Start_Code->func[Start_Code->countfunc-1]->name);
				exit(1);
			}
		}

		if(strcmp(tree->token,">=")==0||strcmp(tree->token,">")==0||strcmp(tree->token,"<=")==0||strcmp(tree->token,"<")==0)
		{
			if((strcmp(Type_expr(tree->left,Current_Scoope),"int")==0||strcmp(Type_expr(tree->left,Current_Scoope),"real")==0)&&(strcmp(Type_expr(tree->right,Current_Scoope),"int")==0||strcmp(Type_expr(tree->right,Current_Scoope),"real")==0))
			msg="boolean";
			else{
				printf("ERROR, you could not do '%s' between ' %s ' and ' %s ' in func/proc ['%s']\n",tree->token,Type_expr(tree->left,Current_Scoope),Type_expr(tree->right,Current_Scoope),Start_Code->func[Start_Code->countfunc-1]->name);
				exit(1);
			}
		}

		if(strcmp(tree->token,"&&")==0||strcmp(tree->token,"||")==0)
		{

			if(strcmp(Type_expr(tree->left,Current_Scoope),Type_expr(tree->right,Current_Scoope))==0&&strcmp(Type_expr(tree->right,Current_Scoope),"boolean")==0)
			msg="boolean";
			else{
				printf("ERROR, you could not do ' %s ' between ' %s ' and ' %s ' in func/proc [' %s ']\n",tree->token,Type_expr(tree->left,Current_Scoope),Type_expr(tree->right,Current_Scoope),Start_Code->func[Start_Code->countfunc-1]->name);
				exit(1);
			}
			

		}
		if(strcmp(tree->token,"-")==0||strcmp(tree->token,"+")==0)
		{
			if((strcmp(Type_expr(tree->left,Current_Scoope),"int")==0||strcmp(Type_expr(tree->left,Current_Scoope),"real")==0)&&(strcmp(Type_expr(tree->right,Current_Scoope),"int")==0||strcmp(Type_expr(tree->right,Current_Scoope),"real")==0))
			{
			if(strcmp(Type_expr(tree->left,Current_Scoope),Type_expr(tree->right,Current_Scoope))==0&&strcmp(Type_expr(tree->left,Current_Scoope),"int")==0)
			msg="int";
			else
			msg="real";
			}

			if(strcmp(Type_expr(tree->right,Current_Scoope),"int")==0&&(strcmp(Type_expr(tree->left,Current_Scoope),"char*")==0||strcmp(Type_expr(tree->right,Current_Scoope),"int*")==0||strcmp(Type_expr(tree->right,Current_Scoope),"real*")==0)){
				msg=Type_expr(tree->left,Current_Scoope);
			}
			else if(strcmp(msg,"")==0)
			{
				printf("ERROR, you could not do ' %s ' between ' %s ' and ' %s ' in func/proc [' %s ']\n",tree->token,Type_expr(tree->left,Current_Scoope),Type_expr(tree->right,Current_Scoope),Start_Code->func[Start_Code->countfunc-1]->name);
				exit(1);
			}

		}
		if(strcmp(tree->token,"*")==0||strcmp(tree->token,"/")==0)
		{
			if((strcmp(Type_expr(tree->left,Current_Scoope),"int")==0||strcmp(Type_expr(tree->left,Current_Scoope),"real")==0)&&(strcmp(Type_expr(tree->right,Current_Scoope),"int")==0||strcmp(Type_expr(tree->right,Current_Scoope),"real")==0))
			{
			if(strcmp(Type_expr(tree->left,Current_Scoope),Type_expr(tree->right,Current_Scoope))==0&&strcmp(Type_expr(tree->left,Current_Scoope),"int")==0)
			msg="int";
			else
			msg="real";
			}
			else
			{
				printf("ERROR, you could not do ' %s ' between ' %s '  and ' %s '\n",tree->token,Type_expr(tree->left,Current_Scoope),Type_expr(tree->right,Current_Scoope));
				exit(1);
			}
		}
		if(strcmp(tree->token,"&")==0)
		{
			if(strcmp(tree->left->token,"(")==0)
				msg=Type_expr(tree->left->left,Current_Scoope);
			else{
				msg=Type_expr(tree->left,Current_Scoope);
				
				}
			if(strcmp(msg,"char")==0)
			msg="char*";
			else
			if(strcmp(msg,"int")==0)
			msg="int*";
			else
			if(strcmp(msg,"real")==0)
			msg="real*";
			else
			{
				printf("ERROR, Operatot '%s' used just for (real,string[i],char,int) but you used it for ' %s ' \n",tree->token,msg);
				exit(1);
			}
		}
		if(strcmp(tree->token,"^")==0)
		{
			if(strcmp(tree->left->token,"(")==0)
				msg=Type_expr(tree->left->left,Current_Scoope);
			else
				msg=Type_expr(tree->left,Current_Scoope);
			
			if(strcmp(msg,"char*")==0)
			msg="char";
			else
			if(strcmp(msg,"int*")==0)
			msg="int";
			else
			if(strcmp(msg,"real*")==0)
			msg="real";
			else
			{
				printf("ERROR, Operatot '%s' used just for Pointers but you used it for ' %s ' \n",tree->token,msg);
				exit(1);
			}

		}
		if(strcmp(tree->token,"(")==0)
			msg=Type_expr(tree->left,Current_Scoope);
		if(strcmp(tree->token,"Call func")==0)
			msg=Search_Func(tree,Current_Scoope,NULL);
		
	}
	if(strcmp(msg,"")==0)
		msg=Search_Var(tree,Current_Scoope);

	
	

	return msg;
}
void push(code* from,char* name)
{
	code * point;
	if(Start_Code==NULL)
		Start_Code=Pr_Code(name);
	else{
	point=Start_Code;
	while(point->nextcode!=NULL)
		point=point->nextcode;
	point->nextcode=Pr_Code(name);
	point->nextcode->oldcode=from;
	}
}
code* Pr_Code(char* name)
{	
	code *newlvl = (code*)malloc(sizeof(code));
	newlvl->place=++scope;
	newlvl->name=name;
	newlvl->var=NULL;
	newlvl->countvar=0;
	newlvl->func=NULL;
	newlvl->countfunc=0;
	newlvl->nextcode=NULL;
	newlvl->oldcode=NULL;
	return newlvl;
}


void New_Var(Arguments * args,int countvars,int isArg,code * Current_Scoope){
	if(countvars==0)
	return;
	Varaiable* temp;
	code * codey=Current_Scoope;

	for(int i=0;i<countvars;i++)
		for(int j=0;j<countvars;j++)
	if(i!=j && strcmp(args[j].name,args[i].name)==0 )
	{
		printf("ERROR, you can not put vars %s in the same scoope",args[i].name);
		code * t=codey->oldcode;
		while(t->oldcode!=NULL && t->oldcode->countfunc==0)
			t=t->oldcode;
		if(t->func!=NULL)
		printf(",in func %s\n",t->func[t->countfunc-1]->name);
			else
		printf("\n");
		exit(1);
	}
	if(codey->var==NULL)
	{ 
		codey->var=(Varaiable*) malloc(sizeof(Varaiable)*countvars);
	}
	else
	{
		temp=codey->var;
		codey->var=(Varaiable*) malloc(sizeof(Varaiable)*(codey->countvar+countvars));
		for(int i=0;i<codey->countvar;i++)
		{
			for(int j=0;j<countvars;j++)
			{
				if(strcmp(temp[i].name,args[j].name)==0 )
				{
					printf("ERROR ,  var ' %s ' Used before ",temp[i].name);
					code * t=codey->oldcode;
					while(t->oldcode!=NULL && t->oldcode->countfunc==0)
						t=t->oldcode;
					if(t->func!=NULL)
					printf("in func %s and cant be use more than one time in the same scoope \n",t->func[t->countfunc-1]->name);
					else
					printf("\n");
					exit(1);
				}
			}
			codey->var[i]=temp[i];	
		}
	}
	for(int j=0;j<countvars;j++)
	{

		codey->var[codey->countvar].name=args[j].name;
		codey->var[codey->countvar].value=NULL;
		codey->var[codey->countvar].isArg=isArg;
		codey->var[codey->countvar].ext=args[j].ext;
		codey->var[(codey->countvar)++].type=args[j].type;
	}
}

void New_Func(char * name,Arguments * args,node *returnType,int ArgsNum,code * Current_Scoope){
	Function** temp;
	code * codey=Current_Scoope;
	for(int i=0;i<ArgsNum;i++)
		for(int j=0;j<ArgsNum;j++)
	if(i!=j && strcmp(args[j].name,args[i].name)==0 )
	{
		printf("ERROR,func %s could not accept rhese args '%s' \n",name,args[i].name);
		exit(1);
	}
	if(codey->func==NULL)
	{ 
		codey->func=(Function**) malloc(sizeof(Function*));
	}
	else
	{
		temp=codey->func;
		codey->func=(Function**) malloc(sizeof(Function*)*(codey->countfunc+1));
		for(int i=0;i<codey->countfunc;i++)
		{
				if(strcmp(temp[i]->name,name)==0 )
				{
					printf("ERROR , func ' %s ' defined before \n",temp[i]->name);
					exit(1);
				}
				codey->func[i]=temp[i];
		}
	}
		codey->func[codey->countfunc]=(Function*) malloc(sizeof(Function));
		codey->func[codey->countfunc]->name=name;
		codey->func[codey->countfunc]->args=args;
		if(returnType==NULL)
		codey->func[codey->countfunc]->returnType=NULL;
		else{
		if(strcmp(returnType->token,"string")==0)
			{
				printf("ERROR,func %s could not return string type but other types\n",name);
				exit(1);
			}
		codey->func[codey->countfunc]->returnType=returnType->token;
		}
		codey->func[codey->countfunc]->ArgsNum=ArgsNum;
		codey->func[codey->countfunc]->get_ret=false;
		++(codey->countfunc); 
}

Arguments * mkArgs(node *tree,int *count){
	Arguments  *arr=NULL,ar[50];
	char* type,*ext;
	if(tree!=NULL)
	{
		node * temp1=tree,*temp=tree;
		do{
		if(strcmp(temp1->token, "")==0)
		{
			temp=temp1->right->left;
			temp1=temp1->left;
			
			
			if(strcmp(temp->token, "(")==0||strcmp(temp->token, "var")==0)
		{
			type=temp->left->token;
			if(temp->left->left!=NULL)
			ext=temp->left->left->left->token;
			node * treee;
			treee=temp->right->left;
			do{
			ar[*count].name=treee->token;
			ar[*count].type=type;
			ar[*count].ext=ext;
			(*count)++;
			if(treee->left==NULL)
				treee=NULL;
			else
				treee=treee->left->left;
			}while(treee!=NULL);
		}
		}
		}while(strcmp(temp1->token, "(")!=0&&strcmp(temp->token, "var")!=0);
		temp=temp1;
		if(strcmp(temp->token, "(")==0||strcmp(temp->token, "var")==0)
		{
			type=temp->left->token;
			node * treee;
			if(strcmp(temp->token, "var")==0)
			treee=temp->right;
			else
			treee=temp->right->left;
			if(temp->left->left!=NULL)
			ext=temp->left->left->left->token;
			do{
			ar[*count].name=treee->token;
			ar[*count].type=type;
			ar[*count].ext=ext;
			(*count)++;
			if(treee->left==NULL)
				treee=NULL;
			else
				treee=treee->left->left;
			}while(treee!=NULL);
		}
		arr=(Arguments*)malloc(sizeof(Arguments)*(*count));
		for(int i=0;i<*count;i++)
		{
			for(int j=0;j<*count;j++){
			}
			arr[i].name=ar[i].name;
			arr[i].type=ar[i].type;
		}
	}
	return arr;
}
node* mknode (char *token, node *left, node *right)
{
	node *newnode = (node*)malloc(sizeof(node));
	newnode->left=left;
	newnode->right=right;
	newnode->token=token;
	newnode->My_code=NULL;
	newnode->var=NULL;
	newnode->label=NULL;
	newnode->LblT=NULL;
	newnode->LblF=NULL;
        newnode->sum=0;
	return newnode;
}

void printTabs(int n)
{
	int i;
	for(i=0;i<n/9;i++)
		printf(" ");
}
void Printtree(node* tree)
{
	int Closetype=0;
          printf(" "); 
	if(strcmp(tree->token, "(IF\n") == 0 || strcmp(tree->token, "(IF_ELSE\n") == 0
           || strcmp(tree->token, "(WHILE\n") == 0 || strcmp(tree->token, "(FOR\n") == 0
           || strcmp(tree->token,"(VARIABLE ") == 0 || strcmp(tree->token, "(FUNC\n") == 0 
           || strcmp(tree->token, "(PROC\n") == 0 ||strcmp(tree->token, "(CODE\n") == 0
           || strcmp(tree->token, "(CALL_FUNC\n") == 0 || strcmp(tree->token, "(RET ") == 0)
	{
                if(strcmp(tree->token, "(FUNC\n") == 0 || strcmp(tree->token, "(PROC\n") == 0)
                  {
                    count=2;
                    printTabs(count);
                    printf("%s",tree->token);
                    printTabs(count*2);
                  }
                else if(strcmp(tree->token,"(RET ")==0)
                  {
                    count++;
                    printTabs(count);
                    printf("%s",tree->token);
                  }
                else if(strcmp(tree->token,"(CALL_FUNC\n")==0)
                  {
                    printf("\n");
                    printTabs(count++);
                    printf("%s",tree->token);
                    printTabs(count*2);
                  }
                else if(strcmp(tree->token,"(VARIABLE ")==0)
                 {
                   printTabs(count++);
                   printf("%s",tree->token);
                 }
                else
                  {
                    count++;
                    printTabs(count);
                    printf("%s",tree->token);
                    printTabs(count*2);
                  }
		Closetype= CloseDown;	
        }
		else if (strcmp(tree->token, "(ARGS") == 0 && (tree->left))
	{
             {
		printf("(ARGS ");
                printTabs(count++);
                Closetype=CloseDown;
                count++;
             } 
	}
        else if (strcmp(tree->token, "(ARGS") == 0)
             {
                 printf("\n");
                 printTabs(count);
                 printf("(ARGS NONE)\n");
                 printTabs(count++);
             }
	else if(strcmp(tree->token, "{") == 0)
	{
                printf("(BLOCK\n");
                printTabs(count);	
	}

	else if(strcmp(tree->token, "}")== 0 ||strcmp(tree->token, ")")== 0)
	{
                printf(")\n");
                printTabs(count);
        }
       
	else if(strcmp(tree->token, "") == 0);

	else if(strcmp(tree->token, "(" )== 0 )
              {
                printf("\n");
                printTabs(count++);
                printf("%s",tree->token);
              }
        else if(strcmp(tree->token, ",") == 0)
                {
                printf("%s ",tree->token);
                }
	else if(strcmp(tree->token, "\n") == 0 || strcmp(tree->token, ";") == 0)
		{
                 printf("\n");
                 printTabs(count);
                }
       else if(strcmp(tree->token, "&&") == 0 || strcmp(tree->token, "/") == 0 ||  strcmp(tree->token, "=") == 0 || 
       strcmp(tree->token, "==") == 0 || strcmp(tree->token, ">") == 0 || strcmp(tree->token, ">=") == 0 ||  strcmp(tree->token, "<") == 0 || 
       strcmp(tree->token, "<=") == 0 || strcmp(tree->token, "-") == 0 || strcmp(tree->token, "!") == 0 || strcmp(tree->token, "!=") == 0 || 
       strcmp(tree->token, "||") == 0 || strcmp(tree->token, "+") == 0 || strcmp(tree->token, "*") == 0 || strcmp(tree->token, "&") == 0 || 
       strcmp(tree->token, "^") == 0 || strcmp(tree->token, "|") == 0 || strcmp(tree->token, ",") == 0 )
	{
                        printf("\n");
                        printTabs(count++);
			printf("(%s",tree->token);
			Closetype=CloseDown;
			if(strcmp(tree->token, "=") == 0)
			Closetype=CloseDown;
	}
	else
	{
		if(tree && (!tree->left && !tree->right) || strcmp(tree->token, "Main") == 0)

			printf("%s ", tree->token);
		else
		{
			
			printf("%s", tree->token);
			
		}
	}
	if (tree->left) 
	{
		
		Printtree(tree->left);
		
	}

	if (tree->right)
	{
		
		Printtree(tree->right);
		
	}


	if(Closetype == CloseDown)
		{
                 printf(")\n");
                 printTabs(count/2);
                }
	
}
int yyerror(char *e)
{
	int yydebug=1; 
	fflush(stdout);
	fprintf(stderr,"%s ERROR at line %d could not accept '%s'\n" ,e,yylineno,yytext);
        exit(1);
	return 0;
}
code* CodeBegin(code * codey)
{
	code * Current_Scoope=codey;
	if(Current_Scoope!=NULL)
	while(Current_Scoope->nextcode!=NULL)
		Current_Scoope=Current_Scoope->nextcode;
	return Current_Scoope;
}


void test(node *tree,code * Current_Scoope){

	if(strcmp(tree->token, "=") == 0 )
	{
		if(!(strcmp(Type_expr(tree->right,Current_Scoope),"NULL")==0&& (strcmp(Type_expr(tree->left,Current_Scoope),"real*")==0||strcmp(Type_expr(tree->left,Current_Scoope),"int*")==0||strcmp(Type_expr(tree->left,Current_Scoope),"char*")==0)))
		if(strcmp(Type_expr(tree->left,Current_Scoope),Type_expr(tree->right,Current_Scoope))!=0)
		{
			printf("ERROR, you could not asuume between %s and %s in %s\n",Type_expr(tree->left,Current_Scoope),Type_expr(tree->right,Current_Scoope),Start_Code->func[Start_Code->countfunc-1]->name);
			exit(1);
		}

	}
	else if(strcmp(tree->token, "var") == 0)
	{
		int countvar=0;
		Arguments * var=mkArgs(tree,&countvar);
		New_Var(var,countvar,0,Current_Scoope);
		
		
	}
	else if(strcmp(tree->token, "if") == 0)
	{
		if(strcmp(Type_expr(tree->left->left,Current_Scoope),"boolean")!=0)
		{
			printf("ERROR,if statment must be boolean statment\n");
			exit(1);
		}
		

		if(strcmp(tree->right->token,"{")!=0)
		{
			push(Current_Scoope,tree->token);
			if (tree->left) 
				test(tree->left,CodeBegin( Current_Scoope->nextcode));
	
			if (tree->right)
				test(tree->right,CodeBegin( Current_Scoope->nextcode));
        	scope--;
			return;
		}
		
		
		
	}
		else if(strcmp(tree->token, "while") == 0)
	{
		if(strcmp(Type_expr(tree->left->left,Current_Scoope),"boolean")!=0)
		{
			printf("ERROR,while statment must be boolean statment\n");
			exit(1);
		}

		if(strcmp(tree->right->token,"{")!=0)
		{
			push(Current_Scoope,tree->token);
			if (tree->left) 
				test(tree->left,CodeBegin( Current_Scoope->nextcode));
	
			if (tree->right)
				test(tree->right,CodeBegin( Current_Scoope->nextcode));
        	scope--;
			return;
		}
		
		
		
	}
	else if(strcmp(tree->token, "FUNC") == 0 )
	{
        int count=0;
		Arguments * arg=mkArgs(tree->left->right->left,&count);
		New_Func(tree->left->token,arg,tree->left->right->right->left,count,Current_Scoope);
		push(Current_Scoope,tree->token);
		New_Var(arg,count,1,CodeBegin(Current_Scoope));
	if (tree->left) 
		test(tree->left,CodeBegin( Current_Scoope->nextcode));
	
	if (tree->right)
		test(tree->right,CodeBegin( Current_Scoope->nextcode));
		if(Current_Scoope->func[Current_Scoope->countfunc-1]->get_ret==false)
		{
			printf("ERROR,there is no return in function ['%s']\n",tree->left->token);
			exit(1);
		}
        scope--;		
		return;
	}
    else if(strcmp(tree->token, "PROC") == 0)
	{
		
        int count=0;
		Arguments * arg=mkArgs(tree->right->left,&count);
		New_Func(tree->left->token,arg,NULL,count,Current_Scoope);
		push(Current_Scoope,tree->token);
		New_Var(arg,count,1,CodeBegin(Current_Scoope));
	if (tree->left) 
		test(tree->left,CodeBegin( Current_Scoope->nextcode));
	
	if (tree->right)
		test(tree->right,CodeBegin( Current_Scoope->nextcode));
		scope--;	
		return;
    }

	else if(strcmp(tree->token, "Call func") == 0)
	{
		int count=0;
		Search_Func(tree,Current_Scoope,&count);
		tree->sum=count;
	  }
	else if(strcmp(tree->token, "CODE") == 0)
	{

		push(NULL,tree->token);
	if (tree->left) 
		test(tree->left,Start_Code);
	
	if (tree->right)
		test(tree->right,Start_Code);
		scope--;
		return;
	}
    else if(strcmp(tree->token, "BODY") == 0)
	{  
		 
    }
	else if(strcmp(tree->token, "ARGS") == 0)
	{     
    }
    else if(strcmp(tree->token, "Main") == 0)
	{
		if(flagMain==true && strcmp(Current_Scoope->name,"CODE")==0)
		{
			printf("Main must use just one time in the hole Scoope and cant be used inside proc or func\n");
			exit(1);
		}
		flagMain=true;
		New_Func(tree->token,NULL,NULL,0,Current_Scoope);
		push(Current_Scoope,tree->token);

	if (tree->left) 
		test(tree->left,CodeBegin( Current_Scoope->nextcode));
	
	if (tree->right)
		test(tree->right,CodeBegin( Current_Scoope->nextcode));
        scope--;
		return;
               
    }       
	else if(strcmp(tree->token, "if-else") == 0)
	{
		if(strcmp(Type_expr(tree->left->left,Current_Scoope),"boolean")!=0)
		{
			printf("ERROR,if_else statment must be boolean statment\n");
			exit(1);
		}

		if(strcmp(tree->right->left->token,"{")!=0)
		{
			push(Current_Scoope,tree->token);
			test(tree->right->left,CodeBegin( Current_Scoope->nextcode));
			scope--;
			push(Current_Scoope,tree->token);
			test(tree->right->right->left,CodeBegin( Current_Scoope->nextcode));
        	scope--;
			return;
		}
		
		
		
	}
	else if(strcmp(tree->token, "return") == 0)
	{
		code * temp= Current_Scoope;
		int flag=true;
		while(strcmp(temp->name,"FUNC")!=0&&strcmp(temp->name,"PROC")!=0&&strcmp(temp->name,"CODE")!=0)
		{
			temp=temp->oldcode;
			flag=false;
		}
		if(flag==false)
		{
			if(strcmp(Type_expr(tree->left,Current_Scoope),temp->oldcode->func[temp->oldcode->countfunc-1]->returnType))
			{
			printf("ERROR,functin  %s should return %s not %s \n",temp->oldcode->func[temp->oldcode->countfunc-1]->name,temp->oldcode->func[temp->oldcode->countfunc-1]->returnType,Type_expr(tree->left,Current_Scoope));
			exit(1);
			}
		}
		else{
		if(temp->oldcode->func[temp->oldcode->countfunc-1]->returnType!=NULL){
		if(0==strcmp(Type_expr(tree->left,Current_Scoope),temp->oldcode->func[temp->oldcode->countfunc-1]->returnType)){
			temp->oldcode->func[temp->oldcode->countfunc-1]->get_ret=true;
		}
		else{
			printf("ERROR,func  %s should return %s not %s \n",temp->oldcode->func[temp->oldcode->countfunc-1]->name,temp->oldcode->func[temp->oldcode->countfunc-1]->returnType,Type_expr(tree->left,Current_Scoope));
			exit(1);
		}
		}
		else
		{
			printf("ERROR,proc %s must not return any value\n",temp->oldcode->func[temp->oldcode->countfunc-1]->name);
			exit(1);
		}  
		}

	}
	else if(strcmp(tree->token, "{") == 0)
	{
    push(Current_Scoope,tree->token);
	if (tree->left) 
		test(tree->left,CodeBegin( Current_Scoope->nextcode));
	
	if (tree->right)
		test(tree->right,CodeBegin( Current_Scoope->nextcode));
        scope--;
		return;							
	}
	else if(strcmp(tree->token, "solovar") == 0 )
	{
		Search_Var(tree->left,Current_Scoope);
	}
	if (tree->left) 
		test(tree->left,Current_Scoope);
	
	if (tree->right)
		test(tree->right,Current_Scoope);

}

int POPParams(Arguments * args,int count){
	int size=0;
	for(int i =0;i<count;i++)
	{
		if(strcmp(args[i].type,"int")==0)
			size += 4;
		else if(strcmp(args[i].type,"char")==0)
			size += 1;
		else if(strcmp(args[i].type,"real")==0)
			size += 8;
		else if(strcmp(args[i].type,"string")==0)
			size += atoi(args[i].ext);
		else if(strcmp(args[i].type,"boolean")==0)
			size += 4;
		else if(strcmp(args[i].type,"int*")==0)
			size += 4;
		else if(strcmp(args[i].type,"char*")==0)
			size += 4;
		else if(strcmp(args[i].type,"real*")==0)
			size += 4;
	}
	return size;
}

void InsertCode(node* node,char *code,char *var,char *label,char *LblT,char *LblF)
	{
		if(code!=NULL){
		node->My_code=(char*)malloc(sizeof(char)*(1+strlen(code)));
		strcpy(node->My_code,code);
		}
		else if(node->My_code==NULL)
		{
		node->My_code=(char*)malloc(sizeof(char)*2);
		strcpy(node->My_code,"");
		}

		if(var!=NULL){
		node->var=(char*)malloc(sizeof(char)*(1+strlen(var)));
		strcpy(node->var,var);
		}
		else if(node->var==NULL)
		{
		node->var=(char*)malloc(sizeof(char)*2);
		strcpy(node->var,"");
		}

		if(label!=NULL&& node->label==NULL){
		node->label=(char*)malloc(sizeof(char)*(1+strlen(label)));
		strcpy(node->label,label);
		}

		if(LblT!=NULL && node->LblT==NULL){
		node->LblT=(char*)malloc(sizeof(char)*(1+strlen(LblT)));
		strcpy(node->LblT,LblT);
		}
		
		if(LblF!=NULL && node->LblF==NULL){
		node->LblF=(char*)malloc(sizeof(char)*(1+strlen(LblF)));
		strcpy(node->LblF,LblF);
		}

	}
void InsertCode2(node* node,char *code,char *var,char *label,char *LblT,char *LblF)
	{
		if(code!=NULL){
		node->My_code=(char*)malloc(sizeof(char)*(1+strlen(code)));
		strcpy(node->My_code,code);
		}
		else if(node->My_code==NULL)
		{
		node->My_code=(char*)malloc(sizeof(char)*2);
		strcpy(node->My_code,"");
		}

		if(var!=NULL){
		node->var=(char*)malloc(sizeof(char)*(1+strlen(var)));
		strcpy(node->var,var);
		}
		else if(node->var==NULL)
		{
		node->var=(char*)malloc(sizeof(char)*2);
		strcpy(node->var,"");
		}

		if(label!=NULL){
		node->label=(char*)malloc(sizeof(char)*(1+strlen(label)));
		strcpy(node->label,label);
		}

		if(LblT!=NULL){
		node->LblT=(char*)malloc(sizeof(char)*(1+strlen(LblT)));
		strcpy(node->LblT,LblT);
		}
		
		if(LblF!=NULL && node->LblF==NULL){
		node->LblF=(char*)malloc(sizeof(char)*(1+strlen(LblF)));
		strcpy(node->LblF,LblF);
		}

	}
	char* Load_Var(){
		char* x;
		asprintf(&x,"t%d",t++);
		return x;
	}
	char* Load_Lbl(){
		char* x;
		asprintf(&x,"_L%d",l++);
		return x;
	}
	char* Many_G(char*s1,char*s2,char*s3,char*s4,char*s5)
	{
		char* x;

		asprintf(&x,"%s %s %s %s %s\n",s1,s2,s3,s4,s5);
		return x;
	}
	
char* F_Stract(char*des,char*src){
		char* tamp=des;
		char* num;
		asprintf(&num,"%d ",line++);
		if(src!=NULL){
			if(des==NULL){
				des=(char*)malloc((strlen(src)+1)*sizeof(char));
				strcpy(des,src);
				return des;
			}
		des=(char*)malloc((strlen(des)+strlen(src)+1+strlen(num))*sizeof(char));
		if(tamp!=NULL){
		strcat(des,tamp);
		}
		if(src!=NULL)
		{
		strcat(des,src);
		}
		}
		return des;
	}
char *Replace(const char *s, const char *oldW, const char *newW) 
{ 
    char *result; 
    int i, cnt = 0; 
    int newWlen = strlen(newW); 
    int oldWlen = strlen(oldW);  
    for (i = 0; s[i] != '\0'; i++) 
    { 
        if (strstr(&s[i], oldW) == &s[i]) 
        { 
            cnt++; 
            i += oldWlen - 1; 
        } 
    } 
    result = (char *)malloc(i + cnt * (newWlen - oldWlen) + 1); 
  
    i = 0; 
    while (*s) 
    { 
        if (strstr(s, oldW) == s) 
        { 
            strcpy(&result[i], newW); 
            i += newWlen; 
            s += oldWlen; 
        } 
        else
            result[i++] = *s++; 
    } 
  
    result[i] = '\0'; 
    return result; 
} 

 
char * mkspace(char *label)
{
	char * msg;
	char x=' ';
	int ext = strlen(F_Stract(label,"\0"));
	if(label==NULL || !strcmp(label,""))
		msg="";
	else
		{
		asprintf(&msg,"%c",x);
		while(ext<5){
			asprintf(&msg,"%c%s",x,msg);
			ext++;
		}
		asprintf(&label,"%s: ",F_Stract(label,"\0"));
		msg=F_Stract(msg,label);
		}
		return msg;
}


void OutputOf3AC(node * tree)
{ 
	
	
	if(strcmp(tree->token, "=") == 0 )
	{ if(tree->left!=NULL) OutputOf3AC(tree->left); if(tree->right!=NULL) OutputOf3AC(tree->right);
	   InsertCode(tree,F_Stract(tree->right->My_code,Many_G(tree->left->var,"=",tree->right->var,"","")),NULL,NULL,NULL,NULL); 
	   return;  
	}
	else if(strcmp(tree->token, "if") == 0)
	{ 
		if(tree->left->left)
		InsertCode(tree->left->left,NULL,NULL,NULL,NULL,tree->label);
		if(tree->right)
		InsertCode2(tree->right,NULL,NULL,tree->label,NULL,NULL);
		if(tree->left!=NULL) OutputOf3AC(tree->left); if(tree->right!=NULL) OutputOf3AC(tree->right);
		InsertCode(tree,F_Stract(tree->left->left->My_code,F_Stract(mkspace(tree->left->left->label),F_Stract(mkspace(tree->left->left->LblT),F_Stract(tree->right->My_code,mkspace(tree->LblT))))),NULL,NULL,NULL,NULL);
		return;
	}
else if(strcmp(tree->token, "if-else") == 0)
	{ 
		if(tree->right->left)
		InsertCode(tree->right->left,NULL,NULL,tree->label,NULL,NULL);			
		if(tree->right->right->left)
		InsertCode2(tree->right->right->left,NULL,NULL,tree->label,NULL,tree->label);
		if(tree->right->left)
		InsertCode2(tree->right->left,NULL,NULL,tree->label,NULL,tree->label);
		
		if(tree->left!=NULL) OutputOf3AC(tree->left); if(tree->right!=NULL) OutputOf3AC(tree->right);
		InsertCode(tree,F_Stract(F_Stract(tree->left->left->My_code,F_Stract(mkspace(tree->left->left->LblT),tree->right->left->My_code))
		,F_Stract(F_Stract("Goto ",F_Stract(F_Stract(tree->label,"\n"),F_Stract(mkspace(tree->left->left->LblF),tree->right->right->left->My_code))),mkspace(tree->label))),NULL,NULL,NULL,NULL);
	return;
	}
	else if(strcmp(tree->token, "while") == 0)
	{ 
		if(tree->left->left)
			InsertCode(tree->left->left,NULL,NULL,NULL,tree->LblF,tree->label);
		if(tree->right)
			InsertCode2(tree->right,NULL,NULL,tree->label,NULL,NULL);
		
		if(tree->left!=NULL) OutputOf3AC(tree->left); if(tree->right!=NULL) OutputOf3AC(tree->right);
			
			InsertCode(tree,F_Stract(F_Stract(F_Stract( mkspace(tree->LblT),tree->left->left->My_code),mkspace(tree->LblF)),
				F_Stract(tree->right->My_code,F_Stract(F_Stract("\tGoto ",F_Stract(tree->LblT,"\n")),mkspace(tree->label)))),NULL,NULL,NULL,NULL);
		return ;
	}
	else if(strcmp(tree->token, "stmnts") == 0)
	{ 
		
		if(tree->right!=NULL)
			if(strcmp(tree->right->token, "if-else") == 0||strcmp(tree->right->token, "while") == 0)
				InsertCode2(tree->right,NULL,NULL,tree->label,NULL,NULL);
        if(tree->right!=NULL && tree->left!=NULL)
        if(strcmp(tree->left->right->token, "if") == 0||strcmp(tree->left->right->token, "if-else") == 0||strcmp(tree->left->right->token, "while") == 0)
				InsertCode2(tree->right,NULL,NULL,NULL,tree->left->right->label,NULL);
		if(tree->left!=NULL) OutputOf3AC(tree->left); if(tree->right!=NULL) OutputOf3AC(tree->right);

			if(tree->right!=NULL && tree->left!=NULL)
                if((strcmp(tree->right->token, "while") == 0||strcmp(tree->right->token, "if-else") == 0)&&(strcmp(tree->left->right->token, "if") == 0||strcmp(tree->left->right->token, "if-else") == 0||strcmp(tree->left->right->token, "while") == 0))
                    InsertCode(tree,F_Stract(tree->left->My_code,&tree->right->My_code[8]),NULL,NULL,NULL,NULL);
                    else
					InsertCode(tree,F_Stract(tree->left->My_code,tree->right->My_code),NULL,NULL,NULL,NULL);
			else if(tree->right!=NULL)
            {
                if(strcmp(tree->right->token, "if-else") == 0||strcmp(tree->right->token, "while") == 0)
                    InsertCode(tree,tree->right->My_code,NULL,NULL,NULL,NULL);
                    else        
				    InsertCode(tree,F_Stract(tree->right->My_code ,mkspace(tree->right->label)),NULL,NULL,NULL,NULL);
            }else
				InsertCode(tree,"",NULL,NULL,NULL,NULL);
			
	return;
		
	}
    else if(strcmp(tree->token, "PROC") == 0)
	{ if(tree->left!=NULL) OutputOf3AC(tree->left); if(tree->right!=NULL) OutputOf3AC(tree->right);
	 char*x; asprintf(&x," %s:\n",tree->left->token);InsertCode(tree,F_Stract(x,tree->right->right->My_code),NULL,NULL,NULL,NULL);
		return;
	}
	else if(strcmp(tree->token, "FUNC") == 0)
	{
		 if(tree->left!=NULL) OutputOf3AC(tree->left); if(tree->right!=NULL) OutputOf3AC(tree->right);
		
	 char*x; asprintf(&x," %s:\n",tree->left->token);InsertCode(tree,F_Stract(x,tree->right->left->My_code),NULL,NULL,NULL,NULL);
		return;
	}
		else if(strcmp(tree->token, "exprs_gram") == 0)
	{
		if(tree->left!=NULL) OutputOf3AC(tree->left); if(tree->right!=NULL) OutputOf3AC(tree->right);
			if(tree->right==NULL)
				InsertCode(tree,F_Stract(tree->left->My_code,F_Stract("PushParam ",F_Stract(tree->left->var,"\n"))),NULL,NULL,NULL,NULL);
			else
				InsertCode(tree,F_Stract(F_Stract(tree->left->My_code,F_Stract("PushParam ",F_Stract(tree->left->var,"\n"))),tree->right->left->My_code),NULL,NULL,NULL,NULL);
	}	
	else if(strcmp(tree->token, "Call func") == 0)
	{ 

		if(tree->left!=NULL) OutputOf3AC(tree->left); if(tree->right!=NULL) OutputOf3AC(tree->right);
		char * x,*parm=(char*)malloc(sizeof(char));
		if(tree->right->left==NULL)
			strcpy(parm,"");
		else
			parm=tree->right->left->My_code;
		InsertCode(tree,NULL,Load_Var(),NULL,NULL,NULL);
		asprintf(&x,"%s%s = LCALL %s\n‪\tPopParams %d‬‬‬‬\n",parm,tree->var,tree->left->token,tree->sum);
		InsertCode(tree,x,NULL,NULL,NULL,NULL);
		return;
	}
	else if(strcmp(tree->token, "CODE") == 0)
	{ if(tree->left!=NULL) OutputOf3AC(tree->left); if(tree->right!=NULL) OutputOf3AC(tree->right);
		 if(tree->left)
			InsertCode(tree,F_Stract(tree->left->My_code,tree->right->My_code),NULL,NULL,NULL,NULL);
		else
			InsertCode(tree,tree->right->My_code,NULL,NULL,NULL,NULL);
		tree->My_code=Replace(tree->My_code, "\n\n", "\n") ;
		char x='a',*y,*z;

		while (x<='z')
		{
			asprintf(&y,"\n%c",x);
			asprintf(&z,"\n\t%c",x);
			tree->My_code=Replace(tree->My_code, y, z) ;
			x++;
		}
		x='A';
				while (x<='Z')
		{
			asprintf(&y,"\n%c",x);
			asprintf(&z,"\n\t%c",x);
			tree->My_code=Replace(tree->My_code, y, z) ;
			x++;
		}
		return;
	}
    else if(strcmp(tree->token, "BODY") == 0)
	{ 
		
		if(tree->left!=NULL) OutputOf3AC(tree->left); if(tree->right!=NULL) OutputOf3AC(tree->right);
		if(tree->right->right->left){
		
		if(tree->right->right->left->My_code[strlen(F_Stract(tree->right->right->left->My_code,"\0"))-2]==':')
			InsertCode(tree,F_Stract(F_Stract("\tBeginFunc‬‬\n",tree->right->right->left->My_code),"EndFunc\n"),NULL,NULL,NULL,NULL);
		else
		    InsertCode(tree,F_Stract(F_Stract("\tBeginFunc‬‬\n",tree->right->right->left->My_code),"\tEndFunc\n"),NULL,NULL,NULL,NULL);
		}
		else
			 InsertCode(tree,F_Stract("\tBeginFunc‬‬\n","\tEndFunc\n"),NULL,NULL,NULL,NULL);
		
		return;
	}
    else if(strcmp(tree->token, "Main") == 0)
	{ 
		 
		if(tree->left!=NULL) OutputOf3AC(tree->left); if(tree->right!=NULL) OutputOf3AC(tree->right);
		
		InsertCode(tree,F_Stract(" Main:\n",tree->left->right->My_code),NULL,NULL,NULL,NULL);
          return;   
    } 
	    else if(strcmp(tree->token, "procedures") == 0)
	{ if(tree->left!=NULL) OutputOf3AC(tree->left); if(tree->right!=NULL) OutputOf3AC(tree->right);
		if(tree->left!=NULL) InsertCode(tree,F_Stract(tree->left->My_code,tree->right->My_code),NULL,NULL,NULL,NULL);else InsertCode(tree,tree->right->My_code,NULL,NULL,NULL,NULL);
    return;
	}        

	else if(strcmp(tree->token, "return") == 0)
	{
		
		 if(tree->left!=NULL) OutputOf3AC(tree->left); if(tree->right!=NULL) OutputOf3AC(tree->right);

		InsertCode(tree,F_Stract(tree->left->My_code,Many_G("return",tree->left->var,"","","")),NULL,NULL,NULL,NULL);
		return;
	}
	else if(strcmp(tree->token, "{") == 0)
	{ 
		if(tree->right->right->left) InsertCode(tree,NULL,NULL,tree->right->right->left->label,tree->right->right->left->LblT,tree->right->right->left->LblF); 
		if(tree->left!=NULL) OutputOf3AC(tree->left); if(tree->right!=NULL) OutputOf3AC(tree->right);
		if(tree->right->right->left) InsertCode(tree,tree->right->right->left->My_code,tree->right->right->left->var,NULL,NULL,NULL); 
		return;			
	}
	else if(strcmp(tree->token, "}") == 0)
	{ if(tree->left!=NULL) OutputOf3AC(tree->left); if(tree->right!=NULL) OutputOf3AC(tree->right);
                      
                      
    }
	else if(strcmp(tree->token, "assmnt_stmnt") == 0)
	{ 
          if(tree->left!=NULL) OutputOf3AC(tree->left); if(tree->right!=NULL) OutputOf3AC(tree->right);
          InsertCode(tree,tree->left->My_code,tree->left->var,tree->left->label,tree->left->LblT,tree->left->LblF); 
	return;                 
        }
	
    else if(strcmp(tree->token, "+") == 0 || strcmp(tree->token, "*") == 0 || strcmp(tree->token, "-") == 0 || strcmp(tree->token, "/") == 0 )
	{ 	
		if(tree->left!=NULL) OutputOf3AC(tree->left); if(tree->right!=NULL) OutputOf3AC(tree->right);
		InsertCode(tree,NULL,Load_Var(),NULL,NULL,NULL);
		InsertCode(tree,F_Stract(F_Stract(tree->left->My_code,tree->right->My_code),Many_G(tree->var,"=",tree->left->var,tree->token,tree->right->var)),NULL,NULL,NULL,NULL);return;}
	else if(strcmp(tree->token, "&") == 0)
	{ 
		if(tree->left!=NULL) OutputOf3AC(tree->left); if(tree->right!=NULL) OutputOf3AC(tree->right);
				if((tree->left->left == NULL))
				InsertCode(tree,"",F_Stract("&",(tree->left->token)),NULL,NULL,NULL);
			else if(strcmp(tree->left->left->token,"[")==0)
					{
						char *x,*fv1,*fv2;
						asprintf(&fv1,"%s",Load_Var()); 
						asprintf(&fv2,"%s",Load_Var());
						asprintf(&x,"\t%s = &%s\n\t%s = %s + %s\n",fv1,tree->left->token,fv2,fv1,tree->left->left->left->var);
						InsertCode(tree,F_Stract(tree->left->left->left->My_code,x),fv2,NULL,NULL,NULL);
					}
				else if (tree->left->left->left==NULL)
				InsertCode(tree,"",F_Stract("&",(tree->left->left->token)),NULL,NULL,NULL);
			else
			{
				char *x,*fv1,*fv2;
				asprintf(&fv1,"%s",Load_Var());
				asprintf(&fv2,"%s",Load_Var()); 
				asprintf(&x,"\t%s = &%s\n\t%s = %s + %s\n",fv1,tree->left->left->token,fv2,fv1,tree->left->left->left->left->var); 
				InsertCode(tree,F_Stract(tree->left->left->left->left->My_code,x),fv2,NULL,NULL,NULL);
			}
			
			

	return;}
	else if(strcmp(tree->token, "^") == 0 )
	{ 
		if(tree->left!=NULL) OutputOf3AC(tree->left); if(tree->right!=NULL) OutputOf3AC(tree->right);
			if((tree->left->left == NULL))
				InsertCode(tree,"",F_Stract("*",(tree->left->token)),NULL,NULL,NULL);
			else
			{
				InsertCode(tree,"",F_Stract("*",(tree->left->left->token)),NULL,NULL,NULL);
			}
			
	return;}
	else if(strcmp(tree->token, "==") == 0 || 
			strcmp(tree->token, ">") == 0 || 
			strcmp(tree->token, ">=") == 0 || 
			strcmp(tree->token, "<") == 0 || 
			strcmp(tree->token, "<=") == 0 || 
			strcmp(tree->token, "!=") == 0) 
	{ 

		if(tree->left!=NULL) OutputOf3AC(tree->left); if(tree->right!=NULL) OutputOf3AC(tree->right);
				InsertCode(tree,F_Stract(F_Stract(tree->left->My_code,tree->right->My_code),F_Stract(Many_G("if",tree->left->var,tree->token,tree->right->var,F_Stract("Goto ",F_Stract(tree->LblT,"\n")))
				,F_Stract("\tGoto ",F_Stract(tree->LblF,"\n")))),NULL,NULL,NULL,NULL);

				
	return;}
	else if(strcmp(tree->token, "(") == 0)
	{
			InsertCode(tree->left,NULL,NULL,NULL,tree->LblT,tree->LblF);
		if(tree->left!=NULL) OutputOf3AC(tree->left); if(tree->right!=NULL) OutputOf3AC(tree->right);
		InsertCode(tree,tree->left->My_code,tree->left->var,NULL,NULL,NULL);
	return;}
	else if(strcmp(tree->token, "!") == 0)
	{ 
		InsertCode(tree->left,NULL,NULL,NULL,tree->LblT,tree->LblF);
		if(tree->left!=NULL) OutputOf3AC(tree->left); if(tree->right!=NULL) OutputOf3AC(tree->right);
				
		 InsertCode(tree,tree->left->My_code,NULL,NULL,NULL,NULL);
		
	return;}
	else if(strcmp(tree->token, "||") == 0) 
	{
		InsertCode(tree->left,NULL,NULL,NULL,tree->LblT,NULL);
		InsertCode(tree->right,NULL,NULL,NULL,tree->LblT,tree->LblF);
		if(tree->left!=NULL) OutputOf3AC(tree->left); if(tree->right!=NULL) OutputOf3AC(tree->right);
		InsertCode(tree,F_Stract(tree->left->My_code,F_Stract(mkspace(tree->left->LblF),tree->right->My_code)),NULL,NULL,NULL,NULL);
	return;}
	else if(strcmp(tree->token, "&&") == 0 )
	{
		
		InsertCode(tree->left,NULL,NULL,NULL,NULL,tree->LblF);
		InsertCode(tree->right,NULL,NULL,NULL,tree->LblT,tree->LblF);
		if(tree->left!=NULL) OutputOf3AC(tree->left); if(tree->right!=NULL) OutputOf3AC(tree->right);
			InsertCode(tree,F_Stract(tree->left->My_code,F_Stract(mkspace(tree->left->LblT),tree->right->My_code)),NULL,NULL,NULL,NULL);
	return;}
	else if(strcmp(tree->token, "null") == 0 )
	{ 
		if(tree->left!=NULL) OutputOf3AC(tree->left); if(tree->right!=NULL) OutputOf3AC(tree->right);
		InsertCode(tree,"",tree->token,NULL,NULL,NULL);
	return;}	
	else if(strcmp(tree->token, "solovar") == 0 )
	{ 
		if(tree->left!=NULL) OutputOf3AC(tree->left); if(tree->right!=NULL) OutputOf3AC(tree->right);
			if(tree->left->left==NULL)
				InsertCode(tree,"",tree->left->token,NULL,NULL,NULL);
			else
			{
				char *x,*fv1,*fv2; asprintf(&fv1,"%s",Load_Var()); asprintf(&fv2,"%s",Load_Var()); asprintf(&x,"\t%s = &%s\n\t%s = %s + %s\n",fv1,tree->left->token,fv2,fv1,tree->left->left->left->var); InsertCode(tree,F_Stract(tree->left->left->left->My_code,x),F_Stract("*",fv2),NULL,NULL,NULL);
			}
			
	return;}
	else if((tree->left!=NULL)&&
			(strcmp(tree->left->token,"INT")==0||
			strcmp(tree->left->token,"HEX")==0||
			strcmp(tree->left->token,"CHAR")==0||
			strcmp(tree->left->token,"REAL")==0||
			strcmp(tree->left->token,"STRING")==0||
			strcmp(tree->left->token,"BOOLEAN")==0))
			{

			if(tree->left!=NULL) OutputOf3AC(tree->left); if(tree->right!=NULL) OutputOf3AC(tree->right);
			if(strcmp(tree->left->token,"STRING")==0)
			InsertCode(tree,"",tree->token,NULL,NULL,NULL);
			else
			if(strcmp(tree->left->token,"BOOLEAN")==0)
			{
			if(strcmp(tree->token,"true")==0 && tree->LblT!=NULL)	
			InsertCode(tree,F_Stract("Goto ",F_Stract(tree->LblT,"\n")),tree->token,NULL,NULL,NULL);
			else if(strcmp(tree->token,"false")==0 && tree->LblF!=NULL)
			InsertCode(tree,F_Stract("Goto ",F_Stract(tree->LblF,"\n")),tree->token,NULL,NULL,NULL);
			else
			InsertCode(tree,tree->token,tree->token,NULL,NULL,NULL);
			}
			else
			InsertCode(tree,"",tree->token,NULL,NULL,NULL);
			return;}
	else if(strcmp(tree->token, "") == 0||strcmp(tree->token, " ") == 0)
	{
		
		if(tree->left)
		InsertCode(tree->left,NULL,NULL,tree->label,tree->LblT,tree->LblF);
		if(tree->right)
		InsertCode(tree->right,NULL,NULL,tree->label,tree->LblT,tree->LblF);
		if(tree->left!=NULL) OutputOf3AC(tree->left); if(tree->right!=NULL) OutputOf3AC(tree->right);
		if(tree->left && tree->right)
			InsertCode(tree,F_Stract(tree->left->My_code,tree->right->My_code),tree->right->var,NULL,NULL,NULL);
		else if(tree->left)
			InsertCode(tree,tree->left->My_code,tree->left->var,NULL,NULL,NULL);	
		else if(tree->right)
			InsertCode(tree,tree->right->My_code,tree->right->var,NULL,NULL,NULL);	
	return;
	}
	else
	{
	if (tree->left) 
		OutputOf3AC(tree->left);
	
	if (tree->right)
		OutputOf3AC(tree->right);
InsertCode(tree,"",tree->token,NULL,NULL,NULL);
	return;}
	if (tree->left) 
		OutputOf3AC(tree->left);
	
	if (tree->right)
		OutputOf3AC(tree->right);
}
