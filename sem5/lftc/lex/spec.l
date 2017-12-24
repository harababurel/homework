digit                    [0-9]
lowercase_letter         [a-z]
uppercase_letter         [A-Z]
letter                   {uppercase_letter}|{lowercase_letter}
identifier               {letter}{1,7}({letter}|{digit})?
positive_digit           [1-9]
integer_number           {positive_digit}({digit}*)
integer                  (-?{integer_number})|0
real_number              -?{integer}"."{digit}*
boolean_constant         true|false
reserved_word            int|bool|float|if|else|while|cin|cout|endl|include|iostream|return|main
separator                \+|\-|\*|\/|=|\<|\>|==|\>=|\<=|!=|\>\>|\<\<|\{|\}|;|\(|\)|\[|\]|\.|#

%{
  char symbol_table_consts[100][9];
  int last_id_consts = 0;
  char symbol_table_identifiers[100][9];
  int last_id_identifiers = 0;
%}

%%
{identifier}             {
                            printf("IDENTIFIER          %s\n", yytext);
                            int found = 0;
                            for (int i = 0; i < last_id_identifiers; i++) {
                              if (strcmp(symbol_table_identifiers[i], yytext) == 0) {
                                found = 1;
                                break;
                              }
                            }
                            if (!found) {
                              strcpy(symbol_table_identifiers[last_id_identifiers++], yytext);
                            }
                         }
{integer}                {
                            printf("INTEGER             %s\n", yytext);
                            int found = 0;
                            for (int i = 0; i < last_id_consts; i++) {
                              if (strcmp(symbol_table_consts[i], yytext) == 0) {
                                found = 1;
                                break;
                              }
                            }
                            if (!found) {
                              strcpy(symbol_table_consts[last_id_consts++], yytext);
                            }
                         }
{real_number}            {
                            printf("REAL NUMBER             %s\n", yytext);
                            int found = 0;
                            for (int i = 0; i < last_id_consts; i++) {
                              if (strcmp(symbol_table_consts[i], yytext) == 0) {
                                found = 1;
                                break;
                              }
                            }
                            if (!found) {
                              strcpy(symbol_table_consts[last_id_consts++], yytext);
                            }
                         }
{boolean_constant}       {
                            printf("BOOLEAN                 %s\n", yytext);
                            int found = 0;
                            for (int i = 0; i < last_id_consts; i++) {
                              if (strcmp(symbol_table_consts[i], yytext) == 0) {
                                found = 1;
                                break;
                              }
                            }
                            if (!found) {
                              strcpy(symbol_table_consts[last_id_consts++], yytext);
                            }
                         }
{reserved_word}          printf("RESERVED WORD       %s\n", yytext);
{separator}              printf("SEPARATOR           %s\n", yytext);
[ \n\t\r]+               printf("");
.                        printf("INVALID CHARACTER:  %s\n", yytext);
%%

int main(int argc, char *argv[]) {
    yyin = fopen(argv[1], "r");
    yylex();

    printf("\nConstant Symbol Table\n");
    for (int i = 0; i < last_id_consts; i++) {
      printf("%d    -    %s\n", i, symbol_table_consts[i]);
    }

    printf("\nIdentifier symbol table\n");
    for (int i = 0; i < last_id_identifiers; i++) {
      printf("%d    -    %s\n", i, symbol_table_identifiers[i]);
    }

    fclose(yyin);
}
