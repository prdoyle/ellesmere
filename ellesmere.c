
#include "lex.h"
#include "lex.l.h"
#include "symbols.h"

static token_t nextToken(){ return (token_t)yylex(); }

int main(int argc, char **argv)
	{
	SymbolTable st = theSymbolTable();
	token_t token = nextToken();
	while (token)
		{
		switch (token)
			{
			case ERROR:
			case NUM_TOKENS:
				printf("Error: <<%s>>\n", lastString());
				break;
			case NO_TOKEN:
				printf("No token!\n");
				break;
			case INT:
				printf("Int: %d\n", lastInt());
				break;
			case STRING:
				printf("String: %s\n", lastString());
				break;
			case WORD:
				{
				Symbol sy = st_byName(lastWord(), st);
				printf("Word #%d %s\n", sy_index(sy, st), sy_name(sy, st));
				break;
				}
			}
		token = nextToken();
		}
	}

//MERGE:30


