#include "quarry_internal.h"

int quarry_commentLexer(quarry_QuarryPtr quarryPointer, int lexerState);

int quarry_punctuation(quarry_QuarryPtr quarry, int lexerState);

int quarry_operatorLexer(quarry_QuarryPtr quarry, int lexerState);

int quarry_groupLexer(quarry_QuarryPtr quarry, int lexerState);

int quarry_wsLexer(quarry_QuarryPtr quarry, int lexerState);

int quarry_sqLexer(quarry_QuarryPtr quarry, int lexerState);

int quarry_dqLexer(quarry_QuarryPtr quarry, int lexerState);

int quarry_decimalLexer(quarry_QuarryPtr quarry, int lexerState);

int quarry_hexBinOct0Lexer(quarry_QuarryPtr quarry, int lexerState);

int quarry_lfLexer(quarry_QuarryPtr quarry, int lexerState);

int quarry_crlfLexer(quarry_QuarryPtr quarry, int lexerState);

int quarry_errLexer(quarry_QuarryPtr quarry,int lexerState);

int quarry_idLexer(quarry_QuarryPtr quarry, int lexerState);
