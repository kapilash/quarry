#include "quarry_testsuites.h"
#include "lexers.h"
#include "quarry_internal.h"


START_TEST(test_identifier1)
{
  unsigned char *input;
  int size,index,retval,expLine;
  quarry_QuarryPtr quarry;
  size = 17;

  printf("\nRunning tests on identifier.");
  input = (unsigned char *)malloc(size*sizeof(unsigned char));
  input[0] = 'x';
  for(index = 1;index<10;index++){
    input[index] = 47+index;
  }
  input[10]='A';
  input[11] = 'a';
  input[12] = 'C';
  input[13] = 'Z';
  input[14] = '_';
  input[15] = 'z';
  input[16] = '0';

  quarry = quarry_makeQuarry(input,size);
  quarry->input.index = 0;

  retval = quarry_idLexer(quarry,0);
  //quarry_printQuarry(quarry);
  fail_unless((retval != 0),"expected non-zero");
  printf(".");
  quarry->input.index = 0;
  quarry->input.data[3]=';';
  retval = quarry_idLexer(quarry,retval);
  //quarry_printQuarry(quarry);
  fail_unless((retval == 0),"expected zero");
  fail_unless((quarry->slabType == quarry_Identifier),"Expected identifier");
  fail_unless((quarry->holder.length == 20),"Expected size is 20");
  fail_unless((quarry->input.index == 3),"Expected input.index is 3");
  free(input);
}
END_TEST

START_TEST(test_identifierKW)
{
  unsigned char *input;
  int size,index,retval,expLine,currKWsize;
  quarry_QuarryPtr quarry;
  size = 20;

  printf("\nRunning tests on identifier.");
  input = (unsigned char *)malloc(size*sizeof(unsigned char));
  for(index = 0;index<size;index++){
    input[index] = ' ';
  }
  currKWsize = strlen("abstract");
  memcpy(input,"abstract",currKWsize);
  quarry = quarry_makeQuarry(input,size);
  quarry->input.index = 0;
  quarry->kwTable = quarry_util_keywordTableJava();

  retval = quarry_idLexer(quarry,0);
  //  quarry_printQuarry(quarry);
  fail_unless((retval == 0),"expected zero");
  fail_unless((quarry->slabType == quarry_Keyword),"Expected keyword");
  fail_unless((quarry->holder.length == currKWsize),"Expected size is currKWsize");
  free(input);
}
END_TEST


void quarry_addIdentifierTests(Suite *suite)
{
  TCase *tc_core = tcase_create("identifiers");
  tcase_add_test (tc_core,test_identifier1);
  tcase_add_test (tc_core,test_identifierKW);
  suite_add_tcase(suite,tc_core);
}
