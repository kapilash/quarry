#include "quarry_testsuites.h"
#include "lexers.h"
#include "quarry_internal.h"


START_TEST(test_EmptyDoubleQuotes1)
{
  unsigned char *input;
  int size,index,retval,expLine;
  quarry_QuarryPtr quarry;
  size = 5;

  printf("\nRunning tests on double quotes.");
  input = (unsigned char *)malloc(size*sizeof(unsigned char));
  input[0]='"';
  input[1] = '"';
  input[2] = '\n';
  input[3] = 'a';
  input[4] = 'b';


  quarry = quarry_makeQuarry(input,size);
  retval = quarry_dqLexer(quarry,0);
  fail_unless((quarry->line == 1),"Line # mismatch");
  fail_unless((quarry->slabType == quarry_String),"Expected string");
  fail_unless((quarry->holder.length == 0),"Expected size is 0");
  fail_unless((quarry->input.index == 2),"Expected input.index is 2");

}
END_TEST


START_TEST(test_EmptyDoubleQuotes2)
{
  unsigned char *input;
  int size,index,retval,expLine;
  quarry_QuarryPtr quarry;
  size = 5;

  printf(".");
  input = (unsigned char *)malloc(size*sizeof(unsigned char));
  input[0]='"';
  input[1] = '"';
  input[2] = '\n';
  input[3] = 'a';
  input[4] = 'b';


  quarry = quarry_makeQuarry(input,size);
  quarry->input.index = 1;
  retval = quarry_dqLexer(quarry,1);
  fail_unless((quarry->line == 1),"Line # mismatch");
  fail_unless((quarry->slabType == quarry_String),"Expected string");
  fail_unless((quarry->holder.length == 0),"Expected size is 0");
  fail_unless((quarry->input.index == 2),"Expected input.index is 2");

}
END_TEST

START_TEST(test_DoubleQuotesNewLines)
{
  unsigned char *input;
  int size,index,retval,expLine;
  quarry_QuarryPtr quarry;
  size = 5;

  printf(".");
  input = (unsigned char *)malloc(size*sizeof(unsigned char));
  input[0]='"';
  input[1] = '\n';
  input[2] = '\n';
  input[3] = '\n';
  input[4] = '"';


  quarry = quarry_makeQuarry(input,size);
  retval = quarry_dqLexer(quarry,0);
  fail_unless((quarry->line == 4),"Line # mismatch");
  fail_unless((quarry->slabType == quarry_String),"Expected string");
  fail_unless((quarry->holder.length == 3),"Expected size is 0");
  fail_unless((quarry->input.index == 5),"Expected input.index is 4");

}
END_TEST


START_TEST(test_HolderLatestSlash1)
{
  unsigned char *input;
  int size,index,retval,expLine;
  quarry_QuarryPtr quarry;
  size = 5;

  printf(".");
  input = (unsigned char *)malloc(size*sizeof(unsigned char));
  input[0]='"';
  input[1] = 'a';
  input[2] = 'b';
  input[3] = 'c';
  input[4] = '\\';


  quarry = quarry_makeQuarry(input,size);
  retval = quarry_dqLexer(quarry,0);

  fail_unless((quarry->line == 1),"Line # mismatch");
  fail_unless((quarry->slabType == quarry_Error),"Expected error");
  fail_unless((retval != 0), "return value != 0");
  fail_unless((quarry->holder.length == 4),"Expected size is 4");
  fail_unless((quarry->holder.data[3] == '\\'),"Expected a backslash at 4");
  fail_unless((quarry->holder.latest == 1),"Expected latest to be true");
  fail_unless((quarry->input.index == 5),"Expected input.index is 5");

}
END_TEST

START_TEST(test_HolderWithBSedChars)
{
  unsigned char *input;
  int size,index,retval,expLine;
  quarry_QuarryPtr quarry;
  size = 6;

  printf(".");
  input = (unsigned char *)malloc(size*sizeof(unsigned char));
  input[0]='"';
  input[1] = '\\';
  input[2] = '"';
  input[3] = '\\';
  input[4] = '\\';
  input[5] = '"';


  quarry = quarry_makeQuarry(input,size);
  retval = quarry_dqLexer(quarry,0);
  //  quarry_printQuarry(quarry);
  fail_unless((quarry->line == 1),"Line # mismatch");
  fail_unless((quarry->slabType == quarry_String),"Expected string");
  fail_unless((quarry->holder.length == 4),"Expected size is 4");
  fail_unless((quarry->holder.data[1] == '"'),"Expected a double quote at 1");
  fail_unless((quarry->holder.data[0] == '\\'),"Expected a double quote at 1");
  fail_unless((quarry->input.index == 6),"Expected input.index is 6");
}
END_TEST


START_TEST(test_HolderLatestSlash2)
{
  unsigned char *input;
  int size,index,retval,expLine;
  quarry_QuarryPtr quarry;
  size = 5;

  printf(".");
  input = (unsigned char *)malloc(size*sizeof(unsigned char));
  input[0]='"';
  input[1] = '"';
  input[2] = '\n';
  input[3] = 'a';
  input[4] = 'b';


  quarry = quarry_makeQuarry(input,size);
  quarry->holder.latest = 1;
  Quarry_AppendNext(quarry,'a');
  Quarry_AppendNext(quarry,'b');
  retval = quarry_dqLexer(quarry,1);

  fail_unless((quarry->line == 1),"Line # mismatch");
  fail_unless((quarry->slabType == quarry_String),"Expected string");
  fail_unless((quarry->holder.length == 3),"Expected size is 3");
  fail_unless((quarry->holder.data[2] == '"'),"Expected a double quote at 2");
  fail_unless((quarry->input.index == 2),"Expected input.index is 2");

}
END_TEST


START_TEST(test_EmptySingleQuotes1)
{
  unsigned char *input;
  int size,index,retval,expLine;
  quarry_QuarryPtr quarry;
  size = 5;

  printf("\nRunning tests on single quotes.");
  input = (unsigned char *)malloc(size*sizeof(unsigned char));
  input[0]='\'';
  input[1] = '\'';
  input[2] = '\n';
  input[3] = 'a';
  input[4] = 'b';


  quarry = quarry_makeQuarry(input,size);
  retval = quarry_sqLexer(quarry,0);
  fail_unless((quarry->line == 1),"Line # mismatch");
  fail_unless((quarry->slabType == quarry_Char),"Expected char");
  fail_unless((quarry->holder.length == 0),"Expected size is 0");
  fail_unless((quarry->input.index == 2),"Expected input.index is 2");

}
END_TEST

START_TEST(test_EmptySingleQuotes2)
{
  unsigned char *input;
  int size,index,retval,expLine;
  quarry_QuarryPtr quarry;
  size = 5;

  printf(".");
  input = (unsigned char *)malloc(size*sizeof(unsigned char));
  input[0]='\'';
  input[1] = '\'';
  input[2] = '\n';
  input[3] = 'a';
  input[4] = 'b';


  quarry = quarry_makeQuarry(input,size);
  quarry->input.index = 1;
  retval = quarry_sqLexer(quarry,1);
  fail_unless((quarry->line == 1),"Line # mismatch");
  fail_unless((quarry->slabType == quarry_Char),"Expected char");
  fail_unless((quarry->holder.length == 0),"Expected size is 0");
  fail_unless((quarry->input.index == 2),"Expected input.index is 2");

}
END_TEST

START_TEST(test_SingleQuotesNewLines)
{
  unsigned char *input;
  int size,index,retval,expLine;
  quarry_QuarryPtr quarry;
  size = 5;

  printf(".\n");
  input = (unsigned char *)malloc(size*sizeof(unsigned char));
  input[0]='\'';
  input[1] = '\n';
  input[2] = '\n';
  input[3] = '\n';
  input[4] = '\'';


  quarry = quarry_makeQuarry(input,size);
  retval = quarry_sqLexer(quarry,0);
  fail_unless((quarry->line == 4),"Line # mismatch");
  fail_unless((quarry->slabType == quarry_Char),"Expected char");
  fail_unless((quarry->holder.length == 3),"Expected size is 0");
  fail_unless((quarry->input.index == 5),"Expected input.index is 4");

}
END_TEST

void quarry_addQuoteTests(Suite *suite)
{
  TCase *tc_core = tcase_create("Quotes");
  tcase_add_test (tc_core,test_EmptyDoubleQuotes1);
  tcase_add_test (tc_core,test_EmptyDoubleQuotes2);
  tcase_add_test (tc_core,test_HolderLatestSlash1);
  tcase_add_test (tc_core,test_HolderLatestSlash2);
  tcase_add_test (tc_core,test_HolderWithBSedChars);
  tcase_add_test (tc_core,test_DoubleQuotesNewLines);


  tcase_add_test (tc_core,test_EmptySingleQuotes1);
  tcase_add_test (tc_core,test_EmptySingleQuotes2);

  tcase_add_test (tc_core,test_SingleQuotesNewLines);

  suite_add_tcase(suite,tc_core);
}

