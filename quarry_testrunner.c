#include "quarry_testsuites.h"


int main(int argc, char **argv){
  int numberFailed = 0;
  Suite *suite = suite_create("lexers");

  /* Add comment tests */
  quarry_addCommentTests(suite);
  quarry_addNumbersTests(suite);
  quarry_addKeywordTests(suite);
  quarry_addIdentifierTests(suite);
  quarry_addReaderTests(suite);
  /* Add quote tests */
  quarry_addQuoteTests(suite);

  /*Create runner and run the suite*/
  SRunner *runner = srunner_create(suite);
  srunner_run_all(runner,CK_NORMAL);
  printf("\n");
  numberFailed = srunner_ntests_failed(runner);
  srunner_free(runner);
  return numberFailed;
}
