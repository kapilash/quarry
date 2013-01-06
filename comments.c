#include "lexers.h"
#include <stdio.h>
#include <stdlib.h>

int quarry_commentLexer(quarry_QuarryPtr quarry, int lexerState){
  unsigned char nextChar;
  int currentIssueLength;
  if (!Quarry_HasMore(quarry)){
    if(lexerState == 0){
      quarry->slabType = quarry_Error;
      Quarry_AppendSingleChar(quarry,47)
      return 1;
    }
    else{
      return lexerState;//Do not disturb the lexerState
    }
  }

  Quarry_ReadNext(quarry,nextChar)
  switch(lexerState){
  case 0 :
    if (!Quarry_HasMore(quarry)){
      quarry->slabType = quarry_Error;
      Quarry_AppendSingleChar(quarry,47)
      return 1;
    }
    Quarry_ReadNext(quarry,nextChar)
      //we do expect it to follow through the next case.
  case 1:
    if(nextChar == 47){
      quarry->holder.length = 0;      
      goto LINE_COMMENT;
    }
    if(nextChar != 42)
      goto ONLY_FWD_SLASH;
    goto THIRD_CHAR;
  case 2:
    Quarry_AppendNext(quarry,nextChar)
    goto CONSUME_COMMENTS;
  case 3:
    if(nextChar == 47){
      (quarry->holder.length)--;
      goto RETURN_COMMENT_END;
    }else{
      Quarry_AppendNext(quarry,nextChar);
      goto CONSUME_COMMENTS;
    }
  case 4:
    if(nextChar == '\n'){
      quarry->slabType  = quarry_Comment;
      return 0;/* an empty line comment */
    }
    Quarry_AppendNext(quarry,nextChar)
    goto LINE_COMMENT;
  default:
    fprintf(stderr,"ERROR. lexerState  is not 0,1,2 or 3");
    exit(1);
  }
 THIRD_CHAR:
  quarry->holder.length = 0;
  if(Quarry_HasMore(quarry)){
    Quarry_ReadNext(quarry,nextChar);
    Quarry_AppendNext(quarry,nextChar);
    if(nextChar == '\n')
      Quarry_IncrLine(quarry);
    goto CONSUME_COMMENTS;
  }else{
    quarry->slabType = quarry_Error;
    quarry->holder.length = 0;
     return 2;
  }
 CONSUME_COMMENTS:
  while (Quarry_HasMore(quarry)){    
    Quarry_ReadNext(quarry,nextChar)
    if(nextChar == 47){
      if (42 == Quarry_LastRead(quarry)) {
	  quarry->holder.length = quarry->holder.length - 1;
	  goto RETURN_COMMENT_END;
      }
      Quarry_AppendNext(quarry,47)
    }else{
      if('\n' == nextChar){
	Quarry_AppendNext(quarry,'\n')
        Quarry_IncrLine(quarry)
      }else{
	Quarry_AppendNext(quarry,nextChar)
      }
    }      
  }
  quarry->slabType = quarry_Error;
  if(42 == Quarry_LastRead(quarry))
    return 3;
  else
    return 2;
 RETURN_COMMENT_END:
  quarry->slabType = quarry_Comment;
  return 0;
 ONLY_FWD_SLASH:
  /* it was only a forward slash not followed by a '*' */
  Quarry_UnRead(quarry);
  quarry->slabType = quarry_Operator;
  Quarry_AppendSingleChar(quarry,47)
  return 0;
 LINE_COMMENT:
  while Quarry_HasMore(quarry){
    Quarry_ReadNext(quarry,nextChar)
    if(nextChar == '\n'){
      Quarry_IncrLine(quarry)
      quarry->slabType = quarry_Comment;
      return 0;
    }
    Quarry_AppendNext(quarry,nextChar)
  }
  quarry->slabType = quarry_Error;
  quarry->holder.length = 0;
  return 4;
}
