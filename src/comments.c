/*
Copyright (c) 2013, Hemanth Kapila
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1) Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2) Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
The name of the Hemanth Kapila may NOT be used to endorse or promote products derived from this software without specific prior written permission.
3) THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
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
    if(nextChar != 42){
	  Quarry_UnRead(quarry);
	  quarry->slabType = quarry_Operator;
	  Quarry_AppendSingleChar(quarry,47);
	  return 0;
    }
    
    goto THIRD_CHAR;
  case 2:
    Quarry_AppendNext(quarry,nextChar)
    goto CONSUME_COMMENTS;
  case 3:
    if(nextChar == 47){
      (quarry->holder.length)--;
      quarry->slabType = quarry_Comment;  return 0;
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
  quarry->holder.md = 0;
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
	  quarry->slabType = quarry_Comment;
	  return 0;
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

 LINE_COMMENT:
  quarry->holder.md = 1;
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
