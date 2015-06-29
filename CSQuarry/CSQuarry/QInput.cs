/*
Copyright (c) Hemanth Kapila
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
The name of Hemanth Kapila may NOT be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

namespace CSQuarry
{
    using System;
    using System.Collections.Generic;
    interface QInput  : IEnumerator<char>
    {
        /// <summary>
        /// Returns number of characters to be read before we find one that macthes the given predicate.
        /// </summary>
        /// <param name="predicate">predicate</param>
        /// <returns>number of bytes</returns>
        int Till(Predicate<char> predicate);

        /// <summary>
        /// Number of bytes that match the given predicate
        /// </summary>
        /// <param name="predicate">condition that needs to be satisfied</param>
        /// <returns>number of characters</returns>
        int WhileTrue(Predicate<char> predicate);

        /// <summary>
        /// Look at the nextChar without incrementing the counter
        /// </summary>
        /// <returns>the nextChar. Char.MinValue at EOF</returns>
        char Peek();

        int Line { get; }

        int Column { get; }
    }
}
