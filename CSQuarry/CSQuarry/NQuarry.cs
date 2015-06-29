namespace CSQuarry
{
    using System;
    using System.Collections.Generic;
    using System.Collections;
    using System.Runtime.InteropServices;

    public class NQuarry : IDisposable, IEnumerable<Token>
    {
        private IntPtr qReader;

        public NQuarry(string fileName, LangType langType)
        {
            qReader = Interop.quarry_fromFile((int)langType, fileName);
        }

        public void Dispose()
        {
            Interop.quarry_close(qReader);
        }

        public IEnumerator<Token> GetEnumerator()
        {
            while (true)
            {
                var tokenPtr = Interop.quarry_nextToken(qReader);
                NativeToken token = new NativeToken();

                Marshal.PtrToStructure(tokenPtr, token);
                var tokenType = (TokenType)token.tokenType;
                if (tokenType == TokenType.QEOF)
                    break;
                byte[] bytes = new byte[token.length];
                Marshal.Copy(token.text, bytes, 0, token.length);
                var str = System.Text.Encoding.UTF8.GetString(bytes);
                var c = new Token()
                {
                    Line = token.line,
                    Column = token.column,
                    Text = str,
                    slabType = tokenType
                };
                Interop.quarry_freeToken(tokenPtr);
                yield return c;
            }
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return this.GetEnumerator();
        }
    }
}
