namespace CSQuarry
{
    using System;
    using System.Runtime.InteropServices;
    using System.Text;
    public enum TokenType
    {
        ERROR,
        KEYWORD,
        IDENT,
        STRING,
        CHAR,
        NUMBER,
        OPERATOR,
        OPEN_BRACE,
        CLOSE_BRACE,
        OPEN_BRACKET,
        CLOSE_BRACKET,
        SQUARE_OPEN,
        SQUARE_CLOSE,
        DOT,
        COLON,
        SEMI_COLON,
        QUESTION_MARK,
        COMMA,
        COMMENT,
        WHITESPACE,
        NEWLINE,
        META_ID,
        BOOL,
        QEOF
    }

    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Ansi)]
    class NativeToken
    {
        internal int line;
        internal int column;
        internal int tokenType;
        internal int length;
        internal IntPtr text;
        internal IntPtr opaque;
    }

    public class Token
    {
        public int Line { get; internal set; }
        public int Column { get; internal set; }
        public string Text { get; internal set; }
        public TokenType slabType { get; internal set; }

        public override string ToString()
        {
            var text = new StringBuilder("{[");
            text.Append(this.Line);
            text.Append(',');
            text.Append(this.Column);
            text.Append(']');
            text.Append(Text);
            text.Append("(");
            text.Append(this.slabType);
            text.Append(')');
            text.Append(this.Text);
            text.Append("}");
            return text.ToString();
        }
    }
}
