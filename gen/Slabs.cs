using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Runtime.InteropServices;

namespace Check
{
    public enum SlabType
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
    
    [StructLayout(LayoutKind.Sequential, CharSet=CharSet.Ansi)]
    class LocalSlab
    {
        internal int slabLine;
        internal int slabCol;
        internal int slabTokenLen;
        internal int slabType;
        internal IntPtr content;
        internal UInt32 slabMD;
    }

    [StructLayout(LayoutKind.Sequential,CharSet = CharSet.Ansi)]
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
        public int Line { get; set; }
        public int Column { get; set; }
        public string Text { get; set; }
        public SlabType slabType { get; set; }

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
