using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Runtime.InteropServices;

namespace Check
{
    public enum SlabType
    {
        Error,
        Keyword,
        Identifier,
        STRING,
        CHAR,
        Numbers,
        Operator,
        Grouping,
        Punctuation,
        Comment,
        Whitespace,
        NewLine,
        MetaId,
        EOF
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
    public class Slab
    {
        public int Line { get; set; }
        public int Column { get; set; }
        public string Text { get; set; }
        public SlabType slabType { get; set; }
        public UInt32 Metadata { get; set; }

        public override string ToString()
        {
            var text = new StringBuilder("{[");
            text.Append(this.Line);
            text.Append(',');
            text.Append(this.Column);
            text.Append(']');
            text.Append(Text);
            text.Append(" ::");
            text.Append(this.slabType);
            text.Append(" with ");
            text.Append(this.Metadata);
            text.Append("}");
            return text.ToString();
        }
    }
}
