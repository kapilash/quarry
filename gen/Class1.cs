using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Runtime.InteropServices;

namespace Check
{
    internal static class Class1
    {
        [DllImport("quarry")]
        internal unsafe static extern IntPtr quarry_fromFile(int pl, string fileName);

        [DllImport("quarry")]
        internal unsafe static extern IntPtr quarry_nextToken(IntPtr i);

        [DllImport("quarry")]
        internal unsafe static extern void quarry_freeToken(IntPtr i);

//        [DllImport("quarry")]
//        internal unsafe static extern void quarry_printSlab(IntPtr i);
        [DllImport("quarry",CharSet=CharSet.Ansi)]
        internal unsafe static extern IntPtr quarry_close(IntPtr i);


    }
}
