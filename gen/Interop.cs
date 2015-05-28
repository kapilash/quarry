using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Runtime.InteropServices;

namespace Check
{
    internal static class Interop
    {
        [DllImport("quarry")]
        internal unsafe static extern IntPtr quarry_newReader(string fileName, int pl);

        [DllImport("quarry")]
        internal unsafe static extern IntPtr quarry_read(IntPtr i);

        [DllImport("quarry")]
        internal unsafe static extern void quarry_freeSlab(IntPtr i);

        [DllImport("quarry")]
        internal unsafe static extern void quarry_printSlab(IntPtr i);
        [DllImport("quarry",CharSet=CharSet.Ansi)]
        internal unsafe static extern IntPtr quarry_closeReader(IntPtr i);


    }
}
