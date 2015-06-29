namespace CSQuarry
{
    using System;
    using System.Runtime.InteropServices;
    /// <summary>
    /// Interface to C++
    /// </summary>
    internal static class Interop
    {
        [DllImport("quarry")]
        internal unsafe static extern IntPtr quarry_fromFile(int pl, string fileName);

        [DllImport("quarry")]
        internal unsafe static extern IntPtr quarry_nextToken(IntPtr i);

        [DllImport("quarry")]
        internal unsafe static extern void quarry_freeToken(IntPtr i);

        //        [DllImport("quarry")]
        //        internal unsafe static extern void quarry_printSlab(IntPtr i);
        [DllImport("quarry", CharSet = CharSet.Ansi)]
        internal unsafe static extern IntPtr quarry_close(IntPtr i);

        [DllImport("quarry", CharSet = CharSet.Ansi)]
        internal unsafe static  extern void moveToFile(IntPtr i, string fileName);

        [DllImport("quarry", CharSet = CharSet.Ansi)]
        internal unsafe static extern void moveToStr(IntPtr i, IntPtr bytes, ulong length, int lineNo, int columnNo);

        [DllImport("quarry")]
        internal unsafe static extern int quarry_numberType(IntPtr* ptr);

        [DllImport("quarry")]
        internal unsafe static extern int quarry_toInt(IntPtr* ptr);

        [DllImport("quarry")]
        internal unsafe static extern uint quarry_toUInt(IntPtr* ptr);

        [DllImport("quarry")]
        internal unsafe static extern long quarry_toLong(IntPtr* ptr);

        [DllImport("quarry")]
        internal unsafe static extern ulong quarry_toULong(IntPtr* ptr);

        [DllImport("quarry")]
        internal unsafe static extern float quarry_toFloat(IntPtr* ptr);

        [DllImport("quarry")]
        internal unsafe static extern double quarry_toDouble(IntPtr* ptr);

        [DllImport("quarry")]
        internal unsafe static extern int quarry_toChar(IntPtr* ptr);

    }
}
