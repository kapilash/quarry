using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Runtime.InteropServices;

namespace Check
{
    public class QuarryReader : IDisposable , IEnumerable<Slab>
    {
        private  IntPtr qreader;
        public QuarryReader(string fileName, int i)
        {
            qreader = Interop.quarry_newReader(fileName, i);
        }


        public void Dispose()
        {
            Interop.quarry_closeReader(qreader);
        }

        public IEnumerator<Slab> GetEnumerator()
        {
            while (true) {
                var slabPtr = Interop.quarry_read(qreader);
                LocalSlab slab = new LocalSlab();

                Marshal.PtrToStructure(slabPtr, slab);
                var slabType = (SlabType)slab.slabType;
                if (slabType == SlabType.EOF)
                    break;
                byte[] bytes = new byte[slab.slabTokenLen];
                Marshal.Copy(slab.content, bytes, 0, slab.slabTokenLen);
                var str = Encoding.UTF8.GetString(bytes);
                var c = new Slab()
                {
                    Line = slab.slabLine,
                    Column = slab.slabCol,
                    Text = str,
                    slabType = slabType,
                    Metadata = slab.slabMD
                };
                Interop.quarry_freeSlab(slabPtr);
                yield return c;
            }
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return this.GetEnumerator();
        }
    }
}
