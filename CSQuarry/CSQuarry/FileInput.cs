namespace CSQuarry
{
    using System;
    using System.IO;
    using System.Collections.Generic;
    using System.IO.MemoryMappedFiles;

    class FileInput : QInput
    {

        private MemoryMappedViewStream stream;
        private TextReader reader;
        private readonly MemoryMappedFile file;
        private char currentChar;

        internal FileInput(string fileName)
        {
            file = MemoryMappedFile.CreateFromFile(fileName, FileMode.Open);
            stream = file.CreateViewStream();
            reader = new StreamReader(stream, System.Text.Encoding.UTF8);
            this.Line = 0;
            this.Column = 0;
            currentChar = char.MinValue;
        }
        public string Till(Predicate<char> predicate)
        {
            var strb = new System.Text.StringBuilder();
            char n = this.Peek();
            while (!predicate(n) && this.MoveNext())
            {
                strb.Append(this.Current);
                n = this.Peek();
            }
            return strb.ToString();
        }

        public string WhileTrue(Predicate<char> predicate)
        {
            var strb = new System.Text.StringBuilder();
            char n = this.Peek();
            while (predicate(n) && this.MoveNext())
            {
                strb.Append(this.Current);
                n = this.Peek();
            }
            return strb.ToString();
        }

        public char Peek()
        {
            return Convert.ToChar(reader.Peek());
        }

        public int Line
        {
            get;
            private set;
        }

        public int Column
        {
            get;
            private set;
        }

        public char Current
        {
            get {
                return this.currentChar;
            }
        }

        public void Dispose()
        {
            file.Dispose();
        }

        object System.Collections.IEnumerator.Current
        {
            get { return this.Current; }
        }

        public bool MoveNext()
        {
            int i = reader.Read();
            if (i < 0)
                return false;
            this.currentChar = Convert.ToChar(i);
            if (this.currentChar == '\n')
            {
                this.Line = this.Line + 1;
                this.Column = 1;
            }
            else if (this.currentChar == '\r')
            {
                int n = reader.Peek();
                if (n > 0 && Convert.ToChar(n) == '\n')
                {
                    reader.Read();
                }
                this.Line = this.Line + 1;
                this.Column = 1;
            }
            else
            {
                this.Column = this.Column + 1;
            }
            return true;
        }

        private void HandleLocation(char c)
        {
            if (this.currentChar == '\n')
            {
                this.Line = this.Line + 1;
                this.Column = 1;
            }
            else if (this.currentChar == '\r')
            {
                int n = reader.Peek();
                if (n > 0 && Convert.ToChar(n) == '\n')
                {
                    reader.Read();
                }
                this.Line = this.Line + 1;
                this.Column = 1;
            }
            else
            {
                this.Column = this.Column + 1;
            }
        }
        public void Reset()
        {
            throw new InvalidOperationException();
        }
    }
}
