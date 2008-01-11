using System;
using System.Collections.Generic;
using System.Text;
using System.Drawing;
using System.IO;
using System.Drawing.Imaging;

namespace CSUserInterface
    {
    public class BitmapUtility
        {

        public static string EncodeBitmapToString(Bitmap bitmap)
            {
            MemoryStream stream = new MemoryStream();
            bitmap.Save(stream, ImageFormat.Jpeg);
            return Convert.ToBase64String(stream.GetBuffer());
            }

        public static Bitmap DecodeStringToBitmap(string st)
            {
            MemoryStream stream = new MemoryStream(Convert.FromBase64String(st));
            return new Bitmap(stream);
            }

        }
    }
