using System;
using System.IO;
using System.Drawing;
using System.Drawing.Imaging;

namespace CSGeneral
	{
	/// <summary>
	/// General utility functions
	/// </summary>
	public class CSUtility
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
