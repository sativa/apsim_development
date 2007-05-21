using System;
using System.IO;
using System.Drawing;
using System.Drawing.Imaging;
using System.Xml;
using System.Xml.Xsl;
using System.Xml.XPath;

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

		public static string EncodeBase64ToString(string base64String)
		{
			// Convert Base64 string back to a simmple string

				// No memos then exit
			if( base64String.Equals("") ) return "";


			//Open up MemoryStream object to obtain an array of character bytes
			System.IO.MemoryStream mem = new System.IO.MemoryStream( 
					Convert.FromBase64String(base64String));

			string str = ""; 
			byte[] bite = mem.ToArray();

			// Loop through array adding each character byte to the end of a string
			foreach( byte abyte in bite)
				str += Convert.ToChar(abyte);

			//return formatted string
			return str;

		}

		public static string EncodeStringToBase64(string str)
		{
			// Converts given string to Base64

			System.IO.MemoryStream mem = new System.IO.MemoryStream(str.Length);

			// Loop through each character in the memo, writing each to a MemoryStream buffer
			foreach( char character in str.ToCharArray() )
				mem.WriteByte(Convert.ToByte(character));

			// convert byte array characters to Base64 return it.
			return Convert.ToBase64String(mem.GetBuffer());

		}

		public static string ApplyStyleSheet(string Contents, string StylesheetContents)
			{
			//Create the XmlParserContext and reader.
			StringReader StyleSheetReader = new StringReader(StylesheetContents);
			XmlTextReader StyleSheet = new XmlTextReader(StyleSheetReader);

			//Load the stylesheet.
            XslCompiledTransform xslt = new XslCompiledTransform();  
			//xslt.Load(StyleSheet, new XmlUrlResolver(), XmlSecureResolver.CreateEvidenceForUrl(""));
			xslt.Load("D:\\development\\APSIMUI\\ProtocolToVariables.xsl");

			//Load the file to transform.

			StringReader ContentsReader = new StringReader(Contents);
			XPathDocument doc = new XPathDocument(ContentsReader);
					      
			//Create an XmlTextWriter which outputs to the console.
			StringWriter SWriter = new StringWriter();
			XmlTextWriter Writer = new XmlTextWriter(SWriter);

			//Transform the file and send the output to the console.
			xslt.Transform(doc, Writer);
			Writer.Close();
			return SWriter.ToString();
			}
	}
}
