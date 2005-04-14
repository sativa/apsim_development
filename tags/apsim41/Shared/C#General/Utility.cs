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

		public static string ApplyStyleSheet(string Contents, string StylesheetContents)
			{
			//Create the XmlParserContext and reader.
			StringReader StyleSheetReader = new StringReader(StylesheetContents);
			XmlTextReader StyleSheet = new XmlTextReader(StyleSheetReader);

			//Load the stylesheet.
			XslTransform xslt = new XslTransform();
			xslt.Load(StyleSheet, new XmlUrlResolver(), XmlSecureResolver.CreateEvidenceForUrl(""));

			//Load the file to transform.

			StringReader ContentsReader = new StringReader(Contents);
			XPathDocument doc = new XPathDocument(ContentsReader);
					      
			//Create an XmlTextWriter which outputs to the console.
			StringWriter SWriter = new StringWriter();
			XmlTextWriter Writer = new XmlTextWriter(SWriter);

			//Transform the file and send the output to the console.
			xslt.Transform(doc, null, Writer, null);
			Writer.Close();
			return SWriter.ToString();
			}
	}
}
