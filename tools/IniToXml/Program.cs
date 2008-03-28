using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Xml;
using CSGeneral;

namespace IniToXml
    {
    class Program
        {
        static void Main(string[] args)
            {
            try
                {
                if (args.Length != 1)
                    throw new Exception("Usage: IniToXML inifilename");
                string InFileName = args[0].ToLower();
                StreamReader In = new StreamReader(InFileName);

                string OutFileName = InFileName.Replace(".ini", ".xml");

                StreamWriter Out = new StreamWriter(OutFileName);
                Out.WriteLine("<" + Path.GetFileNameWithoutExtension(InFileName) + ">");
                string Indentation = "   ";

                string OpenSection = "constants";
                string Line;
                string Comment = "";
                while (!In.EndOfStream)
                    {
                    Line = In.ReadLine();
                    Line = Line.Trim(" \t".ToCharArray());
                    if (Line != "")
                        {
                        if (Line[0] == '!')
                            Comment = Comment + Line.Substring(1) + "\n";
                        else
                            {
                            // New section name found - write section node.
                            if (Line[0] == '[')
                                {
                                CloseSection(OpenSection, ref Indentation, Out);

                                WriteComments(ref Comment, Indentation, Out);

                                string Description;
                                ParseSection(Line, out OpenSection, out Description);
                                if (OpenSection.ToLower() != "constants")
                                    {
                                    Out.Write(Indentation + "<" + OpenSection);
                                    if (Description != "")
                                        Out.Write(" description=\"" + Description + "\"");
                                    Out.WriteLine(">");
                                    Indentation = "      ";
                                    }
                                }
                            else if (Line.IndexOf('=') != -1)
                                {
                                WriteComments(ref Comment, Indentation, Out);

                                string Name;
                                string Units;
                                string Description;
                                string Value;
                                ParseLine(Line, out Name, out Units, out Description, out Value);
                                Out.Write(Indentation + "<" + Name);
                                if (Units != "")
                                    Out.Write(" units=\"" + Units + "\"");
                                if (Description != "")
                                    Out.Write(" description=\"" + Description + "\"");
                                Out.WriteLine(">" + Value + "</" + Name + ">");
                                }
                            }
                        }
                    }
                CloseSection(OpenSection, ref Indentation, Out);
                Out.WriteLine("</" + Path.GetFileNameWithoutExtension(InFileName) + ">");
                Out.Close(); ;
                }
            catch (Exception err)
                {
                Console.WriteLine(err.Message);
                }
            }

        private static void WriteComments(ref string Comment, string Indentation, StreamWriter Out)
            {
            // Write comments
            if (Comment != "")
                {
                Comment = Comment.Replace("--", "__") + " ";
                Comment = Comment.Replace("\n", "\n" + Indentation);
                Out.WriteLine(Indentation + "<!-- " + Comment + "-->");
                Comment = "";
                }
            }

        private static void CloseSection(string OpenSection, ref string Indentation, StreamWriter Out)
            {
            if (OpenSection.ToLower() != "constants")
                {
                Indentation = "   ";
                Out.WriteLine(Indentation + "</" + OpenSection + ">");
                }
            }

        private static void ParseSection(string Line, out string OpenSection, out string Description)
            {
            Description = "";
            int PosDescription = Line.IndexOf('!');
            if (PosDescription != -1)
                {
                Description = Line.Substring(PosDescription + 1).Trim().Replace("\"", "'");
                Line = Line.Remove(PosDescription);
                }
            Line = Line.Trim(" []".ToCharArray());
            int PosPeriod = Line.LastIndexOf('.');
            if (PosPeriod != -1)
                OpenSection = Line.Substring(PosPeriod + 1);
            else
                OpenSection = Line;
            }
        private static void ParseLine(string Line, out string Name, out string Units, out string Description, out string Value)
            {
            Name = "";
            Units = "";
            Description = "";
            Value = "";

            int PosEquals = Line.IndexOf('=');
            if (PosEquals != -1)
                {
                Name = Line.Substring(0, PosEquals).Trim();
                Value = Line.Substring(PosEquals + 1).Trim();
                int PosDescription = Value.IndexOf('!');
                if (PosDescription != -1)
                    {
                    // we have found a description.
                    Description = Value.Substring(PosDescription + 1).Trim().Replace("\"", "'");
                    Value = Value.Remove(PosDescription);
                    }

                int PosUnits = Value.IndexOf('(');
                if (PosUnits != -1)
                    {
                    Units = Value.Substring(PosUnits).Trim(" ()".ToCharArray());
                    Value = Value.Remove(PosUnits).Trim();
                    }
                }
            }




        }
    }
