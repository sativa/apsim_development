//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include <general\xml.h>
#include <general\macro.h>
#include <vector>
#include <string>
// ------------------------------------------------------------------
// provide an entry point for .net and other languages.
// ------------------------------------------------------------------
extern "C" void __stdcall __declspec(dllexport)
doMacroTransform(const char* xmlFileName,
                 const char* macroFileName,
                 const char* outputDirectory)
   {
   XMLDocument doc(xmlFileName);
   Macro macro;
   ifstream in(macroFileName);
   ostringstream contents;
   contents << in.rdbuf();

   vector<string> filesGenerated;
   macro.go(doc.documentElement(), contents.str(), filesGenerated, outputDirectory);
   }

 