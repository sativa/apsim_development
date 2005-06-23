//---------------------------------------------------------------------------

#include <fstream>
#include <sstream>

#include <vector>
#include <string>
#include <general/TreeNodeIterator.h>
#include <general/xml.h>
#include <general/macro.h>

using namespace std;

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

 