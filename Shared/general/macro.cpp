//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "Macro.h"
#include <general\xml.h>
#include <general\string_functions.h>
#include <general\stl_functions.h>
#include <general\stringtokenizer.h>
#include <ApsimShared\ApsimDirectories.h>
using namespace std;
// ------------------------------------------------------------------
// Constructor
// ------------------------------------------------------------------
Macro::Macro()
   {
   }
// ------------------------------------------------------------------
// Destructor
// ------------------------------------------------------------------
Macro::~Macro()
   {
   }
// ------------------------------------------------------------------
// generate the files.
// ------------------------------------------------------------------
void Macro::go(const XMLNode& values,
               const string& macroContents,
               vector<string>& filesGenerated)
   {
   macroValues = &values;
   filesGenerated.erase(filesGenerated.begin(), filesGenerated.end());
   string contents = macroContents;
   contents = parseForEach(contents, "", *macroValues);
   writeStringToFiles(contents, filesGenerated);
   }

// ------------------------------------------------------------------
// Do all macro replacement in specified text for the given macro node.
// ------------------------------------------------------------------
string replaceMacros(const string& originalContents,
                     const string& parentName,
                     const XMLNode& node)
   {
   string contents = originalContents;
   string stringToReplace = parentName + ".name";
   replaceAll(contents, stringToReplace, node.getAttribute("name"));

   vector<string> attributes;
   node.getAttributes(attributes);
   for (unsigned i = 0; i != attributes.size(); i++)
      {
      stringToReplace = parentName + "." + attributes[i];
      replaceAll(contents, stringToReplace, node.getAttribute(attributes[i]));
      }


   unsigned counter = 0;
   for (XMLNode::iterator i = node.begin(); i != node.end(); i++)
      {
      counter++;
      stringToReplace = parentName + ".counter";
      replaceAll(contents, stringToReplace, IntToStr(counter).c_str());

      stringToReplace = parentName + "." + i->getName();
      replaceAll(contents, stringToReplace, i->getValue());
      }
   return contents;
   }
// ------------------------------------------------------------------
// Adjust the start of the specified tag. This routine will remove
// unwanted spaces on the front of the tag.
// ------------------------------------------------------------------
unsigned getStartOfTag(const string& st, unsigned posTag)
   {
   posTag = st.find_last_not_of(' ', posTag-1);
   if (posTag == string::npos)
      return 0;
   else if (st[posTag] == '\n')
      return posTag + 1;
   else
      return posTag+1;
   }
// ------------------------------------------------------------------
// Adjust the end of the specified tag. This routine will remove
// unwanted spaces and a single CR on the end of the tag.
// ------------------------------------------------------------------
unsigned getEndOfTag(const string& st, unsigned posTag, const string& tagName)
   {
   unsigned posEndTag = posTag + tagName.length();
   posEndTag = st.find_first_not_of(' ', posEndTag);
   if (posEndTag == string::npos)
      return st.length()-1;
   else if (st[posEndTag] == '\n')
      return posEndTag;
   else
      return posTag + tagName.length()-1;
   }
// ------------------------------------------------------------------
// Parse and remove all for_each macros from specified string.
// ------------------------------------------------------------------
string Macro::parseForEach(const string& originalContents,
                           const string& parentName,
                           const XMLNode& valuesNode) const
   {
   string contents = originalContents;
   unsigned posForEach = contents.find("#for_each");
   while (posForEach != string::npos)
      {
      StringTokenizer tokenizer(contents, posForEach, " \n");
      string foreach = tokenizer.nextToken();
      string macroName = tokenizer.nextToken();
      if (macroName == "")
         throw runtime_error("Can't find macro name after #for_each");
      unsigned posStartForEachBody = tokenizer.currentPos();
      if (contents[posStartForEachBody] == '\n')
         posStartForEachBody++;

      // Now search through to find the end of the for loop
      // This current loop may contain nested loops - therefore
      // need to count #for_each and #endfor statements.
      unsigned posEndFor;
      unsigned currentPos = posStartForEachBody;
      unsigned count = 1;
      do
         {
         unsigned posForEach = contents.find("#for_each", currentPos);
         posEndFor = contents.find("#endfor", currentPos);
         if (posForEach != string::npos && posForEach < posEndFor)
            {
            count++;
            currentPos = posForEach + 1;
            }
         else
            {
            count--;
            currentPos = posEndFor;
            }
         currentPos++;
         }
      while (count > 0);
      unsigned posStartTag, posEndTag;

      // get the bit of text before the #for_each
      string preBody = contents.substr(0, getStartOfTag(contents, posForEach));

      // get the contents of the #for_each body
      string forEachBody = contents.substr(posStartForEachBody,
                                           getStartOfTag(contents, posEndFor)-posStartForEachBody);

      // get the bit of text after the #endfor
      string postBody = contents.substr(getEndOfTag(contents, posEndFor, "#endfor")+1);

      // recurse back and parse the forEachBody for other for_each macros.
      string body;
      for (XMLNode::iterator i = valuesNode.begin(); i != valuesNode.end(); i++)
         {
         string childToMatch;
         unsigned posChild = macroName.rfind(".");
         if (posChild != string::npos)
            childToMatch = macroName.substr(posChild+1);
         else
            childToMatch = macroName;
         if (Str_i_Eq(i->getName(), childToMatch))
            body += replaceMacros(parseForEach(forEachBody, macroName, *i), macroName, *i);
         }
      forEachBody = body;

      // Resolve any #if defines.
      parseIf(forEachBody);

      contents = preBody + forEachBody + postBody;

      // locate next for_each
      posForEach = contents.find("#for_each");
      }
   return contents;
   }
// ------------------------------------------------------------------
// Write everything between #file/#endfile pairs to the
// file name listed after the #file macro.
// ------------------------------------------------------------------
void Macro::writeStringToFiles(string contents,
                                        vector<string>& fileNamesCreated) const
   {
   unsigned posFile = contents.find("#file");
   while (posFile != string::npos)
      {
      posFile += strlen("#file");
      unsigned posEol = contents.find("\n", posFile);
      string filename=contents.substr(posFile, posEol-posFile);
      Strip(filename, " ");
      replaceAll(filename, "%apsuite", getApsimDirectory());
      
      unsigned posStartFileBody = posEol + 1;
      unsigned posEndFileBody = contents.find("#endfile", posStartFileBody);
      if (posEndFileBody == string::npos)
         throw runtime_error("Cannot find a matching #endfile tag");

      string fileContents = contents.substr(posStartFileBody, posEndFileBody-posStartFileBody);
      replaceGlobalCounter(fileContents);

      // Dump the file text into the given file name
      ofstream out;
      out.open (filename.c_str());
      out << fileContents;
      out.close();
      fileNamesCreated.push_back(ExpandFileName(filename.c_str()).c_str());

      posFile = contents.find("#file", posEndFileBody);
      }
   }

// ------------------------------------------------------------------
// Replace all global counter macros in the specified string.
// ------------------------------------------------------------------
void Macro::replaceGlobalCounter(string& contents) const
   {
   int globalCounter = 1;

   static const char* GLOBAL_COUNTER_INC = "#global.counter inc";
   char* posGlobalCounter = stristr(contents.c_str(), "global.counter");
   while (posGlobalCounter != NULL)
      {
      unsigned posCounter = posGlobalCounter - contents.c_str();
      if (contents[posCounter-1] == '#'
          && Str_i_Eq(contents.substr(posCounter-1, strlen(GLOBAL_COUNTER_INC)),
                      GLOBAL_COUNTER_INC))
         {
         unsigned posStartLine = posCounter;
         posStartLine = contents.rfind('\n', posStartLine);
         if (posStartLine == string::npos)
            posStartLine = 0;
         else
            posStartLine++;

         unsigned posEndLine = contents.find('\n', posStartLine);
         if (posEndLine == string::npos)
            posEndLine = contents.length();

         contents.erase(posStartLine, posEndLine - posStartLine + 1);
         posGlobalCounter = stristr(contents.c_str(), "global.counter");

         globalCounter++;
         }
      else
         contents.replace(posCounter, strlen("global.counter"), IntToStr(globalCounter).c_str());

      posGlobalCounter = stristr(contents.c_str(), "global.counter");
      }
   }
// ------------------------------------------------------------------
// Resolve any #if defines.
// ------------------------------------------------------------------
void Macro::parseIf(string& st) const
   {
   unsigned posElseIf = 0;
   unsigned posElse = 0;
   unsigned posOpenBracket;
   unsigned posCloseBracket;
   unsigned posCondition = st.find("#if");
   while (posCondition != string::npos)
      {
      unsigned posEndIf = st.find("#endif", posCondition+1);
      unsigned posEndBlock = min(min(st.find("#elseif", posCondition+1),
                                     st.find("#else", posCondition+1)), posEndIf);
      if (posEndBlock == string::npos)
         throw runtime_error("Missing endif for if: " + st.substr(posCondition));
      bool ok;
      if (posCondition == posElse && posCondition != posElseIf)
         {
         posOpenBracket = string::npos;
         posCloseBracket = string::npos;
         ok = true;
         }
      else
         {
         posOpenBracket = st.find('(', posCondition);
         posCloseBracket = st.find(')', posOpenBracket);
         ok = evaluateIf(st.substr(posOpenBracket+1, posCloseBracket-posOpenBracket-1));
         }
      if (ok)
         {
         posEndBlock = getStartOfTag(st, posEndBlock);
         posEndIf = getEndOfTag(st, posEndIf, "#endif");

         // remove everything from the end of block to after the endif.
         st.erase(posEndBlock, posEndIf-posEndBlock+1);

         // remove the condition line.
         unsigned posEndCondition;
         if (posCloseBracket != string::npos)
            posEndCondition = getEndOfTag(st, posCloseBracket, ")");
         else
            posEndCondition = getEndOfTag(st, posCondition, "#else");
         posCondition = getStartOfTag(st, posCondition);
         st.erase(posCondition, posEndCondition-posCondition+1);
         }
      else
         {
         // remove everything from start of condition down to end of block.
         posCondition = getStartOfTag(st, posCondition);
         if (posEndBlock == posEndIf)
            posEndBlock = getEndOfTag(st, posEndBlock, "#endif");
         else
            posEndBlock = getStartOfTag(st, posEndBlock);
         st.erase(posCondition, posEndBlock-posCondition+1);
         }

      unsigned posIf = st.find("#if");
      posElse = st.find("#else");
      posElseIf = st.find("#elseif");
      posCondition = min(min(posIf, posElse), posElseIf);
      }
   }
// ------------------------------------------------------------------
// Evaluated the specified #if statement.
// ------------------------------------------------------------------
bool Macro::evaluateIf(const string& conditionLine) const
   {
   vector<string> words;
   SplitStringHonouringQuotes(conditionLine, " ", words);
   if (words.size() != 3)
      throw runtime_error("Badly formatted #if statement: " + conditionLine);
   string lhs = words[0];
   string op = words[1];
   string rhs = words[2];
   Strip(lhs, " ");
   Strip(rhs, " ");
   if (op == "=")
      return Str_i_Eq(lhs, rhs);
   else if (op == "<>")
      return !Str_i_Eq(lhs, rhs);
   else
      {
      double lhsValue = StrToFloatDef(lhs.c_str(), 0.0);
      double rhsValue = StrToFloatDef(rhs.c_str(), 0.0);
      if (op == "<")
         return (lhsValue < rhsValue);
      else if (op == "<=")
         return (lhsValue <= rhsValue);
      else if (op == ">")
         return (lhsValue > rhsValue);
      else if (op == ">=")
         return (lhsValue >= rhsValue);
      else
         throw runtime_error("Unknown #if operator: " + op);
      }
   }

