//---------------------------------------------------------------------------
#include <windows.h>
#pragma hdrstop
#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <general\string_functions.h>
#include <general\io_functions.h>
#include <general\path.h>
#include <general\exec.h>
using namespace std;

string outFile;
string tempBPGFile;

// ------------------------------------------------------------------
// Convert a BPR file.
// ------------------------------------------------------------------
void convertBPR(const string& filename)
   {
   ifstream infile (filename.c_str());
   // Read in contents of file.
   string Contents;
   getline (infile, Contents, '\0');
   infile.close();
   istringstream in(Contents.c_str());

   // open output file for writting
   ofstream out (filename.c_str());

   // Loop through all lines.  If an include line is found then remove
   // all quotes from it.
   bool isIncludeLine;
   string line;
   getline(in, line);

   while (in)
      {
      if (line.find("<INCLUDEPATH value=") != string::npos)
         {
         // need to remove a backslash on the end of a line, otherwise when
         // we remove the quotes, MAKE thinks it's a continuation line.
//         Replace_all(line, "\\\"", "");
//         Replace_all(line, "\"", "");

         To_lower(line);
         Replace_all(line, "&quot", "");
         Replace_all(line, "program files", "progra~1");
         Replace_all(line, "includepath", "INCLUDEPATH");
         Replace_all(line, "$(bcb)", "$(BCB)");
         }


      // write out line
      out << line << endl;

      // get next line
      getline(in, line);
      }
   out.close();
   }
// ------------------------------------------------------------------
// Convert a BPG file.
// ------------------------------------------------------------------
void convertBPG(const string& filename)
   {
   // Open and read in contents of file.
   ifstream infile (filename.c_str());
   string Contents;
   getline (infile, Contents, '\0');
   infile.close();

   // look for the BORLAND lines and replace with ours.
   string borlandLines1 = "  $(ROOT)\\bin\\bpr2mak $**\n"
                          "  $(ROOT)\\bin\\make -$(MAKEFLAGS) -f$*.mak";
   string borlandLines2 = "  $(ROOT)\\bin\\bpr2mak -t$(ROOT)\\bin\\deflib.bmk $**\n"
                          "  $(ROOT)\\bin\\make -$(MAKEFLAGS) -f$*.mak";
   string ourLines1 = "  $(ROOT)\\bin\\bpr2mak $** >tmp.tmp\n"
                      "  cd $(**D)\n"
                      "  echo ------Compiling $**------\n"
                      "  echo ------Compiling $**------ >> " + outFile + "\n"
                      "  $(ROOT)\\bin\\make -$(MAKEFLAGS) -s -f$*.mak >> " + outFile + "\n"
                      "  cd $$$$\n"
                      "  del tmp.tmp";

   string ourLines2 = "  $(ROOT)\\bin\\bpr2mak -t$(ROOT)\\bin\\deflib.bmk $** >tmp.tmp\n"
                      "  cd $(**D)\n"
                      "  echo ------Compiling $**------\n"
                      "  echo ------Compiling $**------ >> " + outFile + "\n"
                      "  $(ROOT)\\bin\\make -$(MAKEFLAGS) -s -f$*.mak >> " + outFile + "\n"
                      "  cd $$$$\n"
                      "  del tmp.tmp";
   replaceAll(Contents, borlandLines1, ourLines1);
   replaceAll(Contents, borlandLines2, ourLines2);
   replaceAll(Contents, "$$$$", Path(filename).Get_directory());

   // open the file for writting and write new contents.

   ofstream out (tempBPGFile.c_str());
   out << Contents;
   out.close();
   }

//---------------------------------------------------------------------------
#pragma argsused
int main(int argc, char* argv[])
   {
   if (argc == 2)
      {
      // save the current working directory.
      Path currentDir = currentDir.getCurrentFolder();

      // change the directory to where the filename is located.
      Path filePath(argv[1]);
      filePath.Change_directory();

      outFile = filePath.Get_directory() + "\\gui.out";
      tempBPGFile = filePath.Get_directory() + "\\temp.bpg";
      unlink(outFile.c_str());

      string filename = argv[1];

      // get current contents of file.
      ifstream in(filename.c_str());
      string contents;
      getline(in, contents, '\0');
      in.close();

      // convert file to something that is compilable.
      if (filename.find(".bpg") != string::npos)
         convertBPG(filename);

      // run make.
      char buffer[100];
      getcwd(buffer, sizeof buffer);

      string commandLine = "make -i -s -f " + tempBPGFile;
      Exec(commandLine.c_str(), SW_SHOW, true);

      // delete temporary bpg file
      unlink(tempBPGFile.c_str());

      // restore the current working directory.
      currentDir.Change_directory();
      }
   else
      {
      cout << "This program does a make on a BORLAND BPR or BPG file." << endl;
      cout << "For some reason the CPPBuilder4&5&6 IDE doesn't write them correctly" << endl;
      cout << "so that they are compilable from the command line." << endl;
      cout << "Usage:    SEGMake filename" << endl;
      cout << "Where:    filename is a .BPR or a .BPG" << endl;
      return 1;
      }

   return 0;
   }

