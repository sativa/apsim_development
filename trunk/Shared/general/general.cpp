//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
//   Important note about DLL memory management when your DLL uses the
//   static version of the RunTime Library:
//
//   If your DLL exports any functions that pass String objects (or structs/
//   classes containing nested Strings) as parameter or function results,
//   you will need to add the library MEMMGR.LIB to both the DLL project and
//   any other projects that use the DLL.  You will also need to use MEMMGR.LIB
//   if any other projects which use the DLL will be perfomring new or delete
//   operations on any non-TObject-derived classes which are exported from the
//   DLL. Adding MEMMGR.LIB to your project will change the DLL and its calling
//   EXE's to use the BORLNDMM.DLL as their memory manager.  In these cases,
//   the file BORLNDMM.DLL should be deployed along with your DLL.
//
//   To avoid using BORLNDMM.DLL, pass string information using "char *" or
//   ShortString parameters.
//
//   If your DLL uses the dynamic version of the RTL, you do not need to
//   explicitly add MEMMGR.LIB as this will be done implicitly for you
//---------------------------------------------------------------------------
USEUNIT("vcl_functions.cpp");
USEUNIT("exec.cpp");
USEUNIT("ini_file.cpp");
USEUNIT("io_functions.cpp");
USEUNIT("macros.cpp");
USEUNIT("math_functions.cpp");
USEUNIT("path.cpp");
USEUNIT("stream_functions.cpp");
USEUNIT("stream_processor.cpp");
USEUNIT("string_functions.cpp");
USEUNIT("stristr.cpp");
USEUNIT("date_class.cpp");
USEUNIT("StringTokenizer.cpp");
USEUNIT("XMLTreeNode.cpp");
USEUNIT("TreeNode.cpp");
USEUNIT("VirtualTreeNode.cpp");
USEUNIT("XMLTreeData.cpp");
USEUNIT("TreeData.cpp");
USEUNIT("MacroSubstFile.cpp");
USEUNIT("Macro.cpp");
USEUNIT("MacroValue.cpp");
USEUNIT("db_functions.cpp");
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
   return 1;
}
//---------------------------------------------------------------------------
 