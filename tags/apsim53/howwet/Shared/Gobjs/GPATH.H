#if !defined (__gpath_h)
#define __gpath_h


#include <gobjs\gstring.h>
#include <classlib\arrays.h>

// *******************************************************************
		 class GOBJS_CLASS_EXPORT GPath {
// *******************************************************************

//  Short description:
//		Path/File name class for handling file names and directories.

//  Notes:

//  Changes:
//		DPH 16/11/94

// ------------------------- Private section -------------------------

	private :

//   Private data
		GString Path_way;						// String for storing the raw file name.

//   Private member functions
//      none

// ------------------------- Protected section -----------------------

	protected :

//   Protected data
//      none

//   Protected member functions
//      none

// -------------------------- Public section -------------------------

	public :

//   Public data
//      none

//   Public member functions
      GPath(void) {};                  // default constructor
      GPath(const GString& Str)  {Path_way = Str;};
                                       // constructor
      GPath(const char *Str)  {Path_way = Str;};
                                       // constructor
		GString Name(void);              // Return the file name part of path
      GString Drive(void);             // Return the drive part of path.  e.g. c:
		GString Directory(void);         // Return the directory part of path
		GString Extension(void);         // Return the extension part of path
      GString Full_path(void);         // Return the full path GString
      bool Is_empty(void);             // Return TRUE is path is empty.

		void To_unix(const GString& DOS_file_system,
                   const GString& UNIX_file_system);
                                       // Convert the path to unix format.
      void To_dos(const GString& DOS_file_system,
                  const GString& UNIX_file_system);
                                       // Convert the path to dos format.

      int operator==(GPath&)
         {return FALSE;};              // equals operator

//      GPath& operator = (const GString& Str)
//         {
//         Path_way.assign(Str);
//         return *this;
//         }                             // Set the path to the specified GString.
      void Set_to_cwd(void);           // Delete the path and set it to the
                                       // current working directory.
      void Set_name(const GString& File_name);
                                       // Set the file name part of path
      void Set_directory(const GString& Directory);
                                       // Set the directory part of path
      void Set_extension(const GString& Extension);
                                       // Set the extension part of path
      void Make_relative(void);        // Make sure the path is relative to
                                       // current working directory.
      void Make_absolute(void);        // Make sure the path is an absolute path.
      bool Exists(void);               // Does the current file exist ?
      void Change_directory(void);     // Change the current working directory
                                       // to what the path holds.
	};
typedef TArrayAsVector <GPath> GPath_array;

#endif  // sentry

