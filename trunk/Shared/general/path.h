#ifndef PATH_H
#define PATH_H

#include <general\string_functions.h>

// ------------------------------------------------------------------
//  Short description:
//    Class handling the manipulation of file paths.

//  Notes:
//		Terms :
//		   Drive     -> d:
//			Directory -> d:\bin
//			Name      -> test.con
//			Extension -> .con
//			Path      -> d:\bin\test.con

//  Changes:
//    DPH 17/11/94
//    DPH 13/5/1997 - reworked to use standard template library.

// ------------------------------------------------------------------
class Path
	{
   private:
      string Drive;
      string Directory;
      string Name;

	public:
      Path(void) {};
      Path(const char* File_path) {Set_path(File_path);};
      int operator== (const Path& From) const {return (Directory == From.Directory &&
                                                 Name == From.Name);};
      int operator< (const Path& From) const {return (Directory < From.Directory &&
                                                 Name < From.Name);};

      string Get_drive(void);
		string Get_directory(void);
		string Get_name(void);
      string Get_name_without_ext(void);
		string Get_extension(void);
      string Get_path(void);
      string Get_full_path (void);              // always returns a full absolute path.

      void Set_to_cwd(void);
      void Set_drive(const char* New_drive);
      void Set_directory(const char* New_directory);
      void Set_name(const char* New_name);
      void Set_extension(const char* New_extension);
      void Set_path(const char* New_path); 

      bool Is_empty(void);
      bool Exists(void);
      void Change_directory(void);

      void Append_directory (const char* Directory);
      void Append_relative_path (const char* Relative_path);
      string Back_up_directory (void);
	};

#endif

