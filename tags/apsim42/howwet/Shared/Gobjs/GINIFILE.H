#if !defined (__ginifile_h)
#define __ginifile_h

#include <gobjs\gpath.h>
#include <gobjs\gstring.h>

// *******************************************************************
       class GOBJS_CLASS_EXPORT GIni_file {
// *******************************************************************

//  Short description:
//    Class handling the reading and writting to/from INI files.

//  Notes:

//  Changes:
//    DPH 17/11/94

// ------------------------- Private section -------------------------

   private :

//   Private data
      GPath INI_file_name;             // Name of ini file

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
      INI_file(void);                  // constructor

      void Set_file_name(const GString& File_name);
                                       // set the name of the ini file.
      void Get_file_path(GPath& Ini_path)
         {Ini_path = INI_file_name;};  // return the full path of .ini file to caller
      virtual void Read(const GString& Section, const GString& Key_name,
                        GString& Str);
                                       // read a GString from ini file.
      void Read(const GString& Section, const GString& Key_name,
                char *Str, size_t Size);
                                       // read a char * from ini file.
      void Read_list(const GString& Section, const GString& Key_name,
                     String_array& String_list);
                                       // write a list of strings
      virtual void Write(const GString& Section, const GString& Key_name,
                         const GString& Str);
                                       // write a GString to ini file.
      void Write_list(const GString& Section, const GString& Key_name,
                      String_array& String_list);
                                       // write a list of strings out.
      void Delete_key(const GString& Section, const GString& Key_name);
                                       // Delete the key_name from the .ini file
      void Delete_section(const GString& Section);
                                       // Delete the section from the .ini file
   };

#endif
