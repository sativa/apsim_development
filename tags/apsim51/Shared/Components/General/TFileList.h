//---------------------------------------------------------------------------
#ifndef TFileListH
#define TFileListH

#include <vcl\dsgnintf.hpp>
// ------------------------------------------------------------------
//  Short description:
//      This class encapsulates a collection of filenames.
//      A property editor has been defined to display a file open
//      dialog for this class.

//  Notes:
//      Extensions should be a list of ".mdb" type strings.
//      No filespecs please.

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
class PACKAGE TFileList : public TPersistent
   {
   private:
      TStringList* FFile_names;
      TStringList* FExtensions;
      bool FMulti_select;
      AnsiString Create_filter_string(void);
      
   protected:
   public:
      __fastcall TFileList(void);
      __fastcall ~TFileList(void);

      void __fastcall Edit();

   __published:
      __property TStringList* Extensions = {read=FExtensions, write=FExtensions};
      __property bool Multi_select = {read=FMulti_select, write=FMulti_select};
      __property TStringList* File_names = {read=FFile_names, write=FFile_names};
   };
// ------------------------------------------------------------------
//  Short description:
//      This class encapsulates a property editor for the
//      TFileList class.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
class TFileListEditor : public TPropertyEditor
   {
   public:
      __fastcall TFileListEditor(void) { }

      TPropertyAttributes __fastcall GetAttributes()
         {return (TPropertyAttributes() << paDialog);}
      void __fastcall Edit();

   };


#endif
