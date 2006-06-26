//---------------------------------------------------------------------------
#ifndef File_list_viewH
#define File_list_viewH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <Forms.hpp>
#include <StdCtrls.hpp>
#include <general\mylist.h>
#include <general\mystring.h>
#include "TDirectoryTree.h"
// ------------------------------------------------------------------
//  Short description:
//      This extends TListView and provides functionality to
//      display a list of file names.

//  Notes:

//  Changes:
//    DPH 11/9/98

// ------------------------------------------------------------------
class PACKAGE TFileListView : public TListView
   {
   private:
      TDragTextSource* FDropSource;
      TDragTextTarget* FDropTarget;

      bool FAllowDelete;
      bool FDeleteConfirmation;
      int FRecurse_subdirectory_level;
      TStringList* FDirectories;
      AnsiString FFilespec;
      TDirectoryTree* FDirectory_tree;
      TNotifyEvent FOnItemsAddedRemoved;

      TImageList* Image_list;
      Graphics::TBitmap* Bitmap;


      // property setters.
      void __fastcall Set_directories (TStringList* Directories);
      void __fastcall Set_recurse_subdirectory_level(int Recurse_subdirectory_level);
      void __fastcall Set_filespec (AnsiString Filespec);
      void __fastcall Set_directory_tree (TDirectoryTree* Directory_tree);
      void __fastcall SetDropSource (TDragTextSource* DropSource);
      void __fastcall SetDropTarget (TDragTextTarget* DropTarget);

      // events we need to trap.
      void __fastcall Directory_tree_change(TTreeNodeShape* Node);
      void __fastcall KeyDown(unsigned short &Key,  Classes::TShiftState Shift);
      void __fastcall StartDrag(TObject *Sender, TDragObject *&DragObject);
      void __fastcall Drop(TObject *Sender, TWinControl *Acceptor, const AnsiString dropText, int X, int Y);

   protected:
      virtual void __fastcall Loaded();

      virtual void Refresh             (void);
      virtual TListItem* Add_file      (const char* filename);
      virtual void Delete_file         (const char* filename);
      virtual void DeleteSelectedItems (void);
      TListItem* Find_file             (const char* filename);
      string GetSingleDirectory        (void);

   public:
      __fastcall TFileListView(TComponent* Owner);
      __fastcall ~TFileListView();

      void Get_files (list<string>& File_list);
      void Set_files (list<string>& File_list);
      void Add_files (list<string>& File_list);
      void GetSelectedFiles (list<string>& Files);

   __published:
      __property bool AllowDelete = {read=FAllowDelete, write=FAllowDelete, nodefault};
      __property bool DeleteConfirmation = {read=FDeleteConfirmation, write=FDeleteConfirmation, nodefault};
      __property TStringList* Directories = {read=FDirectories, write=Set_directories};
      __property int Recurse_subdirectory_level = {read=FRecurse_subdirectory_level, write=Set_recurse_subdirectory_level};
      __property AnsiString Filespec = {read=FFilespec, write=Set_filespec};
      __property TDirectoryTree* Directory_tree = {read=FDirectory_tree, write=Set_directory_tree};
      __property TDragTextSource* DropSource = {read=FDropSource, write=SetDropSource};
      __property TDragTextTarget* DropTarget = {read=FDropTarget, write=SetDropTarget};
      __property TNotifyEvent OnItemsAddedRemoved = {read=FOnItemsAddedRemoved, write=FOnItemsAddedRemoved};
   };
//---------------------------------------------------------------------------
#endif
