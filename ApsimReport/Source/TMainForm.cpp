//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TMainForm.h"
#include "TRenameFileForm.h"
#include "TPageSetupForm.h"
#include <general\vcl_functions.h>
#include <general\inifile.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "TSEGReport"
#pragma link "TSEGLibrary"
#pragma resource "*.dfm"
TMainForm *MainForm;

//---------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* Owner)
   : TForm(Owner)
   {
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::FormShow(TObject *Sender)
   {
   SEGReport1->zoom = textToZoom(ZoomEdit->Text);
   SEGReport1->clear();
   SEGReport1->showReportInspector = true;

   Path iniPath(Application->ExeName.c_str());
   iniPath.Set_extension(".ini");
   IniFile ini;
   ini.setFileName(iniPath.Get_path());
   string leftSt, topSt, widthSt, heightSt;
   ini.read("MainFormPos", "Left", leftSt);
   ini.read("MainFormPos", "Top", topSt);
   ini.read("MainFormPos", "Width", widthSt);
   ini.read("MainFormPos", "Height", heightSt);
   if (leftSt != "" && topSt != "" && widthSt != "" && heightSt != "")
      {
      Left = atoi(leftSt.c_str());
      Top = atoi(topSt.c_str());
      Width = atoi(widthSt.c_str());
      Height = atoi(heightSt.c_str());
      }
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::LayoutModeExecute(TObject *Sender)
   {
   SEGReport1->layoutMode = !SEGReport1->layoutMode;
   LayoutMode->Checked = SEGReport1->layoutMode;
   AdvancedEditMode->Checked = SEGReport1->editMode;
   }
//--------------------------------------------------------------------------
void __fastcall TMainForm::AdvancedEditModeExecute(TObject *Sender)
   {
   SEGReport1->editMode = !SEGReport1->editMode;
   LayoutMode->Checked = SEGReport1->layoutMode;
   AdvancedEditMode->Checked = SEGReport1->editMode;
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::ExitExecute(TObject *Sender)
   {
   saveIfNecessary();   
   Close();
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::NewExecute(TObject *Sender)
   {
   saveIfNecessary();
   if (SEGLibrary1->letUserSelectFile(filename))
      {
      open(filename);
      filename = "";
      setCaption();
      }
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::OpenExecute(TObject *Sender)
   {
   if (OpenDialog1->Execute())
      {
      saveIfNecessary();
      open(OpenDialog1->FileName);
      ZoomUpDown->Position = SEGReport1->zoom;
      }
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::SaveExecute(TObject *Sender)
   {
   save(filename);
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::SaveAsExecute(TObject *Sender)
   {
   if (SaveDialog1->Execute())
      save(SaveDialog1->FileName);
   }
//---------------------------------------------------------------------------
void TMainForm::open(AnsiString file)
   {
   filename = file;
   SEGReport1->load(filename);
   setCaption();
   }
//---------------------------------------------------------------------------
void TMainForm::save(AnsiString file)
   {
   if (file == "")
      SaveAsExecute(NULL);
   else
      {
      filename = file;
      setCaption();
      SEGReport1->save(filename);
      }
   }
//---------------------------------------------------------------------------
void TMainForm::setCaption(void)
   {
   static const char* APPLICATION_CAPTION = "APSIM Report";

   AnsiString caption = APPLICATION_CAPTION;
   if (filename != "")
      caption += " - " + ExtractFileName(filename);
   Caption = caption;
   }
//---------------------------------------------------------------------------
int TMainForm::textToZoom(AnsiString zoomText)
   {
   int posPercent = zoomText.Pos("%");
   if (posPercent == 0)
      posPercent = zoomText.Length() + 1;
   return StrToInt(zoomText.SubString(1, zoomText.Pos("%")-1));
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::ZoomEditKeyDown(TObject *Sender, WORD &Key,
      TShiftState Shift)
   {
   if (Key == VK_RETURN)
      {
      int zoom = textToZoom(ZoomEdit->Text);
      ZoomUpDown->Position = zoom;
      }
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::DeleteFileExecute(TObject *Sender)
   {
   if (Application->MessageBox("Are you sure?", "Question", MB_ICONSTOP | MB_YESNO)
       == IDYES)
      {
//      ::DeleteFile(FileListBox->FileName.c_str());
//      FileListBox->Update();
      }
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::CopyFileExecute(TObject *Sender)
   {
//   fileThatWasCopied = FileListBox->FileName;
//   FileListBox->ItemIndex = -1;
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::PasteFileExecute(TObject *Sender)
   {
   if (fileThatWasCopied != "")
      {
      // call Windows api routine
      SHFILEOPSTRUCT op;
      ZeroMemory(&op,sizeof(op));
      op.hwnd=GetForegroundWindow();
      op.wFunc=FO_COPY;
      op.pFrom=fileThatWasCopied.c_str();
      AnsiString destDir = ExtractFileDir(fileThatWasCopied.c_str());
      op.pTo=destDir.c_str();
      op.fFlags = FOF_RENAMEONCOLLISION;
      SHFileOperation(&op);
//      FileListBox->Update();
      }
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::RenameFileExecute(TObject *Sender)
   {
/*   AnsiString oldName = FileListBox->FileName;
   RenameFileForm->Edit1->Text = ExtractFileName(oldName);
   if (RenameFileForm->ShowModal() == mrOk)
      {
      AnsiString newName = ExtractFileDir(oldName) + "\\" + RenameFileForm->Edit1->Text;

      // call Windows api routine
      SHFILEOPSTRUCT op;
      ZeroMemory(&op,sizeof(op));
      op.hwnd=GetForegroundWindow();
      op.wFunc=FO_RENAME;
      op.pFrom=oldName.c_str();
      op.pTo=newName.c_str();
      SHFileOperation(&op);
      FileListBox->Update();
      }
*/   }

void __fastcall TMainForm::SaveEnvironmentExecute(TObject *Sender)
   {
   SEGReport1->saveEnvironment();
   Path iniPath(Application->ExeName.c_str());
   iniPath.Set_extension(".ini");
   IniFile ini;
   ini.setFileName(iniPath.Get_path());
   ini.write("MainFormPos", "Left", IntToStr(Left).c_str());
   ini.write("MainFormPos", "Top", IntToStr(Top).c_str());
   ini.write("MainFormPos", "Width", IntToStr(Width).c_str());
   ini.write("MainFormPos", "Height", IntToStr(Height).c_str());
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::PageSetupExecute(TObject *Sender)
   {
   PageSetupForm->TopEdit->Text = IntToStr((int)SEGReport1->topMargin);
   PageSetupForm->LeftEdit->Text = IntToStr((int)SEGReport1->leftMargin);
   PageSetupForm->BottomEdit->Text = IntToStr((int)SEGReport1->bottomMargin);
   PageSetupForm->RightEdit->Text = IntToStr((int)SEGReport1->rightMargin);
   PageSetupForm->PortraitButton->Down = SEGReport1->isPortrait;
   PageSetupForm->LandscapeButton->Down = !SEGReport1->isPortrait;
   if (PageSetupForm->ShowModal() == mrOk)
      {
      SEGReport1->topMargin = StrToInt(PageSetupForm->TopEdit->Text);
      SEGReport1->leftMargin = StrToInt(PageSetupForm->LeftEdit->Text);
      SEGReport1->bottomMargin = StrToInt(PageSetupForm->BottomEdit->Text);
      SEGReport1->rightMargin = StrToInt(PageSetupForm->RightEdit->Text);
      SEGReport1->isPortrait = PageSetupForm->PortraitButton->Down;
      }
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::PrintExecute(TObject *Sender)
   {
   SEGReport1->print();
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::ZoomEditChange(TObject *Sender)
   {
   ZoomEdit->Text = IntToStr(ZoomUpDown->Position) + "%";
   SEGReport1->zoom = ZoomUpDown->Position;
   }
//---------------------------------------------------------------------------
// Perform a save if the report needs saving and the user says so.
//---------------------------------------------------------------------------
void TMainForm::saveIfNecessary(void)
   {
   if (SEGReport1->needsSaving && MessageBox(NULL, "Save changes to report?", "Confirm",
                             MB_ICONQUESTION | MB_YESNO) == IDYES)
      save(filename);
   }
//---------------------------------------------------------------------------
// Send the current report to the library
//---------------------------------------------------------------------------
void __fastcall TMainForm::SendToLibraryExecute(TObject *Sender)
   {
   saveIfNecessary();
   if (filename != "")
      SEGLibrary1->sendToLibrary(filename);
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::LibraryExecute(TObject *Sender)
   {
   SEGLibrary1->letUserManipulate();
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::FormClose(TObject *Sender, TCloseAction &Action)
   {
   SEGReport1->editMode = false;
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::RefreshActionExecute(TObject *Sender)
   {
   SEGReport1->refresh();
   }
//---------------------------------------------------------------------------

