//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TMainForm.h"
#include "TPageSetupForm.h"
#include "TReportForm.h"
#include "TObjectInspectorForm.h"
#include "TDataPreviewForm.h"
#include <general\vcl_functions.h>
#include <general\io_functions.h>
#include <general\inifile.h>
#include <general\path.h>
#include <ApsimShared\ApsimSettings.h>
#include <TApsimFileReader.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "TSEGReport"
#pragma link "TSEGLibrary"
#pragma link "MRUFList"
#pragma link "JPEG"
#pragma resource "*.dfm"
TMainForm *MainForm;
extern AnsiString commandLine;

//---------------------------------------------------------------------------
// constructor.
//---------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* Owner)
   : TForm(Owner)
   {
   }
//---------------------------------------------------------------------------
// Form has been shown - set everything up.
//---------------------------------------------------------------------------
void __fastcall TMainForm::FormShow(TObject *Sender)
   {
   ReportForm->Parent = this;
   ReportForm->Visible = true;

   ReportForm->Report->clear();
   ReportForm->Report->createPage("Page1");
   ZoomUpDown->Position = ReportForm->Report->zoom;
   ReportForm->Report->getPageNames(ReportForm->TabControl->Tabs);
   ReportForm->onPageChanged = pageChanged;

   ApsimSettings settings;
   string leftSt, topSt, widthSt, heightSt;
   settings.read("Apsim Report Pos|MainFormLeft", leftSt);
   settings.read("Apsim Report Pos|MainFormTop", topSt);
   settings.read("Apsim Report Pos|MainFormWidth", widthSt);
   settings.read("Apsim Report Pos|MainFormHeight", heightSt);
   if (leftSt != "" && topSt != "" && widthSt != "" && heightSt != "")
      {
      Left = atoi(leftSt.c_str());
      Top = atoi(topSt.c_str());
      Width = atoi(widthSt.c_str());
      Height = atoi(heightSt.c_str());
      }
   if (commandLine != "")
      processCommandLine(commandLine);
   }
//---------------------------------------------------------------------------
// Form has been closed - save everything.
//---------------------------------------------------------------------------
void __fastcall TMainForm::FormClose(TObject *Sender, TCloseAction &Action)
   {
   ReportForm->Report->edit(false);
   saveIfNecessary();
   ApsimSettings settings;
   settings.write("Apsim Report Pos|MainFormLeft", IntToStr(Left).c_str());
   settings.write("Apsim Report Pos|MainFormTop", IntToStr(Top).c_str());
   settings.write("Apsim Report Pos|MainFormWidth", IntToStr(Width).c_str());
   settings.write("Apsim Report Pos|MainFormHeight", IntToStr(Height).c_str());
   }
//---------------------------------------------------------------------------
// User wants to edit the report.
//---------------------------------------------------------------------------
void __fastcall TMainForm::EditReportActionExecute(TObject *Sender)
   {
   edit(false);
   if (EditReportAction->Checked)
      {
      ReportForm->Report->showDataPage(false);
      edit(true);
      }
   DataPreviewForm->Visible = false;
   }
//---------------------------------------------------------------------------
// User wants to edit the data.
//---------------------------------------------------------------------------
void __fastcall TMainForm::EditDataActionExecute(TObject *Sender)
   {
   saveFormPosition(DataPreviewForm);
   DataPreviewForm->Visible = EditDataAction->Checked;
   edit(false);
   if (EditDataAction->Checked)
      {
      ReportForm->TabControl->TabHeight = 1;
      ReportForm->TabControl->TabWidth = 1;
      ReportForm->Report->showDataPage(true);
      edit(true);
      }
   else
      {
      ReportForm->Report->showDataPage(false);
      ReportForm->TabControl->TabHeight = 0;  // auto size.
      ReportForm->TabControl->TabWidth = 0;
      }
   loadFormPosition(DataPreviewForm);
   }
//---------------------------------------------------------------------------
// Tell report to go into edit mode.
//---------------------------------------------------------------------------
void TMainForm::edit(bool turnOn)
   {
   static TForm* palette = NULL;
   if (palette != NULL)
      {
      saveFormPosition(palette);
      saveFormPosition(ObjectInspectorForm);
      }
   // Hook up the object inspector to the report and make it visible.
   ReportForm->Report->OnSelectionChanged = selectionChanged;
   ObjectInspectorForm->Visible = turnOn;

   // turn on edit mode and make palette dockable.
   palette = ReportForm->Report->edit(turnOn);
   if (palette != NULL)
      {
      palette->DragMode = dmAutomatic;
      palette->DragKind = dkDock;
      loadFormPosition(palette);
      loadFormPosition(ObjectInspectorForm);
      }
   if (!turnOn)
      {
      HideDockPanel(LeftDockPanel);
      HideDockPanel(BottomDockPanel);
      HideDockPanel(RightDockPanel);
      populateToolBar();
      }
   }
//---------------------------------------------------------------------------
// User has changed selections - update our data window and pass event along
// object inspector.
//---------------------------------------------------------------------------
void __fastcall TMainForm::selectionChanged(TObject* sender)
   {
   ObjectInspectorForm->ObjectInspector->update(sender);
   loadFormPosition(ObjectInspectorForm);
   TSEGTable* data = dynamic_cast<TSEGTable*> (sender);
   if (data != NULL)
      {
      DataPreviewForm->DataSource->DataSet = data;
      loadFormPosition(DataPreviewForm);
      }
   }
//---------------------------------------------------------------------------
// User has changed pages in tab control - update main form.
//---------------------------------------------------------------------------
void __fastcall TMainForm::pageChanged(TObject* sender)
   {
   ZoomUpDown->Position = ReportForm->Report->zoom;
   }
//---------------------------------------------------------------------------
// Populate the toolbar.
//---------------------------------------------------------------------------
void TMainForm::populateToolBar(void)
   {
   ReportForm->Report->populateToolBar(ReportToolBar);
   ReportToolBar->Visible = (ReportToolBar->ButtonCount > 0);
   for (int b = 0; b != ReportToolBar->ButtonCount; b++)
      {
      ReportToolBar->Buttons[b]->OnClick = ReportButtonClick;
      ReportToolBar->Buttons[b]->Hint = ReportToolBar->Buttons[b]->Caption;
      }
   }
//---------------------------------------------------------------------------
// user has clicked a report toolbar button.
//---------------------------------------------------------------------------
void __fastcall TMainForm::ReportButtonClick(TObject* sender)
   {
   TToolButton* button = dynamic_cast<TToolButton*>(sender);
   ObjectInspectorForm->Visible = true;
   loadFormPosition(ObjectInspectorForm);
   ObjectInspectorForm->ObjectInspector->update((TComponent*)button->Tag);
   }
//---------------------------------------------------------------------------
// User has clicked exit.
//---------------------------------------------------------------------------
void __fastcall TMainForm::ExitActionExecute(TObject *Sender)
   {
   Close();
   }
//---------------------------------------------------------------------------
// User has clicked new.
//---------------------------------------------------------------------------
void __fastcall TMainForm::NewActionExecute(TObject *Sender)
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
// User has clicked open.
//---------------------------------------------------------------------------
void __fastcall TMainForm::OpenActionExecute(TObject *Sender)
   {
   if (OpenDialog1->Execute())
      {
      saveIfNecessary();
      open(OpenDialog1->FileName);
      ZoomUpDown->Position = ReportForm->Report->zoom;
      }
   }
//---------------------------------------------------------------------------
// User has clicked save.
//---------------------------------------------------------------------------
void __fastcall TMainForm::SaveActionExecute(TObject *Sender)
   {
   save(filename);
   }
//---------------------------------------------------------------------------
// User has clicked saveAs
//---------------------------------------------------------------------------
void __fastcall TMainForm::SaveAsActionExecute(TObject *Sender)
   {
   if (SaveDialog1->Execute())
      save(SaveDialog1->FileName);
   }
//---------------------------------------------------------------------------
// Go open the specified file.
//---------------------------------------------------------------------------
void TMainForm::open(AnsiString file)
   {
   filename = file;
   ReportForm->Report->load(filename);
   setCaption();
   ZoomUpDown->Position = ReportForm->Report->zoom;
   MRUFileList->AddItem(filename);
   ReportForm->Report->getPageNames(ReportForm->TabControl->Tabs);
   populateToolBar();
   }
//---------------------------------------------------------------------------
// Go save to the specified file.
//---------------------------------------------------------------------------
void TMainForm::save(AnsiString file)
   {
   if (file == "")
      SaveAsActionExecute(NULL);
   else
      {
      if (ExtractFileExt(file) == ".report")
         {
         filename = file;
         setCaption();
         }
      ReportForm->Report->save(file);
      MRUFileList->AddItem(filename);
      }
   }
//---------------------------------------------------------------------------
// Set the caption of the application.
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
void __fastcall TMainForm::CopyToClipboardActionExecute(TObject *Sender)
   {
   ReportForm->Report->copyToClipboard();
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::PageSetupActionExecute(TObject *Sender)
   {
   PageSetupForm->PortraitButton->Down = ReportForm->Report->isPortrait;
   PageSetupForm->LandscapeButton->Down = !ReportForm->Report->isPortrait;
   if (PageSetupForm->ShowModal() == mrOk)
      {
      ReportForm->Report->isPortrait = PageSetupForm->PortraitButton->Down;
      }
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::PrintActionExecute(TObject *Sender)
   {
   ReportForm->Report->print(false);
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::PrintCurrentPageActionExecute(TObject *Sender)
   {
   ReportForm->Report->print(true);
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::ZoomEditChange(TObject *Sender)
   {
   static bool inHere = false;
   if (!inHere)
      {
      inHere = true;
      ZoomEdit->Text = IntToStr(ZoomUpDown->Position) + "%";
      ReportForm->Report->zoom = ZoomUpDown->Position;
      inHere = false;
      }
   }
//---------------------------------------------------------------------------
// Perform a save if the report needs saving and the user says so.
//---------------------------------------------------------------------------
void TMainForm::saveIfNecessary(void)
   {
   if (ReportForm->Report->needsSaving && MessageBox(NULL, "Save changes to report?", "Confirm",
                             MB_ICONQUESTION | MB_YESNO) == IDYES)
      save(filename);
   }
//---------------------------------------------------------------------------
// Send the current report to the library
//---------------------------------------------------------------------------
void __fastcall TMainForm::SendToLibraryActionExecute(TObject *Sender)
   {
   saveIfNecessary();
   if (filename != "")
      SEGLibrary1->sendToLibrary(filename);
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::LibraryActionExecute(TObject *Sender)
   {
   SEGLibrary1->letUserManipulate();
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::RefreshActionExecute(TObject *Sender)
   {
   ReportForm->Report->refresh();
   }
//---------------------------------------------------------------------------
// Use has clicked on a MRU filename - load it into the report.
//---------------------------------------------------------------------------
void __fastcall TMainForm::MRUFileListMRUItemClick(TObject *Sender,
      AnsiString AFilename)
   {
   open(AFilename);
   }
//---------------------------------------------------------------------------
// process the specified command line.
// e.g. commandline:
//    apsimreport test.report wmf farm1.out farm2.out ...
// all are optional.
//---------------------------------------------------------------------------
void TMainForm::processCommandLine(AnsiString commandLine)
   {
   vector<string> commandWords;
   SplitStringHonouringQuotes(commandLine.c_str(), " ", commandWords);
   if (commandWords.size() >= 1)
      {
      string reportFileName = commandWords[0];
      open(reportFileName.c_str());
      if (commandWords.size() >= 2)
         {
         string outputFileName = commandWords[1];

         for (unsigned i = 2; i != commandWords.size(); i++)
            {
            replaceAll(commandWords[i], "\"", "");
            unsigned posPeriod = commandWords[i].find('.');
            if (posPeriod != string::npos)
               {
               string objectName = commandWords[i].substr(0, posPeriod);
               string propertyLine = commandWords[i].substr(posPeriod+1);
               string propertyName, propertyValue;
               unsigned posEquals = propertyLine.find('=');
               if (posEquals != string::npos)
                  {
                  propertyName = propertyLine.substr(0, posEquals);
                  propertyValue = propertyLine.substr(posEquals+1);

                  // get the first file reader and pass the output file to it.
                  TComponent* reportComp = getComponent<TComponent> (ReportForm->Report, "Report");
                  TSEGTable* object = getComponent<TSEGTable> (reportComp, objectName.c_str());
                  if (object != NULL)
                     object->setProperty(propertyName, propertyValue);
                  }
               }
            }
         Path(reportFileName).Change_directory();
         save(outputFileName.c_str());
         Close();
         }
      }
   }
//---------------------------------------------------------------------------
// Show dock panel.
//---------------------------------------------------------------------------
void TMainForm::ShowDockPanel(TWinControl* APanel, TControl* Client, TRect& dockRect)
   {
   //Client - the docked client to show.

   APanel->Visible = true;
   if (APanel == LeftDockPanel)
      LeftSplitter->Visible = true;
   else if (APanel == BottomDockPanel)
      BottomSplitter->Visible = true;
   else
      RightSplitter->Visible = true;

   if (APanel == LeftDockPanel)
      {
      APanel->Width = dockRect.Right - dockRect.Left;
      LeftSplitter->Left = APanel->Width + LeftSplitter->Width;
      }
   else if (APanel == BottomDockPanel)
      {
      APanel->Height = dockRect.Bottom - dockRect.Top;
      BottomSplitter->Top = ClientHeight - APanel->Height - BottomSplitter->Width;
      }
   else if (APanel == RightDockPanel)
      {
      APanel->Width = dockRect.Right - dockRect.Left;
      RightSplitter->Left = Width - APanel->Width - RightSplitter->Width;
      }
   Client->Show();
   }
//---------------------------------------------------------------------------
// Show dock panel.
//---------------------------------------------------------------------------
void TMainForm::HideDockPanel(TPanel* APanel)
   {
   // Since docking to a non-visible docksite isn't allowed, instead of setting
   // Visible for the panels we set the width to zero. The default InfluenceRect
   // for a control extends a few pixels beyond it's boundaries, so it is possible
   // to dock to zero width controls.

   // Don't try to hide a panel which has visible dock clients.
   if (APanel->VisibleDockClientCount > 1)
      return;

   if (APanel == LeftDockPanel)
      LeftSplitter->Visible = false;
   else if (APanel == BottomDockPanel)
      BottomSplitter->Visible = false;
   else
      RightSplitter->Visible = false;

   if (APanel == LeftDockPanel)
      APanel->Width = 0;
   else if (APanel == BottomDockPanel)
      APanel->Height = 0;
   else if (APanel == RightDockPanel)
      APanel->Width = 0;
   }

//---------------------------------------------------------------------------
void __fastcall TMainForm::FormDockOver(TObject *Sender,
      TDragDockObject *Source, int X, int Y, TDragState State,
      bool &Accept)
   {
   TPanel* SenderPanel = dynamic_cast<TPanel*>(Sender);

   Accept = true; //(dynamic_cast<TDockableForm*>(Source->Control) != NULL);
   if (Accept)
      {
      // Modify the DockRect to preview dock area.
      Types::TPoint TopLeft = ClientToScreen(
        Point(SenderPanel->Left, SenderPanel->Top));
      Types::TPoint BottomRight;
      if (SenderPanel->Align == alBottom)
         {
         TopLeft = ClientToScreen(Point(SenderPanel->Left, SenderPanel->Top-ClientHeight/3));
         BottomRight = ClientToScreen(Point(SenderPanel->Width, SenderPanel->Top));
         }

      else if (SenderPanel->Align == alRight)
         BottomRight = SenderPanel->ClientToScreen(
            Point(-this->ClientWidth / 3, SenderPanel->Height));

      else
         BottomRight = SenderPanel->ClientToScreen(
            Point(this->ClientWidth / 3, SenderPanel->Height));
         Source->DockRect = Types::TRect(TopLeft, BottomRight);
      }
   }
//---------------------------------------------------------------------------

void __fastcall TMainForm::LeftDockPanelGetSiteInfo(TObject *Sender,
      TControl *DockClient, TRect &InfluenceRect, TPoint &MousePos,
      bool &CanDock)
   {
   // If CanDock is true, the panel will not automatically draw the preview rect.
//   CanDock = (dynamic_cast<TDockableForm*>(DockClient) != NULL);
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::FormDockDrop(TObject *Sender,
      TDragDockObject *Source, int X, int Y)
   {
   TPanel* SenderPanel = dynamic_cast<TPanel*>(Sender);
   if (SenderPanel == NULL)
      throw EInvalidCast("");

   // OnDockDrop gets called AFTER the client has actually docked,
   // so we check for DockClientCount = 1 before making the dock panel visible.
  if (SenderPanel->DockClientCount == 1)
      ShowDockPanel(SenderPanel, Source->Control, Source->DockRect);

   // Make DockManager repaints it's clients.
   SenderPanel->DockManager->ResetBounds(true);
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::FormUnDock(TObject *Sender,
      TControl *Client, TWinControl *NewTarget, bool &Allow)
   {
   TPanel* SenderPanel = dynamic_cast<TPanel*>(Sender);
   if (SenderPanel == NULL)
      throw EInvalidCast("");

   // OnUnDock gets called BEFORE the client is undocked, in order to optionally
   // disallow the undock. DockClientCount is never 0 when called from this event.
   if (SenderPanel->DockClientCount == 1)
      HideDockPanel(SenderPanel);
   }

//---------------------------------------------------------------------------
// load the position a form from settings file.
//---------------------------------------------------------------------------
void TMainForm::loadFormPosition(TForm* form)
   {
   form->Visible = true;

   ApsimSettings settings;
   string keyPrefix = string("Apsim Report Pos|") + form->Name.c_str() + string("_");
   int left, top, width, height;

   settings.read(keyPrefix + "Left", left);
   settings.read(keyPrefix + "Top", top);
   settings.read(keyPrefix + "Width", width);
   settings.read(keyPrefix + "Height", height);
   form->Left = left;
   form->Top = top;
   form->Width = width;
   form->Height = height;
   string dockSiteName;
   settings.read(keyPrefix + "DockSite", dockSiteName);
   if (dockSiteName != "")
      {
      string alignString;
      settings.read(keyPrefix + "DockSiteAlign", alignString);
      TAlign align = alLeft;
      if (alignString == "alLeft")
         align = alLeft;
      else if (alignString == "alTop")
         align = alTop;
      else if (alignString == "alRight")
         align = alRight;
      else if (alignString == "alBottom")
         align = alBottom;
      else if (alignString == "alNone")
         align = alNone;
      else
         {
         string msg = "Invalid align type in positionControl: " + alignString;
         ShowMessage(msg.c_str());
         }

      TWinControl* dockSite = getComponent<TWinControl>(this, dockSiteName.c_str());
      if (form->Parent != dockSite)
         {
         form->Parent = dockSite;
         form->ManualDock(dockSite, NULL, align);
         }
      else if (dockSite != NULL)
         {
         TPanel* panel = dynamic_cast<TPanel*>(dockSite);
         if (panel != NULL)
            {
            ShowDockPanel(panel, form, form->ClientRect);
            panel->DockManager->ResetBounds(true);
            }
         }
      }
   }
//---------------------------------------------------------------------------
// save the position of a form from settings file.
//---------------------------------------------------------------------------
void TMainForm::saveFormPosition(TForm* form)
   {
   if (form->Visible)
      {
      ApsimSettings settings;
      string keyPrefix = string("Apsim Report Pos|") + form->Name.c_str() + string("_");

      settings.write(keyPrefix + "Left", form->Left);
      settings.write(keyPrefix + "Top", form->Top);
      settings.write(keyPrefix + "Width", form->Width);
      settings.write(keyPrefix + "Height", form->Height);
      if (form->Parent != NULL)
         {
         settings.write(keyPrefix + "DockSite", form->Parent->Name.c_str());

         string alignString;
         if (form->Align == alLeft)
            alignString = "alLeft";
         else if (form->Align == alTop)
            alignString = "alTop";
         else if (form->Align == alRight)
            alignString = "alRight";
         else if (form->Align == alBottom)
            alignString = "alBottom";
         else if (form->Align == alNone)
            alignString = "alNone";
         settings.write(keyPrefix + "DockSiteAlign", alignString);
         }
      else
         {
         settings.deleteKey(keyPrefix + "DockSite");
         settings.deleteKey(keyPrefix + "DockSiteAlign");
         }
      }
   }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------


