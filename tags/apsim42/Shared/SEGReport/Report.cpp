//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "Report.h"
#include "ComponentRegistration.h"

#include <fstream>
#include <QuickRpt.hpp>
#include <general\vcl_functions.h>
#include <general\path.h>
#include <general\inifile.h>
#include <dcfdes.hpp>
#include <qrctrls.hpp>
#include <gtQrRtns.hpp>
#include "TSegTable.h"
#include "TGraph.h"
#include "TWizardForm.h"
#include "TXYGraph.h"

#pragma package(smart_init)
#pragma link "dcfdes"
#pragma link "gtQRXport"
#pragma link "gtQRXport_Doc"
#pragma link "gtQRXport_HTML"
#pragma link "gtQRXport_JPEG"
#pragma link "kbmMemTable"
#pragma link "jpeg"
using namespace std;
extern HINSTANCE instanceHandle;
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
Report::Report(TWinControl* p)
   : parent(p)
   {
   dataForm = NULL;
   reportForm = NULL;
   currentPage = NULL;
   uiForm = NULL;
   OnObjectInspectorUpdate = NULL;

   clear();
   isEditing = false;
   isDirty = false;
   zoomToFit = false;

   buttonImages = new TImageList(parent);
   buttonImages->Width = 24;
   buttonImages->Height = 24;
   buttonImages->Masked = true;

   RegisterComponents();
   }
//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
Report::~Report(void)
   {
   if (isEditing)
      edit(false);
   delete buttonImages;
   delete dataForm;
   delete reportForm;
   pages.erase(pages.begin(), pages.end());
   }
//---------------------------------------------------------------------------
// Clear the report and create a new one.
//---------------------------------------------------------------------------
void Report::clear(void)
   {
   delete reportForm;
   reportForm = new TForm((TComponent*)NULL);
   reportForm->Parent = parent;
   reportForm->Name = "report";
   reportForm->Align = alClient;
   reportForm->BorderIcons = TBorderIcons();
   reportForm->BorderStyle = bsNone;
   reportForm->Caption = "";
   reportForm->Visible = true;
   reportForm->OnResize = onResize;
   scrollBox = new TScrollBox(reportForm);
   scrollBox->Parent = reportForm;
   scrollBox->Align = alClient;

   dataForm = new TForm(reportForm);
   dataForm->Name = "data";
   dataForm->Align = alClient;
   dataForm->BorderIcons = TBorderIcons();
   dataForm->BorderStyle = bsNone;
   dataForm->Caption = "";
   dataForm->Visible = false;


   currentPage = NULL;
   pages.erase(pages.begin(), pages.end());
   isDirty = false;
   }
//---------------------------------------------------------------------------
// If the filename exists then load it into the report.
//---------------------------------------------------------------------------
void Report::load(const string& fileName, bool quiet)
   {
   if (FileExists(fileName.c_str()))
      {
      clear();

      SetCurrentDir(ExtractFileDir(fileName.c_str()));
      edit(false);

      ifstream in(fileName.c_str());
      string versionLine;
      getline(in, versionLine);
      if (getKeyValue(versionLine, "version") == "")
         {
         in.seekg(0);
         readOldVersion(0, in);
         }
      else if (getKeyValue(versionLine, "version") == "1.0")
         readOldVersion(1, in);

      else
         {
         if (getKeyValue(versionLine, "version") == "2.0")
            {
            in.close();
            convertVersion2To3(fileName);
            in.open(fileName.c_str());
            getline(in, versionLine);
            }
         string numPagesLine;
         getline(in, numPagesLine);
         unsigned numPages = StrToInt(getKeyValue(numPagesLine, "NumPages").c_str());

         vector<TComponent*> components;
         components.push_back(dataForm);
         for (unsigned p = 0; p != numPages; p++)
            {
            TQuickRep* newPage = new TQuickRep(reportForm);
            newPage->Visible = false;
            newPage->Parent = scrollBox;
            pages.push_back(newPage);
            components.push_back(newPage);
            }
         loadComponents(in, components);
         }
      showPage(0);
      refresh(quiet);
      isDirty = false;
      }
   }
//---------------------------------------------------------------------------
// Save the report to the specified filename.
//---------------------------------------------------------------------------
void Report::save(const std::string& fileName)
   {
   if (ExtractFileExt(fileName.c_str()) == ".report")
      {
      dataForm->Visible = false;
      setReportDirectory(ExtractFileDir(fileName.c_str()).c_str());

      ofstream out(fileName.c_str(), ios::binary);
      if (out.is_open())
         {
         out << "Version = 3.0\r\n";
         out << "NumPages = " << pages.size() << "\r\n";
         saveComponent(out, dataForm);
         for (unsigned p = 0; p != pages.size(); p++)
            saveComponent(out, pages[p]);

         out.close();
         isDirty = false;
         }
      else
         MessageBox(NULL, "Cannot save file. Is file readonly?", "Error", MB_ICONSTOP | MB_OK);
      }
   else
      exportCurrentToFile(fileName);
   }
//---------------------------------------------------------------------------
// Edit the current page.
//---------------------------------------------------------------------------
TForm* Report::edit(bool turnOn)
   {
   static TDCLiteDesigner* formDesigner = NULL;

   if (turnOn && currentPage != NULL)
      {
      bool dataPage = !currentPage->Visible;

      formDesigner = new TDCLiteDesigner(NULL);
      formDesigner->OnSelectionChanged = onDesignerSelectionChanged;
      if (dataPage)
         {
         formDesigner->DesignedComponent = dataForm;
         formDesigner->LimitControl = dataForm;
         }
      else
         {
         formDesigner->DesignedComponent = scrollBox;
         formDesigner->LimitControl = scrollBox;
         }

      formDesigner->ShowComponents = dataPage;
      formDesigner->ShowPalette = true;
      formDesigner->Active = true;
      GetPalForm()->AutoSize = false;
      GetPalForm()->Width = 400;
      GetPalForm()->Height = 80;
      GetPalForm()->Palette->RefreshData();
      if (dataPage)
         GetPalForm()->Palette->DeletePage(0);
      else
         GetPalForm()->Palette->DeletePage(1);
      GetPalForm()->Palette->SetNewTabIndex(0);

      return dynamic_cast<TForm*> (GetPalForm());
      }
   else if (!turnOn && formDesigner != NULL)
      {
      isDirty = formDesigner->Designer->WasChanged;
      delete formDesigner;
      formDesigner = NULL;
      }
   return NULL;
   }
//---------------------------------------------------------------------------
// Set the object inspector form to use.
//---------------------------------------------------------------------------
void Report::setObjectInspectorForm(TForm* objectInspector, TDataSource* dataInspector)
   {
   objectInspectorForm = objectInspector;
   dataInspectorSource = dataInspector;
   }
//---------------------------------------------------------------------------
// The designer has changed selection - get current object and call
// selectionChangedEvent.
//---------------------------------------------------------------------------
void __fastcall Report::onDesignerSelectionChanged(TObject* sender)
   {
   TDCLiteDesigner* formDesigner = dynamic_cast<TDCLiteDesigner*>(sender);

   // get the currently selected component.
   if (formDesigner->Designer->SelectedComponents->Count == 1)
      {
      TComponent* selectedComponent
         = (TComponent*) formDesigner->Designer->SelectedComponents->Items[0];
      updateObjectInspector(selectedComponent);
      }
   else
      updateObjectInspector(NULL);
   }
//---------------------------------------------------------------------------
// update the objectInspector for the specified component.
//---------------------------------------------------------------------------
void Report::updateObjectInspector(TComponent* component)
   {
   if (objectInspectorForm != NULL)
      {
      if (OnObjectInspectorUpdate != NULL)
         OnObjectInspectorUpdate(objectInspectorForm);

      objectInspectorForm->Visible = true;

      delete uiForm;
      uiForm = NULL;

      if (component != NULL)
         {
         // get a property form.
         uiForm = createComponentUI(component, objectInspectorForm, true);

         // If an addin returned a form then make that form a child of the parent
         // form.
         if (uiForm != NULL)
            {
            uiForm->BorderStyle = bsNone;
            uiForm->Parent = objectInspectorForm;
            uiForm->Align = alClient;
            uiForm->Show();
            }
         isDirty = true;
         }
      dataInspectorSource->DataSet = dynamic_cast<TDataSet*>(component);
      }
   }
//---------------------------------------------------------------------------
// Create a page on the report.
//---------------------------------------------------------------------------
AnsiString Report::createPage(void)
   {
   unsigned pageNumber = 1;
   while (getComponent<TQuickRep>(reportForm, "Page" + IntToStr(pageNumber)) != NULL)
      pageNumber++;

   AnsiString pageName = "Page" + IntToStr(pageNumber);
   if (pages.size() != 0)
      isDirty = true;

   TQuickRep* newPage = new TQuickRep(reportForm);
   newPage->Parent = scrollBox;
   newPage->Name = pageName;
   newPage->Units = MM;
   newPage->Page->Ruler = false;
   newPage->Frame->Style = psClear;
   newPage->Page->TopMargin = 0;
   newPage->Page->LeftMargin = 0;
   newPage->Page->RightMargin = 0;
   newPage->Page->BottomMargin = 0;
   newPage->Enabled = false;
   newPage->Zoom = 50;

   newPage->CreateBand(rbTitle);

   pages.push_back(newPage);
   showPage(pages.size()-1);
   return pageName;
   }
//---------------------------------------------------------------------------
// Get a page name for a particular page.
//---------------------------------------------------------------------------
AnsiString getPageName(TQuickRep* page)
   {
   return page->Name;
   }
//---------------------------------------------------------------------------
// Return a list of page names.
//---------------------------------------------------------------------------
void Report::getPageNames(TStrings* pageNames)
   {
   pageNames->Clear();
   for (unsigned p = 0; p != pages.size(); p++)
      pageNames->Add(getPageName(pages[p]));
   }
//---------------------------------------------------------------------------
// Show a page on the parent form.
//---------------------------------------------------------------------------
void Report::showPage(unsigned pageNumber)
   {
   if (pageNumber < pages.size())
      {
      if (currentPage != NULL)
         {
         currentPage->Visible = false;
         currentPage->Parent = NULL;
         }
      currentPage = pages[pageNumber];
      currentPage->Visible = true;
      currentPage->Parent = scrollBox;
      centrePage();
      }
   }
//---------------------------------------------------------------------------
// Show the data page.
//---------------------------------------------------------------------------
void Report::showDataPage(bool showData)
   {
   if (showData)
      {
      currentPage->Visible = false;
      currentPage->Parent = NULL;
      dataForm->Visible = true;
      dataForm->Parent = parent;
      }
   else
      {
      dataForm->Visible = false;
      dataForm->Parent = NULL;
      currentPage->Parent = scrollBox;
      currentPage->Visible = true;
      }
//   if (isEditing)
//      {
//      edit(false);
//      edit(true);
//      }
   }
//---------------------------------------------------------------------------
// Rename a page.
//---------------------------------------------------------------------------
void Report::renamePage(AnsiString oldName, AnsiString newName)
   {
   for (unsigned p = 0; p != pages.size(); p++)
      {
      if (oldName.AnsiCompareIC(getPageName(pages[p])) == 0)
         pages[p]->Name = newName;
      isDirty = true;
      }
   }
//---------------------------------------------------------------------------
// Delete a page.
//---------------------------------------------------------------------------
void Report::deletePage(unsigned pageIndex)
   {
   if (pageIndex < pages.size())
      {
      delete pages[pageIndex];
      pages.erase(pages.begin()+pageIndex);
      currentPage = NULL;
      showPage(0);
      isDirty = true;
      }
   }
//---------------------------------------------------------------------------
// Move the specified page (oldPageIndex) to before insertBeforePageIndex
//---------------------------------------------------------------------------
void Report::movePage(unsigned oldPageIndex, unsigned insertBeforePageIndex)
   {
   TQuickRep* pageToMove = pages[oldPageIndex];
   if (oldPageIndex < insertBeforePageIndex)
      {
      // move down.
      insertBeforePageIndex--;
      }
   pages.erase(pages.begin() + oldPageIndex);
   pages.insert(pages.begin() + insertBeforePageIndex, pageToMove);
   isDirty = true;
   }

//---------------------------------------------------------------------------
// Centre the current page within the parent container
//---------------------------------------------------------------------------
void Report::centrePage(void)
   {
   if (currentPage != NULL)
      {
      if (zoomToFit)
         {
         scrollBox->AutoScroll = false;
         currentPage->Zoom *= 1.0 * parent->Width / currentPage->Width;
         }

      int left = (scrollBox->Width - currentPage->Width) / 2;
      int top = max((scrollBox->Height - currentPage->Height) / 2, 0);
      currentPage->Left = max(left, 0);
      currentPage->Top  = max(top, 0);

      // position the title band.

      TQRBand* titleBand = getControlOfType<TQRBand>(currentPage);
      titleBand->Size->Height = currentPage->Page->Length
                              - currentPage->Page->TopMargin
                              - currentPage->Page->BottomMargin;
      titleBand->Size->Width = currentPage->Page->Width
                             - currentPage->Page->LeftMargin
                             - currentPage->Page->RightMargin;
      }
   }
//---------------------------------------------------------------------------
// The report has been resized - reposition ourselves in the middle
// of the form.
//---------------------------------------------------------------------------
void __fastcall Report::onResize(TObject* sender)
   {
   centrePage();
   }

//---------------------------------------------------------------------------
// Call the setReportDirectory method in all SEGTable components.
//---------------------------------------------------------------------------
void Report::setReportDirectory(const std::string& reportDirectory)
   {
   for (int c = 0; c < dataForm->ComponentCount; c++)
      {
      TSEGTable* table = dynamic_cast<TSEGTable*> (dataForm->Components[c]);
      if (table != NULL)
         table->setReportDirectory(reportDirectory.c_str());
      }
   }
//---------------------------------------------------------------------------
// return zoom
//---------------------------------------------------------------------------
int Report::getZoom(void)
   {
   if (currentPage != NULL)
      return currentPage->Zoom;
   return 50;
   }
//---------------------------------------------------------------------------
// Set the zoom level
//---------------------------------------------------------------------------
void Report::setZoom(int zoom)
   {
   if (currentPage != NULL && currentPage->Zoom != zoom)
      {
      currentPage->Zoom = zoom;
      centrePage();
      isDirty = true;
      }
   }
//---------------------------------------------------------------------------
// return true if the report is in portrait mode.
//---------------------------------------------------------------------------
bool Report::getIsPortrait(void)
   {
   if (currentPage != NULL)
      return (currentPage->Page->Orientation == poPortrait);
   return true;
   }

//---------------------------------------------------------------------------
// set the reports orientation.
//---------------------------------------------------------------------------
void Report::setIsPortrait(bool isPortrait)
   {
   if (currentPage != NULL)
      {
      if (isPortrait)
         currentPage->Page->Orientation = poPortrait;
      else
         currentPage->Page->Orientation = poLandscape;
      centrePage();
      isDirty = true;
      }
   }
//---------------------------------------------------------------------------
// Copy the report to the clipboard.
//---------------------------------------------------------------------------
void Report::copyToClipboard(void)
   {
   // save a bmp version of current page to clipboard.
   string tmpFile = Path::getTempFolder().Get_path();
   tmpFile += "\\clipboard.bmp";
   exportCurrentToFile(tmpFile);
   Graphics::TBitmap* bitmap = new Graphics::TBitmap;
   bitmap->LoadFromFile(tmpFile.c_str());
   unsigned short format;
   unsigned int dataHandle;
   HPALETTE palette;
   bitmap->SaveToClipboardFormat(format, dataHandle, palette);
   Clipboard()->SetAsHandle(format, dataHandle);
   delete bitmap;
   DeleteFile(tmpFile.c_str());

   // save an emf version of current page to clipboard.
   tmpFile = Path::getTempFolder().Get_path();
   tmpFile += "\\clipboard.emf";
   exportCurrentToFile(tmpFile);
   Graphics::TMetafile* metafile = new Graphics::TMetafile;
   metafile->LoadFromFile(tmpFile.c_str());
   metafile->SaveToClipboardFormat(format, dataHandle, palette);
   Clipboard()->SetAsHandle(format, dataHandle);
   delete metafile;
   DeleteFile(tmpFile.c_str());
   }
//---------------------------------------------------------------------------
// Print the report.
//---------------------------------------------------------------------------
void Report::print(bool currentPageOnly)
   {
   if (currentPage != NULL)
      {
      currentPage->PrinterSetup();
      if (currentPageOnly)
         currentPage->Print();

      else
         {
         for (unsigned p = 0; p != pages.size(); p++)
            pages[p]->Print();
         }
      }
   }
//---------------------------------------------------------------------------
// Refresh the report
//---------------------------------------------------------------------------
void Report::refresh(bool quiet)
   {
   TSEGTable::errorMessage = "";

   // loop through all dataset components and refresh them.  This will then
   // cause all data aware components to refresh.
   for (int componentI = 0; componentI < dataForm->ComponentCount; componentI++)
      {
      TSEGTable* table = dynamic_cast<TSEGTable*> (dataForm->Components[componentI]);
      if (table != NULL)
         table->refresh();
      }

   if (!quiet && TSEGTable::errorMessage != "")
      {
      ::MessageBox(NULL, TSEGTable::errorMessage.c_str(), "Errors were encountered", MB_ICONSTOP | MB_OK);
      TSEGTable::errorMessage = "";
      }
   }
//---------------------------------------------------------------------------
// Refresh all components linked to datasets but not the datasets themselves.
//---------------------------------------------------------------------------
void Report::refreshLinkedComponents(void)
   {
   TSEGTable::errorMessage = "";

   // loop through all dataset components and refresh them.  This will then
   // cause all data aware components to refresh.
   for (int componentI = 0; componentI < dataForm->ComponentCount; componentI++)
      {
      TSEGTable* table = dynamic_cast<TSEGTable*> (dataForm->Components[componentI]);
      if (table != NULL)
         table->refreshLinkedComponents();
      }
   }

//---------------------------------------------------------------------------
// Export the current page to the specified file.
//---------------------------------------------------------------------------
void Report::exportCurrentToFile(const std::string& fileName)
   {
   if (currentPage != NULL)
      {
      setZoom(100);            
      try
         {
         if (ExtractFileExt(fileName.c_str()) == ".bmp")
            ExportToBMP(currentPage, fileName.c_str(), false, true);
         else if (ExtractFileExt(fileName.c_str()) == ".jpg")
            {
            gtQRJPEGSettings->PixelFormat = pf8bit;
            ExportToJPEG(currentPage, fileName.c_str(), false, false);
            }
         else if (ExtractFileExt(fileName.c_str()) == ".html")
            {
            gtQRHTMLSettings->ExportImageFormat = ifGIF;
            ExportToHTML(currentPage, fileName.c_str(), false, false);
            }
         else if (ExtractFileExt(fileName.c_str()) == ".pdf")
            ExportToPDF(currentPage, fileName.c_str(), false, false);
         else if (ExtractFileExt(fileName.c_str()) == ".gif")
            {
            gtQRGIFSettings->PixelFormat = pf8bit;
            ExportToGIF(currentPage, fileName.c_str(), false, true);
            }
         else if (ExtractFileExt(fileName.c_str()) == ".rtf")
            {
            gtQRRTFSettings->ExportImageFormat = ifBMP;
            ExportToRTF(currentPage, fileName.c_str(), false, false);
            }
         else if (ExtractFileExt(fileName.c_str()) == ".wmf")
            ExportToWMF(currentPage, fileName.c_str(), false, false);
         else if (ExtractFileExt(fileName.c_str()) == ".emf")
            ExportToEMF(currentPage, fileName.c_str(), false, false);

         AnsiString tempFilePath = ExtractFileDir(fileName.c_str());
         if (tempFilePath != "")
            tempFilePath += "\\";
         AnsiString tempFile =  tempFilePath
                             + Path(fileName).Get_name_without_ext().c_str()
                             + "0001"
                             + ExtractFileExt(fileName.c_str());
         CopyFile(tempFile.c_str(), fileName.c_str(), false);
         DeleteFile(tempFile);

         // html exports do things this way!!!!!???
         tempFilePath = ExtractFileDir(fileName.c_str());
         if (tempFilePath != "")
            tempFilePath += "\\";
         tempFile =  tempFilePath
                             + Path(fileName).Get_name_without_ext().c_str()
                             + ".0001.htm";
         CopyFile(tempFile.c_str(), fileName.c_str(), false);
         DeleteFile(tempFile);
         }
      catch (const Exception& err)
         {
         }
      }
   }
//---------------------------------------------------------------------------
// Methods for filling a tool bar with buttons.
//---------------------------------------------------------------------------
void Report::populateToolBar(TToolBar* toolbar)
   {
   toolbar->Images = buttonImages;
   while (toolbar->ButtonCount > 0)
      delete toolbar->Buttons[0];
   buttonImages->Clear();
   toolbar->AutoSize = false;

   // Loop through all data components and add to tool bar IF the
   // data component indicates it is ok.
   for (int c = 0; c < dataForm->ComponentCount; c++)
      {
      TSEGTable* table = dynamic_cast<TSEGTable*>(dataForm->Components[c]);
      if (table != NULL && table->addToToolBar)
         addButtonToToolBar(table, toolbar);
      }
   // Loop through current page and add all chart components
   if (currentPage != NULL)
      {
      TQRBand* titleBand = getControlOfType<TQRBand>(currentPage);
      for (int c = 0; c < titleBand->ControlCount; c++)
         {
         TGraph* graph = dynamic_cast<TGraph*>(titleBand->Controls[c]);
         if (graph != NULL)
            addButtonToToolBar(graph, toolbar);
         }
      }
   toolbar->AutoSize = true;
   }
//---------------------------------------------------------------------------
// Add a button to the specified toolbar.
//---------------------------------------------------------------------------
void Report::addButtonToToolBar(TComponent* component, TToolBar* toolbar)
   {
   // Add bitmap to imagelist.
   Graphics::TBitmap* bitmap = new Graphics::TBitmap;
   getComponentBitmap(component->ClassName(), bitmap);
   if (!bitmap->Empty)
      {
      buttonImages->AddMasked(bitmap, bitmap->TransparentColor);
      delete bitmap;

      // add action to actionlist
      AnsiString name = component->Name;

      int buttonNum = buttonImages->Count;
      TToolButton *button = new TToolButton(toolbar);
      button->Parent = toolbar;
      button->Caption = name;
      button->Style = tbsButton;
      button->Hint = name;
      button->Tag = (int)component;
      button->ImageIndex = buttonImages->Count-1;
      button->OnClick = buttonClick;

      // make sure the button is at the far right of the toolbar.
      button->Left = (buttonNum-1) * toolbar->ButtonWidth + 2;
      toolbar->Width = button->Left + toolbar->ButtonWidth + 2;
      }
   }
//---------------------------------------------------------------------------
// loop through all add-in's looking for a resource for the specified
// component name.  Return true if found.
//---------------------------------------------------------------------------
bool Report::getComponentBitmap(AnsiString className, Graphics::TBitmap* bitmap)
   {
   if (className.LowerCase().Pos("series") == 0)
      {
      // try loading the resource with same name as class.
      AnsiString resourceName = UpperCase(className);
      try
         {
         bitmap->LoadFromResourceName((unsigned)instanceHandle, resourceName);
         return true;
         }
      catch (const Exception& error)
         {}
      }
   return false;
   }
//---------------------------------------------------------------------------
// User has clicked a button - go display object inspector.
//---------------------------------------------------------------------------
void __fastcall Report::buttonClick(TObject* sender)
   {
   TToolButton* button = dynamic_cast<TToolButton*>(sender);
   updateObjectInspector((TComponent*)button->Tag);
   }
//---------------------------------------------------------------------------
// Set a property.
//---------------------------------------------------------------------------
void Report::setProperty(const std::string& componentName,
                         const std::string& propertyName,
                         const std::string& propertyValue)
   {
   // get the first file reader and pass the output file to it.
   TComponent* component = getComponent<TComponent> (dataForm, componentName.c_str());
   TSEGTable* data = dynamic_cast<TSEGTable*>(component);
   if (data != NULL)
      data->setProperty(propertyName, propertyValue);
   }
//---------------------------------------------------------------------------
// Return a component to caller.
//---------------------------------------------------------------------------
TComponent* Report::getAComponent(const std::string& componentName)
   {
   TComponent* component = getComponent<TComponent> (dataForm, componentName.c_str());
   if (component == NULL)
      component = getComponent<TComponent> (reportForm, componentName.c_str());
   return component;
   }





//---------------------------------------------------------------------------
// Check for and read in the old version of the component.
//---------------------------------------------------------------------------
void Report::readOldVersion(int versionNumber, std::istream& in)
   {
   RegisterClass(__classid(TPageControl));
   RegisterClass(__classid(TTabSheet));
   RegisterClass(__classid(TScrollBox));

   // remove all instances of TSEGChart & TRichText.
   ostringstream contentsBuffer;
   contentsBuffer << in.rdbuf();
   string contents = contentsBuffer.str();
   replaceAll(contents, "TQRImage", "TImage");
   replaceAll(contents, "TSEGChart", "TGraph");
   replaceAll(contents, "TRichText", "TText");
   istringstream newIn(contents.c_str());

   TForm* formReadIn = new TForm((TComponent*)NULL);
   loadComponent(newIn, formReadIn);
   for (int c = 0; c != formReadIn->ComponentCount; c++)
      {
      TSEGTable* table = dynamic_cast<TSEGTable*>(formReadIn->Components[c]);
      TQuickRep* page = dynamic_cast<TQuickRep*>(formReadIn->Components[c]);
      TChartSeries* series = dynamic_cast<TChartSeries*>(formReadIn->Components[c]);
      if (table != NULL)
         {
         moveComponentTree(formReadIn->Components[c], dataForm);
         c = -1; // start counter back at beginning.
         }
      else if (series != NULL)
         {
         moveComponentTree(formReadIn->Components[c], reportForm);
         c = -1; // start counter back at beginning.
         }
      else if (page != NULL)
         {
         moveComponentTree(formReadIn->Components[c], reportForm);
         page->Visible = false;
         page->Parent = scrollBox;
         pages.push_back(page);
         c = -1; // start counter back at beginning.
         }
      }
   delete formReadIn;
   }
//---------------------------------------------------------------------------
// Version 2 to 3
//---------------------------------------------------------------------------
void Report::convertVersion2To3(const std::string& fileName)
   {
   ifstream in(fileName.c_str());
   ostringstream contentsStream;
   contentsStream << in.rdbuf();
   string contents = contentsStream.str();
   replaceAll(contents, "SeriesTitle = ", "SeriesTitle1 = ");
   in.close();

   ofstream out(fileName.c_str());
   out << contents;
   }
//---------------------------------------------------------------------------
// Move the specified component to the specified owner.  Uses recursion.
//---------------------------------------------------------------------------
void Report::moveComponentTree(TComponent* component, TComponent* owner)
   {
   if (component != NULL && component->Owner != NULL)
      {
      component->Owner->RemoveComponent(component);
      owner->InsertComponent(component);
      TWinControl* control = dynamic_cast<TWinControl*>(component);
      if (control != NULL)
         {
         for (int c = 0; c != control->ControlCount; c++)
            moveComponentTree(control->Controls[c], owner);
         }
      }
   }
//---------------------------------------------------------------------------
// Show the report wizard
//---------------------------------------------------------------------------
void Report::showWizard()
   {
   TForm* parents[2];
   parents[0] = dataForm;
   parents[1] = reportForm;

   TWizardForm* wizardForm = new TWizardForm(parent);

   bool someUIsFound = false;
   for (int formI = 0; formI != 2; formI++)
      {
      for (int componentI = 0; componentI < parents[formI]->ComponentCount; componentI++)
         {
         TComponent* component = parents[formI]->Components[componentI];
         if (doShowComponentInWizard(component))
            {
            // get a property form.
            TForm* uiForm = createComponentUI(component, wizardForm, false);

            // If an addin returned a form then make that form a child of the parent
            // form.
            if (uiForm != NULL)
               {
               uiForm->BorderStyle = bsNone;
               wizardForm->addComponentForm(uiForm);
               someUIsFound = true;
               }
            }
         }
      }
   if (someUIsFound)
      {
      wizardForm->ShowModal();
      delete wizardForm;
      }
   }
//---------------------------------------------------------------------------
// Return true if the specified component show be include in the wizard.
//---------------------------------------------------------------------------
bool Report::doShowComponentInWizard(TComponent* component)
   {
   TSEGTable* table = dynamic_cast<TSEGTable*> (component);
   if (table != NULL)
      return table->addToWizard;

   TXYGraph* graph = dynamic_cast<TXYGraph*> (component);
   if (graph != NULL)
      return true;

   return false;
   }

