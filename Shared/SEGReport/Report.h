//---------------------------------------------------------------------------

#ifndef ReportH
#define ReportH
#include <string>
namespace Quickrpt {
   class TQuickRep;
   };
namespace Classes {
   class TComponent;
   };
//---------------------------------------------------------------------------
// Report class.
//---------------------------------------------------------------------------
class __declspec(dllexport) Report
   {
   public:
      Report(TWinControl* parent);
      ~Report(void);

      //---------------------------------------------------------------------------
      // Clear the report and create a new one.
      //---------------------------------------------------------------------------
      void clear(void);

      //---------------------------------------------------------------------------
      // If the filename exists then load it into the report.
      //---------------------------------------------------------------------------
      void load(const std::string& fileName, bool quiet);

      //---------------------------------------------------------------------------
      // Save the report to the specified filename.
      //---------------------------------------------------------------------------
      void save(const std::string& fileName);

      //---------------------------------------------------------------------------
      // Edit the report.  Returns a pointer to the palette.
      //---------------------------------------------------------------------------
      TForm* edit(bool turnOn);

      //---------------------------------------------------------------------------
      // Set the object inspector form to use.
      //---------------------------------------------------------------------------
      void setObjectInspectorForm(TForm* objectInspectorForm, TDataSource* dataInspectorSource);

      //---------------------------------------------------------------------------
      // Page methods.
      //---------------------------------------------------------------------------
      AnsiString createPage(void);
      void getPageNames(TStrings* pageNames);
      void showPage(unsigned pageIndex);
      void showDataPage(bool showData);
      void renamePage(AnsiString oldName, AnsiString newName);
      void deletePage(unsigned pageIndex);
      void movePage(unsigned oldPageIndex, unsigned insertBeforePageIndex);

      //---------------------------------------------------------------------------
      // Copy the report to the clipboard.
      //---------------------------------------------------------------------------
      void copyToClipboard(void);

      //---------------------------------------------------------------------------
      // zoom methods
      //---------------------------------------------------------------------------
      int getZoom(void);
      void setZoom(int zoom);

      //---------------------------------------------------------------------------
      // report orientation methods
      //---------------------------------------------------------------------------
      bool getIsPortrait(void);
      void setIsPortrait(bool isPortrait);

      //---------------------------------------------------------------------------
      // Print the report.
      //---------------------------------------------------------------------------
      void print(bool currentPageOnly);

      //---------------------------------------------------------------------------
      // Methods for filling a tool bar with buttons.
      //---------------------------------------------------------------------------
      void populateToolBar(TToolBar* toolbar);

      //---------------------------------------------------------------------------
      // Refresh the report
      //---------------------------------------------------------------------------
      void refresh(bool quiet);

      //---------------------------------------------------------------------------
      // Refresh all components linked to datasets but not the datasets themselves.
      //---------------------------------------------------------------------------
      void refreshLinkedComponents(void);

      //---------------------------------------------------------------------------
      // Return true if the report needs saving.
      //---------------------------------------------------------------------------
      bool needsSaving(void) {return isDirty;}

      //---------------------------------------------------------------------------
      // Set a property.
      //---------------------------------------------------------------------------
      void setProperty(const std::string& componentName,
                       const std::string& propertyName,
                       const std::string& propertyValue);

      //---------------------------------------------------------------------------
      // Return a component to caller.
      //---------------------------------------------------------------------------
      TComponent* getAComponent(const std::string& componentName);

      //---------------------------------------------------------------------------
      // Event that triggers when object inspector is updated.
      //---------------------------------------------------------------------------
      TNotifyEvent OnObjectInspectorUpdate;

      //---------------------------------------------------------------------------
      // Set zoom to fit.
      //---------------------------------------------------------------------------
      void setZoomToFit(bool fit) {zoomToFit = fit;}

      //---------------------------------------------------------------------------
      // Show the report wizard
      //---------------------------------------------------------------------------
      void showWizard();

   private:
      TWinControl* parent;
      std::vector<Quickrpt::TQuickRep*> pages;
      Quickrpt::TQuickRep* currentPage;
      bool isDirty;
      bool isEditing;
      bool zoomToFit;
      TForm* reportForm;
      TForm* dataForm;
      TForm* objectInspectorForm;
      TDataSource* dataInspectorSource;
      TForm* uiForm;
      TScrollBox* scrollBox;
      TImageList* buttonImages;

      //---------------------------------------------------------------------------
      // The designer has changed selection - get current object and call
      // selectionChangedEvent.
      //---------------------------------------------------------------------------
      void __fastcall onDesignerSelectionChanged(TObject* sender);

      //---------------------------------------------------------------------------
      // update the objectInspector for the specified component.
      //---------------------------------------------------------------------------
      void updateObjectInspector(TComponent* component);

      //---------------------------------------------------------------------------
      // Centre the current page within the parent container
      //---------------------------------------------------------------------------
      void centrePage(void);

      //---------------------------------------------------------------------------
      // The report has been resized - reposition ourselves in the middle
      // of the form.
      //---------------------------------------------------------------------------
      void __fastcall onResize(TObject* sender);

      //---------------------------------------------------------------------------
      // Call the setReportDirectory method in all SEGTable components.
      //---------------------------------------------------------------------------
      void setReportDirectory(const std::string& reportDirectory);

      //---------------------------------------------------------------------------
      // Export the current page to the specified file.
      //---------------------------------------------------------------------------
      void exportCurrentToFile(const std::string& fileName);


      //---------------------------------------------------------------------------
      // Add a button to the specified toolbar.
      //---------------------------------------------------------------------------
      void addButtonToToolBar(TComponent* component, TToolBar* toolbar);

      //---------------------------------------------------------------------------
      // loop through all add-in's looking for a resource for the specified
      // component name.  Return true if found.
      //---------------------------------------------------------------------------
      bool getComponentBitmap(AnsiString className, Graphics::TBitmap* bitmap);

      //---------------------------------------------------------------------------
      // User has clicked a button - go display object inspector.
      //---------------------------------------------------------------------------
      void __fastcall buttonClick(TObject* button);

      //---------------------------------------------------------------------------
      // Check for and read in the old version of the component.
      //---------------------------------------------------------------------------
      void readOldVersion(int versionNumber, std::istream& in);

      //---------------------------------------------------------------------------
      // Move the specified component to the specified owner.  Uses recursion.
      //---------------------------------------------------------------------------
      void moveComponentTree(TComponent* component, TComponent* owner);

      //---------------------------------------------------------------------------
      // Return true if the specified component show be include in the wizard.
      //---------------------------------------------------------------------------
      bool doShowComponentInWizard(TComponent* component);

      //---------------------------------------------------------------------------
      // Version 2 to 3
      //---------------------------------------------------------------------------
      void convertVersion2To3(const std::string& fileName);

   };
#endif
