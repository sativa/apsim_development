//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TReportForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "TSEGReport"
#pragma link "AdvGrid"
#pragma link "BaseGrid"
#pragma link "DBAdvGrd"
#pragma resource "*.dfm"
TReportForm *ReportForm;
//---------------------------------------------------------------------------
__fastcall TReportForm::TReportForm(TComponent* Owner)
   : TForm(Owner)
   {
   onPageChanged = NULL;
   }
//---------------------------------------------------------------------------
void __fastcall TReportForm::TabControlChange(TObject *Sender)
   {
   Report->showPage(TabControl->TabIndex);
   if (onPageChanged != NULL)
      onPageChanged(NULL);
   }
//---------------------------------------------------------------------------
void __fastcall TReportForm::CreatePageClick(TObject *Sender)
   {
   unsigned pageIndex = TabControl->Tabs->Count;
   AnsiString newPageName = "Page" + IntToStr(TabControl->Tabs->Count+1);
   Report->createPage(newPageName);
   Report->showPage(pageIndex);
   TabControl->Tabs->Add(newPageName);
   TabControl->TabIndex = pageIndex;
   if (onPageChanged != NULL)
      onPageChanged(NULL);
   }
//---------------------------------------------------------------------------
void __fastcall TReportForm::DeletePageClick(TObject *Sender)
   {
   if (TabControl->Tabs->Count > 1 &&
       Application->MessageBox("Are you sure you want to delete this page",
                               "Question", MB_ICONQUESTION | MB_YESNO) == IDYES)
      {
      int tabToDelete = TabControl->TabIndex;
      Report->deletePage(tabToDelete);
      Report->showPage(0);
      TabControl->Tabs->Delete(tabToDelete);
      TabControl->TabIndex = 0;
      }
   if (onPageChanged != NULL)
      onPageChanged(NULL);
   }
//---------------------------------------------------------------------------
void __fastcall TReportForm::RenamePageClick(TObject *Sender)
   {
   AnsiString oldPageName = TabControl->Tabs->Strings[TabControl->TabIndex];
   AnsiString newPageName = oldPageName;
   if (InputQuery("Rename page", "Enter new page name", newPageName))
      {
      Report->renamePage(oldPageName, newPageName);
      TabControl->Tabs->Strings[TabControl->TabIndex] = newPageName;
      }
   }
//---------------------------------------------------------------------------
// given a page control and an x/y coordinate - returns the tab index.
//---------------------------------------------------------------------------
int PCDetectTab(TTabControl* pageControl, int x, int y)
   {
   TTCHitTestInfo hitInfo;
   hitInfo.pt.x = x;
   hitInfo.pt.y = y;
   hitInfo.flags = 0;
   return pageControl->Perform(TCM_HITTEST, 0, (int)&hitInfo);
   }
//---------------------------------------------------------------------------
// User has begun to drag something - if it is a tab then allow the drag
// to continue.
//---------------------------------------------------------------------------
void __fastcall TReportForm::TabControlMouseDown(TObject *Sender,
      TMouseButton Button, TShiftState Shift, int x, int y)
   {
   int tab = PCDetectTab(TabControl, x, y);
   if (tab >= 0)
      {
      draggedTab = tab;
      TabControl->BeginDrag(true);
      }
   }
//---------------------------------------------------------------------------
// User has begun to drag something - if it is a tab then allow the drag
// to continue.
//---------------------------------------------------------------------------
void __fastcall TReportForm::TabControlDragOver(TObject *sender,
      TObject *source, int x, int y, TDragState state, bool &accept)
   {
   int tab = PCDetectTab(TabControl, x, y);
   accept = (source == sender && tab >= 0 && tab != draggedTab);

   static int lastTab = -1;
   static int lastLeft;
   if (accept && tab != lastTab)
      {
      TRect r;
      SendMessage(TabControl->Handle, TCM_GETITEMRECT, tab, (int) &r);
      int aleft;
      if (tab > draggedTab)
         aleft = r.right - 3;
      else
         aleft = r.left + 3;
      TabControl->Canvas->Pen->Mode = pmNotXor;
      TabControl->Canvas->Pen->Width = 4;
      TabControl->Canvas->Pen->Color = clRed;
      if (lastTab >= 0)
         {
         TabControl->Canvas->MoveTo(lastLeft, r.top+2);
         TabControl->Canvas->LineTo(lastLeft, r.bottom-1);
         }
      lastLeft = aleft;
      TabControl->Canvas->MoveTo(aleft, r.top+2);
      TabControl->Canvas->LineTo(aleft, r.bottom-1);
      lastTab = tab;
      }
   }
//---------------------------------------------------------------------------
// User has dropped a tab.
//---------------------------------------------------------------------------
void __fastcall TReportForm::TabControlDragDrop(TObject *Sender,
      TObject *Source, int x, int y)
   {
   int tab = PCDetectTab(TabControl, x, y);
   if (tab >= 0)
      {
      Report->movePage(draggedTab, tab);
      Report->getPageNames(TabControl->Tabs);
      if (onPageChanged != NULL)
         onPageChanged(NULL);
      }
   }
//---------------------------------------------------------------------------

