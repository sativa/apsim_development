//---------------------------------------------------------------------------
#pragma hdrstop

#include "ContextHandler.h"
#include <fstream>
#include <general\path.h>
#include <general\stream_functions.h>
#pragma package(smart_init)

extern ULONG g_DllRefCount;

using namespace std;
extern string getApsimVersion(void);
extern string getApsimDirectory(void);

//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
CContextMenuHandler::CContextMenuHandler()
   {
   cRefs = 0;
   g_DllRefCount++;
   }
//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
CContextMenuHandler::~CContextMenuHandler()
   {
   g_DllRefCount--;
   }
//---------------------------------------------------------------------------
// The QueryInterface method
//---------------------------------------------------------------------------
STDMETHODIMP CContextMenuHandler::QueryInterface(REFIID riid, LPVOID *ppv)
   {
   *ppv = NULL;

   if(IsEqualIID(riid, IID_IUnknown))
      *ppv = this;
   else if(IsEqualIID(riid, IID_IContextMenu))
      *ppv = (IContextMenu*)this;
   else if(IsEqualIID(riid, IID_IShellExtInit))
      *ppv = (IShellExtInit*)this;

   if (*ppv == NULL)
      {
      return E_NOINTERFACE;
      }

   ((LPUNKNOWN)*ppv)->AddRef();

   return NOERROR;
   }
//---------------------------------------------------------------------------
// Increment our reference count.
//---------------------------------------------------------------------------
STDMETHODIMP_(DWORD) CContextMenuHandler::AddRef(void)
   {
   return ++cRefs;
   }
//---------------------------------------------------------------------------
// Decrement our reference count - delete ourself if count == 0
//---------------------------------------------------------------------------
STDMETHODIMP_(DWORD) CContextMenuHandler::Release(void)
   {
   if (--cRefs != 0)
      return cRefs;
   delete this;
   return 0;
   }
//---------------------------------------------------------------------------
// Create our context menu.
//---------------------------------------------------------------------------
STDMETHODIMP CContextMenuHandler::QueryContextMenu
   (HMENU hMenu, UINT indexMenu, UINT idCmdFirst, UINT idCmdLast, UINT uFlags)
   {
   if(((uFlags & 0x000F) == CMF_NORMAL) || (uFlags & CMF_EXPLORE))
      {
      string subMenuName;
      vector<MenuDescription> menuDescriptions;
      createMenus(subMenuName, menuDescriptions);
      if (menuDescriptions.size())
         {
         unsigned idCmd = idCmdFirst;
         HMENU subMenu = CreatePopupMenu();
         for (unsigned menu = 0; menu != menuDescriptions.size(); ++menu)
            {
            menus.insert(Menus::value_type(menu, menuDescriptions[menu]));
            InsertMenu(subMenu, 0xFFFFFFFF, MF_STRING | MF_BYPOSITION, idCmd++,
                       menuDescriptions[menu].name.c_str());
            }
         string subMenuName = "&Apsim" + getApsimVersion();
         InsertMenu(hMenu,
                    indexMenu++,
                    MF_STRING | MF_BYPOSITION | MF_POPUP,
                    (UINT)subMenu,
                    subMenuName.c_str());
         }

      return MAKE_HRESULT(SEVERITY_SUCCESS, 0, USHORT(menuDescriptions.size()));
      }
   return MAKE_HRESULT(SEVERITY_SUCCESS, 0, USHORT(0));
   }

//---------------------------------------------------------------------------
// User has clicked on a menu item - do something in response.
//---------------------------------------------------------------------------
STDMETHODIMP CContextMenuHandler::InvokeCommand(LPCMINVOKECOMMANDINFO lpici)
   {
   // BOOL fEx = FALSE;
   BOOL fUnicode = FALSE;

   if(lpici->cbSize == sizeof(CMINVOKECOMMANDINFOEX))
      {
      // fEx = TRUE;
      if(lpici->fMask & CMIC_MASK_UNICODE)
         fUnicode = TRUE;
      }

   // DPH - I'm not sure what 'listing' is here for.  Is it a
   // reserved verb?
   if(!fUnicode && HIWORD(lpici->lpVerb))
      {
      if(strcmpi(lpici->lpVerb, "listing"))
         return E_FAIL;
      }
   else if(fUnicode && HIWORD(((CMINVOKECOMMANDINFOEX*)lpici)->lpVerbW))
      {
      if(_wcsicmp(((CMINVOKECOMMANDINFOEX*)lpici)->lpVerbW, L"listing"))
         return E_FAIL;
      }
   else
      {
      // code here for executing actions when menus are clicked
      unsigned idCmd = (unsigned) lpici->lpVerb;
      menus[idCmd].handler();
      }

   return S_OK;
   }

//---------------------------------------------------------------------------
// Return help string for menu item.
//---------------------------------------------------------------------------
STDMETHODIMP CContextMenuHandler::GetCommandString
   (UINT idCmd, UINT uType, UINT *pwReserved, LPSTR pszName, UINT cchMax)
   {
   HRESULT  hr = E_INVALIDARG;
   wstring wideString;

   switch(uType)
      {
      case GCS_HELPTEXTA: lstrcpynA(pszName, menus[idCmd].description.c_str(), cchMax);
                          hr = S_OK;
                          break;
      case GCS_HELPTEXTW: wideString = wstring(menus[idCmd].description.begin(), menus[idCmd].description.end());
                          lstrcpynW((LPWSTR)pszName, wideString.c_str(), cchMax);
                          hr = S_OK;
                          break;
      case GCS_VERBA:     lstrcpynA(pszName, menus[idCmd].verb.c_str(), cchMax);
                          hr = S_OK;
                          break;
      case GCS_VERBW:     wideString = wstring(menus[idCmd].verb.begin(), menus[idCmd].verb.end());
                          lstrcpynW((LPWSTR)pszName, wideString.c_str(), cchMax);
                          hr = S_OK;
                          break;
      default:            hr = S_OK;
                          break;
      }
   return hr;
   }
//---------------------------------------------------------------------------
// User has clicked on a file(s) - Initialise ourselves and get
// the filenames.
//---------------------------------------------------------------------------
STDMETHODIMP CContextMenuHandler::Initialize
   (LPCITEMIDLIST pIDFolder, IDataObject *pData, HKEY hRegKey)
   {
   // If Initialize has already been called, release the old
   // IDataObject pointer.
   //if(p_DataObj)
   //   p_DataObj->Release();

   // If a data object pointer was passed in, save it and
   // extract the file name.
   if(pData)
      {
      p_DataObj = pData;
      //pData->AddRef();

      STGMEDIUM medium;
      FORMATETC fe = {CF_HDROP, NULL, DVASPECT_CONTENT, -1, TYMED_HGLOBAL};
      UINT uCount;

      if(SUCCEEDED(p_DataObj->GetData(&fe, &medium)))
         {
         // Get the file name from the CF_HDROP.
         uCount = DragQueryFile((HDROP)medium.hGlobal, (UINT)-1, NULL, 0);
         char fileName[MAX_PATH];
         for (unsigned i = 0; i != uCount; i++)
            {
            DragQueryFile((HDROP)medium.hGlobal, i, fileName, sizeof(fileName));
            fileNames.push_back(fileName);
            }
         ReleaseStgMedium(&medium);
         }
      }
   return S_OK;
   }
//---------------------------------------------------------------------------
// Return the name of the sub menu and a vector of menu descriptions.
//---------------------------------------------------------------------------
void CContextMenuHandler::createMenus(string& subMenuName,
                                      vector<MenuDescription>& menuDescriptions)
   {
   // get extension of file
   unsigned posExtension = fileNames[0].find(".");
   if (posExtension != string::npos)
      {
      string extension = fileNames[0].substr(posExtension,
                                             fileNames[0].length()-posExtension);
      if (strcmpi(extension.c_str(), ".out") == 0 ||
          strcmpi(extension.c_str(), ".con") == 0 ||
          strcmpi(extension.c_str(), ".apf") == 0 ||
          strcmpi(extension.c_str(), ".run") == 0 ||
          strcmpi(extension.c_str(), ".sim") == 0)
         {

         if (strcmpi(extension.c_str(), ".out") == 0)
            {
            menuDescriptions.push_back(
               MenuDescription("Send to &Excel",
                               "Send all data in the selected files to Microsoft EXCEL",
                               "excel",
                               &excel));
            menuDescriptions.push_back(
               MenuDescription("&Graph using ApsVis",
                               "Graph the selected files using ApsVis",
                               "apsvis",
                               &apsvis));
            menuDescriptions.push_back(
               MenuDescription("&Graph using ApsimOutlook",
                               "Graph the selected files using ApsimOutlook",
                               "apsimoutlook",
                               &apsimoutlook));
            }
         else if (strcmpi(extension.c_str(), ".con") == 0)
            {
            menuDescriptions.push_back(
               MenuDescription("&Run Apsim",
                               "Run Apsim",
                               "run",
                               &run));
            menuDescriptions.push_back(
               MenuDescription("&Convert to SIM",
                               "Convert the specified control file to a SIM file",
                               "sim",
                               &sim));
            }
         else if (strcmpi(extension.c_str(), ".apf") == 0)
            {
            menuDescriptions.push_back(
               MenuDescription("&Compile (make)",
                               "Compile the specified project file",
                               "make",
                               &make));
            menuDescriptions.push_back(
               MenuDescription("Compile &all (build)",
                               "Build the specified project file from scratch",
                               "build",
                               &build));
            }
         else if (strcmpi(extension.c_str(), ".run") == 0)
            {
            menuDescriptions.push_back(
               MenuDescription("&Run Apsim",
                               "Run Apsim",
                               "run",
                               &run));
            }
         else if (strcmpi(extension.c_str(), ".sim") == 0)
            {
            menuDescriptions.push_back(
               MenuDescription("&Run Apsim",
                               "Run Apsim",
                               "run",
                               &run));
            }
         }
      }
   }

//---------------------------------------------------------------------------
// Send all files to EXCEL.
//---------------------------------------------------------------------------
void __fastcall CContextMenuHandler::excel(void)
   {
   for (unsigned int file = 0; file != fileNames.size(); file++)
      {
      // open input stream
      ifstream in (fileNames[file].c_str());

      // output output stream.
      Path OutPath (fileNames[file]);
      OutPath.Set_extension (".csv");
      ofstream out (OutPath.Get_path().c_str());

      // copy first two lines as is.
      string Line;
      getline (in, Line);
      out << Line << endl;
      getline (in, Line);
      out << Line << endl;

      // convert file.
      Convert_2_CSV(in, out);

      // close files.
      in.close();
      out.close();

      // give output file to EXCEL.
      ShellExecute (NULL, "open", OutPath.Get_path().c_str(), NULL, "", SW_SHOW);
      }
   }
//---------------------------------------------------------------------------
// Send all files to APSVis.
//---------------------------------------------------------------------------
void __fastcall CContextMenuHandler::apsvis(void)
   {

   }
//---------------------------------------------------------------------------
// Send all files to Apsim Outlook.
//---------------------------------------------------------------------------
void __fastcall CContextMenuHandler::apsimoutlook(void)
   {

   }
//---------------------------------------------------------------------------
// Send all files to Apsim.
//---------------------------------------------------------------------------
void __fastcall CContextMenuHandler::run(void)
   {
   for (unsigned int file = 0; file != fileNames.size(); file++)
      {
      string command = "\"" + getApsimDirectory() + "\\bin\\apsrun\" \"" + fileNames[file] + "\"";

      // run command
      WinExec(command.c_str(), SW_SHOW);
      }
   }
//---------------------------------------------------------------------------
// Convert all files to SIM format.
//---------------------------------------------------------------------------
void __fastcall CContextMenuHandler::sim(void)
   {
   for (unsigned int file = 0; file != fileNames.size(); file++)
      {
      string command = "\"" + getApsimDirectory() + "\\bin\\apsrun\" /CreateSIM \"" + fileNames[file] + "\"";

      // run command
      WinExec(command.c_str(), SW_SHOW);
      }
   }
//---------------------------------------------------------------------------
// Compile all files
//---------------------------------------------------------------------------
void __fastcall CContextMenuHandler::make(void)
   {
   for (unsigned int file = 0; file != fileNames.size(); file++)
      {
      string command = "\"" + getApsimDirectory() + "\\bin\\apsbuild -make \"" + fileNames[file] + "\"";

      // run command
      WinExec(command.c_str(), SW_SHOW);
      }
   }
//---------------------------------------------------------------------------
// Build all files.
//---------------------------------------------------------------------------
void __fastcall CContextMenuHandler::build(void)
   {
   for (unsigned int file = 0; file != fileNames.size(); file++)
      {
      string command = "\"" + getApsimDirectory() + "\\bin\\apsbuild -build \"" + fileNames[file] + "\"";

      // run command
      WinExec(command.c_str(), SW_SHOW);
      }
   }

