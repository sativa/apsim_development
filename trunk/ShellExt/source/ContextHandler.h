//---------------------------------------------------------------------------

#ifndef ContextHandlerH
#define ContextHandlerH

#include <shlobj.h>
#include <vector>
#include <string>
#include <map>

typedef void __fastcall (__closure *EventHandler)(void);

struct MenuDescription
   {
   MenuDescription(const std::string menuName,
                   const std::string desc,
                   const std::string v,
                   EventHandler h)
      : name(menuName), description(desc), verb(v), handler(h) { }
   MenuDescription(void) { }

   std::string name;
   std::string description;
   std::string verb;
   EventHandler handler;
   };

//---------------------------------------------------------------------------
// Our Context Menu handler
//---------------------------------------------------------------------------
class CContextMenuHandler : public IContextMenu, IShellExtInit
   {
   private:
      DWORD cRefs;
      IDataObject *p_DataObj;

      std::vector<std::string> fileNames;
      typedef std::map<unsigned, MenuDescription> Menus;
      Menus menus;

      void createMenus(std::string& subMenuName,
                       std::vector<MenuDescription>& menuDescriptions);

      void __fastcall excel(void);
      void __fastcall apsvis(void);
      void __fastcall apsimoutlook(void);
      void __fastcall run(void);
      void __fastcall sim(void);
      void __fastcall make(void);
      void __fastcall build(void);

   public:
      CContextMenuHandler();
      ~CContextMenuHandler();

      STDMETHODIMP QueryInterface(REFIID riid, LPVOID *ppvObj);
      STDMETHODIMP_(DWORD) AddRef(void);
      STDMETHODIMP_(DWORD) Release(void);

      STDMETHODIMP QueryContextMenu(HMENU, UINT, UINT, UINT, UINT);
      STDMETHODIMP InvokeCommand(LPCMINVOKECOMMANDINFO);
      STDMETHODIMP GetCommandString(UINT, UINT, UINT*, LPSTR, UINT);

      STDMETHODIMP Initialize(LPCITEMIDLIST, LPDATAOBJECT, HKEY);
   };
typedef CContextMenuHandler* LPCONTEXTMENUHANDLER;

#endif

