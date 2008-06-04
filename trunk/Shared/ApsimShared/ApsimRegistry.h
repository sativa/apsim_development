//---------------------------------------------------------------------------
#ifndef ApsimRegistryH
#define ApsimRegistryH

#include <general/platform.h>
#include <vector>
#include <map>
#include <set>

#include <ApsimShared/ApsimRegistrationType.h>
#include <ApsimShared/ApsimRegistration.h>
#include <ApsimShared/ApsimRegistrationData.h>



// ------------------------------------------------------------------
//  Short description:
//     Encapsulates the registry for the simulation.  Its 
//     purpose is to map FQ names to IDs. 
// ------------------------------------------------------------------
class EXPORT ApsimRegistry 
   {
   public:
      ApsimRegistry(void) 
         {
         paddocks.item.ID = 0;
         paddocks.item.Name = ".MasterPM";
         paddocks.parent = &paddocks;
         };
      ~ApsimRegistry();

      unsigned int add(ApsimRegistration *);

      void erase(int owner, unsigned int regID);
      void erase(const std::string& owner, unsigned int regID)
         {
         int componentID = componentByName(owner);
         erase(componentID, regID);
         }

      // Find subscribers to a registration event
      void lookup(ApsimRegistration *, 
                  std::vector<ApsimRegistration*>&);

      // Find a single registration for a component,
      //  returns NULL if not found.
      ApsimRegistration *find(int ownerID, unsigned int regnID);
      ApsimRegistration *find(EventTypeCode type, int ownerID, const std::string &name);

      // Component routines
      // Add a component to the system
      void addComponent(int parentID, int componentID, 
                        const std::string &name);

      int componentByName(const std::string &);
      std::string componentByID(int);
  
      void getSiblings(int componentID, vector<int> &siblings);
      void getSiblingsAndDescendants(int componentID, vector<int> &siblings);
      void getSiblingsAndParents(int componentID, vector<int> &siblings);

      // Discriminate between "native" and "foreign" components
      void setForeignTaint(int);
      void clearForeignTaint(int);

      // How to get the singleton registry
      static ApsimRegistry& EXPORT getApsimRegistry(void); // singleton.
      
      // split open a qualified pathname (modulename.variablename) 
      void unCrackPath(int sourceID, const std::string &fqName, int &id, std::string &name);

      // Reset the whole registry
      void reset(void);
      
      std::string ApsimRegistry::getDescription(int componentID);

      void dumpStats(void);
      void dumpComponentTree(void);
      void dumpAll(void);
      
   private:
      typedef multimap<string, ApsimRegistration* , less<string> > registrations_type;
      registrations_type registrations;
      
      // associate component ID with string names
      typedef struct {
         int ID;
         std::string Name;
      } Component;
      vector<Component> components;

      // Tree class that holds the paddock structure
      template<class PItem> class PTree
         {
         public:
          PItem item;
          std::vector<PTree*> children;
          PTree *parent;
         };
      PTree<Component>  paddocks;
      PTree<Component> *findComponent(int componentID);
      PTree<Component> *findComponent(PTree<Component> *node, int componentID);
      void getDescendants(PTree<Component>*node, vector<int> &siblings);

      // Test whether a registration is in the scope of a component
      bool inScope(int , ApsimRegistration*);
      bool inScope(PTree<Component>*callerNode, ApsimRegistration *reg);
      
      // Whether a component is using our assigned reg IDs
      bool isForeign(int componentID);
      vector<int> taintedComponents;

      void lookup(ApsimRegistration *, 
                  std::vector<int> &, 
                  std::vector<ApsimRegistration*>&);

      void pruneDuplicates( std::vector<ApsimRegistration*>&subscribers);

      void dumpComponentTree(int indent, PTree<Component>* node);
      void dumpAll(PTree<Component>* node);
   };


#endif


