#ifndef ComponentAliasH
#define ComponentAliasH
#include <ComponentInterface\Interfaces.h>
// ------------------------------------------------------------------
// Encapsulates a component.
// ------------------------------------------------------------------
class ComponentAlias
   {
   public:
      ComponentAlias(const std::string& name,
                     unsigned int ComponentAliasId);
      ComponentAlias(const std::string& name,
                     const std::string& dllFileName,
                     unsigned int ComponentAliasId,
                     unsigned int parentId);
      ~ComponentAlias(void);

      std::string getName(void) {return name;}
      std::string getExecutable(void) const
         {
         if (computation != NULL)
            return computation->getExecutable();
         else
            return "";
         }
      unsigned int ID;

   private:
      protocol::IComputation* computation;
      std::string name;
   };


#endif