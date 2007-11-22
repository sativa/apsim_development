#ifndef CompositePoolH
#define CompositePoolH
#include <string>
class ScienceAPI;

class CompositePool : public Pool
   {
   public:
      CompositePool(ScienceAPI& scienceAPI, const std::string& Name, const std::string& PartName);

      void AddPool(Biomass& Pool);
      virtual float DM() const;
      virtual float N()  const;
      virtual float P()  const;

      virtual Biomass& operator = (const Biomass& rhs);

   private:
      std::vector<Biomass*> Pools;

   };

#endif