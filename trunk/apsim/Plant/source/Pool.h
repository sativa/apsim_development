#ifndef PoolH
#define PoolH
#include <string>
class ScienceAPI;
class Delta;
class Pool
   {
   public:
      Pool ();
      Pool(ScienceAPI& scienceAPI, const std::string& Name, const std::string& PartName);
      void Clear (void);
      float DM;
      float N;
      float P;

      Pool operator + (const Pool& Pool2) const;
      Pool operator + (const Delta& Dlt) const;
      Pool operator * (float Fraction) const;
      Pool operator = (const Pool& Pool2);
      Pool operator - (const Delta& Dlt) const;
   private:

      std::string PartName;
      std::string Name;
   };

#endif