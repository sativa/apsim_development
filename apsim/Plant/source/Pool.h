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
      void Init (float Plants);
      float DM;
      float N;
      float P;

      Pool operator + (const Pool& Pool2);
      Pool operator + (const Delta& Dlt);
      Pool operator * (float Fraction);
      Pool operator = (const Pool& Pool2);
      Pool operator - (const Delta& Dlt);
   private:
      std::string PartName;
      std::string Name;
      ScienceAPI* scienceAPI;
   };

#endif