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
      interpolationFunction DigestibilityMax;
      interpolationFunction DigestibilityAvg;
      interpolationFunction DigestibilityMin;
      float Pconc() {return divide(P,DM,0.0);};
      float Nconc() {return divide(N,DM,0.0);};
      float NconcPercent() {return divide(N,DM,0.0)*fract2pcnt;};
      float PconcPercent() {return divide(P,DM,0.0)*fract2pcnt;};

      Pool operator + (const Pool& Pool2);
      Pool operator + (const Pool Pool2);
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