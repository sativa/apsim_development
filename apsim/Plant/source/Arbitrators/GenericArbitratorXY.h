#ifndef GENERICARBITRATORXY_H
#define GENERICARBITRATORXY_H

class genericArbitratorXY : public Arbitrator
   {
  private:
   vector <string> PartitionParts;
   vector <string> PartitionRules;

   interpolationFunction ratio_root_shoot;
   vector <interpolationFunction> Fracs;
  public:
   genericArbitratorXY(ScienceAPI& scienceAPI, plantInterface *p)
      : Arbitrator(scienceAPI, p) {};
   ~genericArbitratorXY(void) {};

   virtual void readSpeciesParameters (protocol::Component *, vector<string> &);
   virtual void partitionDM(float,vector <plantPart *>& Parts, string FruitName);
   virtual void zeroAllGlobals(void) ;
   virtual float dltDMWhole(float dlt_dm);
   };

#endif
