#ifndef GENERICARBITRATOR_H
#define GENERICARBITRATOR_H

class genericArbitrator : public Arbitrator
   {
  private:
   float frac_leaf[max_table];                       // fraction of remaining dm allocated to leaves
   float ratio_root_shoot[max_table];                // root:shoot ratio of new dm ()
   vector <string> PartitionParts;
   vector <string> PartitionRules;
   vector <vector <float> > Fracs;
  public:
   genericArbitrator(ScienceAPI& scienceAPI, plantInterface *p)
      : Arbitrator(scienceAPI, p) {};
   ~genericArbitrator(void) {};

   virtual void readSpeciesParameters (protocol::Component *, vector<string> &);
   virtual void partitionDM(float,vector <plantPart *>& Parts);
   virtual void zeroAllGlobals(void) ;
   virtual float dltDMWhole(float dlt_dm);
   };

#endif
