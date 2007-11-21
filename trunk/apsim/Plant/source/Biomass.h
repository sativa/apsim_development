#ifndef BiomassH
#define BiomassH
class Biomass
   {
   public:
      Biomass();
      Biomass(float DM, float N, float P); 
      virtual void Clear();
      float DM() const {return privateDM;}
      float N()  const {return privateN;}
      float P()  const {return privateP;}

      float Pconc() {return divide(privateP,DM(),0.0);};
      float Nconc() {return divide(privateN,DM(),0.0);};
      float NconcPercent() {return divide(privateN,DM(),0.0)*fract2pcnt;};
      float PconcPercent() {return divide(privateP,DM(),0.0)*fract2pcnt;};

      Biomass operator + (const Biomass& rhs);
      Biomass operator - (const Biomass& rhs);
      Biomass operator * (float Fraction);
      Biomass operator = (const Biomass& rhs);

   protected:
      float privateDM;
      float privateN;
      float privateP;

   private:
      void CheckBounds();

   };

#endif