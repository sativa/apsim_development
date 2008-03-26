#include <stdio.h>
#include <math.h>
#include <stdexcept>
#include <string>
#include "PlantPart.h"

#include "Biomass.h"
#include "Delta.h"
using namespace std;
 
Biomass::Biomass()
   {
   Clear();
   }
Biomass::Biomass(float DM, float N, float P)
   {
   privateDM = DM;
   privateN = N;
   privateP = P;
   }


void Biomass::Clear (void)
   {
   privateDM = 0.0;
   privateN = 0.0;
   privateP = 0.0;
   }

Biomass Biomass::operator + (const Biomass& Biomass2)
   {
   Biomass Temp;
   Temp.privateDM = DM() + Biomass2.DM();
   Temp.privateN = N() + Biomass2.N();
   Temp.privateP = P() + Biomass2.P();
   return Temp;
   }

Biomass Biomass::operator - (const Biomass& Biomass2)
   {
   Biomass Temp;
   Temp.privateDM = DM() - Biomass2.DM();
   Temp.privateN = N() - Biomass2.N();
   Temp.privateP = P() - Biomass2.P();
   return Temp;
   }

Biomass Biomass::operator * (float Fraction)
   {
   Biomass Temp;
   Temp.privateDM = DM() * Fraction;
   Temp.privateN = N() * Fraction;
   Temp.privateP = P() * Fraction;
   return Temp;
   }

Biomass& Biomass::operator = (const Biomass& Biomass2)
   {
   privateDM = Biomass2.DM();
   privateN = Biomass2.N();
   privateP = Biomass2.P();
   CheckBounds();
   return *this;
   }

void Biomass::CheckBounds()
   {
   // Now check nothing is negative
   const float ctz = -0.00001;
   if (DM() < ctz)
      cerr << endl << "     *** " << "DM Biomass is negative! " << ftoa(DM(),6) << endl << endl;
   if (N() < ctz)
      cerr << endl << "     *** " << "N Biomass is negative! " << ftoa(N(),6) << endl<< endl;
   if (P() < ctz)
      cerr << endl << "     *** " << " P Biomass is negative! " << ftoa(P(),6) << endl<< endl;
   }

