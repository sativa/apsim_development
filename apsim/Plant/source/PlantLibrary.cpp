#include <stdio.h>
#include <vector>
#include <string>
#include <list>

#include <string.h>
#include <math.h>

#include "PlantLibrary.h"

using namespace std;

/*   ================================================================
 *    Conversion constants
 *   ================================================================
 *
 *   Short description:
 *     Globally used conversion constants
 */
// WEIGHT conversion
const float gm2kg = 1.0/1000.0;           // constant to convert g to kg
const float kg2gm = 1000.0;               // conversion of kilograms to grams
const float mg2gm = 1.0/1000.0;           // conversion of mg to grams
const float t2g = 1000.0*1000.0;          // tonnes to grams
const float g2t = 1.0/ t2g;               // grams to tonnes
const float t2kg = 1000.0;                // tonnes to kilograms
const float kg2t = 1.0/ t2kg;             // kilograms to tonnes

// AREA conversion
const float ha2scm = 10000.0*10000.0;     // ha to sq cm
const float ha2sm = 10000.0;              // conversion of hectares to sq metres
const float sm2ha = 1.0/10000.0;          // constant to convert m^2 to hectares
const float sm2smm = 1000000.0;           // conversion of square metres to square mm
const float smm2sm = 1.0/1000000.0;       // conversion factor of mm^2 to m^2
const float scm2smm = 100.0;              // conversion factor of cm^2 to mm^2

// PRESSURE and related conversion
const float g2mm = 1.0e3/1.0e6;           // convert g water/m^2 to mm water
                                          // 1 g water = 1,000 cubic mm and
                                          // 1 sq m = 1,000,000 sq mm
const float mb2kpa = 100.0/1000.0;        // convert pressure mbar to kpa
                                          // 1000 mbar = 100 kpa

// LENGTH conversion
const float cm2mm = 10.0;                 // cm to mm
const float mm2cm = 1.0/10.0;             // conversion of mm to cm
const float mm2m = 1.0/1000.0;            // conversion of mm to m
const float km2m  = 1000.0;               // conversion of km to metres

// VOLUME conversion
const float cmm2cc = 1.0/1000.0;          // conversion of cubic mm to cubic cm
const float conv_gmsm2kgha = 100.0;       // convert g/sm -> kg/ha
const float conv_pc2fr = 0.01;            // convert %age to fraction
const float pcnt2fract = 1.0/100.0;       // convert percent to fraction
const float fract2pcnt = 100.0;           // convert fraction to percent
const float mm2lpsm = 1.0;                // mm depth to litres per sq m
const float lpsm2mm = 1.0;                // litres per sq m to mm depth
const float day2hr  = 24.0;               // days to hours
const float hr2s    = 60.0*60.0;          // hours to seconds
const float s2hr    = 1.0/hr2s;           // seconds to hours


void stageSubject::update() {
  for (std::list<observer*>::iterator o = observers.begin();
       o !=  observers.end();
       o++)
      (*o)->update();
};
void stageSubject::reset(){
   for (std::list<observer*>::iterator o = observers.begin();
        o !=  observers.end();
        o++)
       (*o)->reset();
};

stageSubject::stageSubject() {};


bool char2any(const char *str, int &value) {
   return (sscanf(str, "%d", &value) == 1);
}
bool char2any(const char *str, float &value) {
   return (sscanf(str, "%f", &value) == 1);
}
std::string any2string(float value) {
   return(ftoa(value, ".2"));
}
std::string any2string(int value) {
   return(itoa(value, 5));
}
