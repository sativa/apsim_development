//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TestSoils.h"
#include <boost\filesystem\path.hpp>
#include <boost\filesystem\operations.hpp>
#include <general\string_functions.h>
#include <math.h>
#include "..\Soils.h"

#pragma package(smart_init)
using namespace std;
using namespace boost::unit_test_framework;
using namespace boost::filesystem;

const char* SOIL_DATA =
   "\n<Soil name=\"Test soil\">\n\n"
   "  <Region>Test region</Region>\n\n"
   "  <Cona>2</Cona>\n\n"
   "  <DiffusConst>220</DiffusConst>\n\n"
   "  <DiffusSlope>22</DiffusSlope>\n\n"
   "  <U>2</U>\n\n"
   "  <Salb>0.13</Salb>\n\n"
   "  <Cn2Bare>68</Cn2Bare>\n\n"
   "  <CnRed>20</CnRed>\n\n"
   "  <CnCov>0.8</CnCov>\n\n"
   "  <CnCanopyFact>1</CnCanopyFact>\n\n"
   "  <RootCn>45</RootCn>\n\n"
   "  <RootWt>1500</RootWt>\n\n"
   "  <SoilCn>12.5</SoilCn>\n\n"
   "  <EnrACoeff>7.4</EnrACoeff>\n\n"
   "  <EnrBCoeff>0.2</EnrBCoeff>\n"
   "\n"
   "  <layer name=\"1\">\n"
   "    <thickness>100</thickness>\n"
   "    <airdry>0.042</airdry>\n"
   "    <ll15>0.084</ll15>\n"
   "    <dul>0.296</dul>\n"
   "    <sat>0.353</sat>\n"
   "    <sw>0.295</sw>\n"
   "    <swcon>0.7</swcon>\n"
   "    <bd>1.716</bd>\n"
   "    <oc>1.05</oc>\n"
   "    <ec>0</ec>\n"
   "    <esp>5</esp>\n"
   "    <ph>8.5</ph>\n"
   "    <fbiom>0.025</fbiom>\n"
   "    <finert>0.4</finert>\n"
   "    <no3>5</no3>\n"
   "    <nh4>1.24</nh4>\n"
   "    <crop name=\"wheat\">\n"
   "      <ll>0.084</ll>\n"
   "      <kl>0.06</kl>\n"
   "      <xf>1</xf>\n"
   "      <kl0>0.08</kl0>\n"
   "    </crop>\n"
   "  </layer>\n"
   "\n"
   "  <layer name=\"2\">\n"
   "    <thickness>150</thickness>\n"
   "    <airdry>0.119</airdry>\n"
   "    <ll15>0.149</ll15>\n"
   "    <dul>0.297</dul>\n"
   "    <sat>0.361</sat>\n"
   "    <sw>0.297</sw>\n"
   "    <swcon>0.3</swcon>\n"
   "    <bd>1.694</bd>\n"
   "    <oc>0.45</oc>\n"
   "    <ec>0.3</ec>\n"
   "    <esp>15</esp>\n"
   "    <ph>8.8</ph>\n"
   "    <fbiom>0.02</fbiom>\n"
   "    <finert>0.6</finert>\n"
   "    <no3>4</no3>\n"
   "    <nh4>0.18</nh4>\n"
   "    <crop name=\"wheat\">\n"
   "      <ll>0.149</ll>\n"
   "      <kl>0.06</kl>\n"
   "      <xf>1</xf>\n"
   "      <kl0>0.08</kl0>\n"
   "    </crop>\n"
   "  </layer>\n"
   "\n"
   "  <layer name=\"3\">\n"
   "    <thickness>150</thickness>\n"
   "    <airdry>0.149</airdry>\n"
   "    <ll15>0.149</ll15>\n"
   "    <dul>0.297</dul>\n"
   "    <sat>0.361</sat>\n"
   "    <sw>0.297</sw>\n"
   "    <swcon>0.3</swcon>\n"
   "    <bd>1.694</bd>\n"
   "    <oc>0.45</oc>\n"
   "    <ec>0.4</ec>\n"
   "    <esp>25</esp>\n"
   "    <ph>8.8</ph>\n"
   "    <fbiom>0.02</fbiom>\n"
   "    <finert>0.6</finert>\n"
   "    <no3>2</no3>\n"
   "    <nh4>0.18</nh4>\n"
   "    <crop name=\"wheat\">\n"
   "      <ll>0.149</ll>\n"
   "      <kl>0.04</kl>\n"
   "      <xf>1</xf>\n"
   "      <kl0>0.06</kl0>\n"
   "    </crop>\n"
   "  </layer>\n"
   "\n"
   "  <layer name=\"4\">\n"
   "    <thickness>150</thickness>\n"
   "    <airdry>0.144</airdry>\n"
   "    <ll15>0.144</ll15>\n"
   "    <dul>0.285</dul>\n"
   "    <sat>0.377</sat>\n"
   "    <sw>0.285</sw>\n"
   "    <swcon>0.3</swcon>\n"
   "    <bd>1.65</bd>\n"
   "    <oc>0.3</oc>\n"
   "    <ec>0.5</ec>\n"
   "    <esp>35</esp>\n"
   "    <ph>8.9</ph>\n"
   "    <fbiom>0.015</fbiom>\n"
   "    <finert>0.8</finert>\n"
   "    <no3>1</no3>\n"
   "    <nh4>0.18</nh4>\n"
   "    <crop name=\"wheat\">\n"
   "      <ll>0.144</ll>\n"
   "      <kl>0.04</kl>\n"
   "      <xf>1</xf>\n"
   "      <kl0>0.06</kl0>\n"
   "    </crop>\n"
   "  </layer>\n"
   "\n"
   "  <layer name=\"5\">\n"
   "    <thickness>150</thickness>\n"
   "    <airdry>0.144</airdry>\n"
   "    <ll15>0.144</ll15>\n"
   "    <dul>0.285</dul>\n"
   "    <sat>0.377</sat>\n"
   "    <sw>0.285</sw>\n"
   "    <swcon>0.3</swcon>\n"
   "    <bd>1.65</bd>\n"
   "    <oc>0.3</oc>\n"
   "    <ec>0.6</ec>\n"
   "    <esp>45</esp>\n"
   "    <ph>8.9</ph>\n"
   "    <fbiom>0.015</fbiom>\n"
   "    <finert>0.8</finert>\n"
   "    <no3>0.5</no3>\n"
   "    <nh4>0.18</nh4>\n"
   "    <crop name=\"wheat\">\n"
   "      <ll>0.144</ll>\n"
   "      <kl>0.02</kl>\n"
   "      <xf>1</xf>\n"
   "      <kl0>0.06</kl0>\n"
   "    </crop>\n"
   "  </layer>\n"
   "\n"
   "  <layer name=\"6\">\n"
   "    <thickness>300</thickness>\n"
   "    <airdry>0.164</airdry>\n"
   "    <ll15>0.164</ll15>\n"
   "    <dul>0.261</dul>\n"
   "    <sat>0.378</sat>\n"
   "    <sw>0.261</sw>\n"
   "    <swcon>0.3</swcon>\n"
   "    <bd>1.647</bd>\n"
   "    <oc>0.3</oc>\n"
   "    <ec>0.7</ec>\n"
   "    <esp>55</esp>\n"
   "    <ph>8.9</ph>\n"
   "    <fbiom>0.01</fbiom>\n"
   "    <finert>0.9</finert>\n"
   "    <no3>0.5</no3>\n"
   "    <nh4>0.13</nh4>\n"
   "    <crop name=\"wheat\">\n"
   "      <ll>0.164</ll>\n"
   "      <kl>0.02</kl>\n"
   "      <xf>0.777</xf>\n"
   "      <kl0>0.04</kl0>\n"
   "    </crop>\n"
   "  </layer>\n"
   "\n"
   "  <layer name=\"7\">\n"
   "    <thickness>300</thickness>\n"
   "    <airdry>0.26</airdry>\n"
   "    <ll15>0.26</ll15>\n"
   "    <dul>0.261</dul>\n"
   "    <sat>0.375</sat>\n"
   "    <sw>0.261</sw>\n"
   "    <swcon>0.3</swcon>\n"
   "    <bd>1.657</bd>\n"
   "    <oc>0.25</oc>\n"
   "    <ec>0.8</ec>\n"
   "    <esp>65</esp>\n"
   "    <ph>8.9</ph>\n"
   "    <fbiom>0.01</fbiom>\n"
   "    <finert>0.9</finert>\n"
   "    <no3>0.5</no3>\n"
   "    <nh4>0.13</nh4>\n"
   "    <crop name=\"wheat\">\n"
   "      <ll>0.26</ll>\n"
   "      <kl>0.01</kl>\n"
   "      <xf>0.777</xf>\n"
   "      <kl0>0.03</kl0>\n"
   "    </crop>\n"
   "  </layer>\n"
   "\n"
   "  <layer name=\"8\">\n"
   "    <thickness>300</thickness>\n"
   "    <airdry>0.26</airdry>\n"
   "    <ll15>0.26</ll15>\n"
   "    <dul>0.261</dul>\n"
   "    <sat>0.367</sat>\n"
   "    <sw>0.261</sw>\n"
   "    <swcon>0.3</swcon>\n"
   "    <bd>1.677</bd>\n"
   "    <oc>0.25</oc>\n"
   "    <ec>0.9</ec>\n"
   "    <esp>75</esp>\n"
   "    <ph>8.9</ph>\n"
   "    <fbiom>0.01</fbiom>\n"
   "    <finert>0.9</finert>\n"
   "    <no3>0.5</no3>\n"
   "    <nh4>0.11</nh4>\n"
   "    <crop name=\"wheat\">\n"
   "      <ll>0.26</ll>\n"
   "      <kl>0.01</kl>\n"
   "      <xf>0.645</xf>\n"
   "      <kl0>0.02</kl0>\n"
   "    </crop>\n"
   "  </layer>\n"
   "\n"
   "</Soil>";

Soil* soil;

//---------------------------------------------------------------------------
// Perform a comparison of 2 vectors allowing for a tolerance.
// NB: floating point numbers are not always represented exactly by a computer.
//---------------------------------------------------------------------------
template<class T>
bool vectorsAreEqual(const vector<T>& lhs, const T rhs[])
   {
   if (lhs.size() != 8)
      return false;
   for (unsigned i = 0; i != lhs.size(); i++)
      {
      if (abs(lhs[i] - rhs[i]) > 0.00001)
         return false;
      }
   return true;
   }
bool doublesAreEqual(double value1, double value2)
   {
   return (abs(value1 - value2) < 0.00001);
   }
//---------------------------------------------------------------------------
// Setup the test environment
//---------------------------------------------------------------------------
void setUp(void)
   {
   soil = new Soil(SOIL_DATA);
   }
//---------------------------------------------------------------------------
// Tear down the test environment
//---------------------------------------------------------------------------
void tearDown(void)
   {
   delete soil;
   }
//---------------------------------------------------------------------------
// Test the write apsim soil file method
//---------------------------------------------------------------------------
void testWriteApsimFile(void)
   {
   setUp();

   ostringstream contents;
   soil->writeApsimPar(contents);

   BOOST_CHECK(contents.str() ==
      "[soil.soilwat2.parameters]\n"
      "   diffus_const = 220   ! coeffs for unsaturated water flow\n"
      "   diffus_slope = 22\n"
      "   cn2_bare     = 68    ! bare soil runoff curve number\n"
      "   cn_red       = 20    ! potetial reduction in curve number due to residue\n"
      "   cn_cov       = 0.8   ! cover for maximum reduction in curve number\n"
      "   salb         = 0.13  ! bare soil albedo\n"
      "   cona         = 2     ! stage 2 evap coef.\n"
      "   u            = 2     ! stage 1 soil evaporation coefficient (mm)\n"
      "\n"
      "   dlayer  =      100     150     150     150     150     300     300     300   ! layer thickness mm soil\n"
      "   air_dry =    0.042   0.119   0.149   0.144   0.144   0.164   0.260   0.260   ! air dry mm water/ mm soil\n"
      "   ll15    =    0.084   0.149   0.149   0.144   0.144   0.164   0.260   0.260   ! lower limit mm water/mm soil\n"
      "   dul     =    0.296   0.297   0.297   0.285   0.285   0.261   0.261   0.261   ! drained upper limit mm water/mm soil\n"
      "   sat     =    0.353   0.361   0.361   0.377   0.377   0.378   0.375   0.367   ! saturation mm water/mm soil\n"
      "   sw      =    0.295   0.297   0.297   0.285   0.285   0.261   0.261   0.261   ! starting soil water mm water/mm soil\n"
      "   swcon   =    0.700   0.300   0.300   0.300   0.300   0.300   0.300   0.300   ! drainage coefficient\n"
      "   bd      =    1.716   1.694   1.694   1.650   1.650   1.647   1.657   1.677   ! bulk density gm dry soil/cc moist soil\n"
      "\n"
      "[soil.soiln2.parameters]\n"
      "   root_cn      = 45     ! C:N ratio of initial root residues\n"
      "   root_wt      = 1500   ! root residues as biomass (kg/ha)\n"
      "   soil_cn      = 12.5   ! C:N ratio of soil\n"
      "   enr_a_coeff  = 7.4\n"
      "   enr_b_coeff  = 0.2\n"
      "   profile_reduction =  off\n"
      "\n"
      "   oc      =    1.050   0.450   0.450   0.300   0.300   0.300   0.250   0.250   ! Soil Organic Carbon\n"
      "   ph      =    8.500   8.800   8.800   8.900   8.900   8.900   8.900   8.900   ! pH of soil\n"
      "   fbiom   =    0.025   0.020   0.020   0.015   0.015   0.010   0.010   0.010   ! Organic C Biomass Fraction\n"
      "   finert  =    0.400   0.600   0.600   0.800   0.800   0.900   0.900   0.900   ! Inert Organic C Fraction\n"
      "   no3ppm  =    5.000   4.000   2.000   1.000   0.500   0.500   0.500   0.500   ! Nitrate Concentration\n"
      "   nh4ppm  =    1.240   0.180   0.180   0.180   0.180   0.130   0.130   0.110   ! Ammonium Concentration\n"

      "\n"

      "[soil.wheat.parameters]\n"

      "   ll      =    0.084   0.149   0.149   0.144   0.144   0.164   0.260   0.260\n"
      "   kl      =    0.060   0.060   0.040   0.040   0.020   0.020   0.010   0.010\n"
      "   xf      =    1.000   1.000   1.000   1.000   1.000   0.777   0.777   0.645\n");



   tearDown();
   }
//---------------------------------------------------------------------------
// Test the getThickness method.
//---------------------------------------------------------------------------
void testGetThickness(void)
   {
   setUp();
   unsigned correctThickness[8] = {100,150,150,150,150,300,300,300};
   BOOST_CHECK(vectorsAreEqual<unsigned>(soil->thickness(), correctThickness));
   tearDown();
   }
//---------------------------------------------------------------------------
// Test the getCrops method works
//---------------------------------------------------------------------------
void testGetCrops(void)
   {
   setUp();
   vector<string> crops = soil->crops();
   BOOST_CHECK(crops.size() == 1);
   BOOST_CHECK(crops[0] == "wheat");
   tearDown();
   }
//---------------------------------------------------------------------------
// Test that we can get crop data
//---------------------------------------------------------------------------
void testGetCropData(void)
   {
   static double correctLL[8] = {0.084, 0.149, 0.149, 0.144, 0.144, 0.164, 0.260, 0.260};
   static double correctKL[8] = {0.060, 0.060, 0.040, 0.040, 0.020, 0.020, 0.010, 0.010};
   static double correctXF[8] = {1.000, 1.000, 1.000, 1.000, 1.000, 0.777, 0.777, 0.645};

   setUp();
   vector<double> ll = soil->ll("wheat");
   vector<double> kl = soil->kl("wheat");
   vector<double> xf = soil->xf("wheat");
   BOOST_CHECK(ll.size() == 8);
   BOOST_CHECK(kl.size() == 8);
   BOOST_CHECK(xf.size() == 8);
   BOOST_CHECK(vectorsAreEqual<double>(ll, correctLL));
   BOOST_CHECK(vectorsAreEqual<double>(kl, correctKL));
   BOOST_CHECK(vectorsAreEqual<double>(xf, correctXF));
   tearDown();
   }
//---------------------------------------------------------------------------
// Test that we can create a soil.
//---------------------------------------------------------------------------
void testCreateSoil(void)
   {
   Soil soil;
   soil.setName("Test soil");
   soil.setRegion("Test region");
   soil.setCona(2);
   soil.setDiffusConst(220);
   soil.setDiffusSlope(22);
   soil.setU(2);
   soil.setSalb(0.13);
   soil.setCn2Bare(68);
   soil.setCnRed(20);
   soil.setCnCov(0.8);
   soil.setCnCanopyFact(1.0);

   soil.setRootCN(45);
   soil.setRootWt(1500);
   soil.setSoilCN(12.5);
   soil.setEnrAcoeff(7.4);
   soil.setEnrBcoeff(0.2);

   static const unsigned thickness[8] = {     100,     150,     150,     150,     150,     300,     300,     300};
   static const double airdry[8]  =     {   0.042,   0.119,   0.149,   0.144,   0.144,   0.164,   0.260,   0.260};
   static const double ll15[8]    =     {   0.084,   0.149,   0.149,   0.144,   0.144,   0.164,   0.260,   0.260};
   static const double dul[8]     =     {   0.296,   0.297,   0.297,   0.285,   0.285,   0.261,   0.261,   0.261};
   static const double sat[8]     =     {   0.353,   0.361,   0.361,   0.377,   0.377,   0.378,   0.375,   0.367};
   static const double sw[8]      =     {   0.295,   0.297,   0.297,   0.285,   0.285,   0.261,   0.261,   0.261};
   static const double swcon[8]   =     {   0.700,   0.300,   0.300,   0.300,   0.300,   0.300,   0.300,   0.300};
   static const double bd[8]      =     {   1.716,   1.694,   1.694,   1.650,   1.650,   1.647,   1.657,   1.677};
   static const double oc[8]      =     {   1.050,   0.450,   0.450,   0.300,   0.300,   0.300,   0.250,   0.250};
   static const double ec[8]      =     {   0.000,   0.300,   0.400,   0.500,   0.600,   0.700,   0.800,   0.900};
   static const double esp[8]     =     {   5.000,  15.000,  25.000,  35.000,  45.000,  55.000,  65.000,  75.000};
   static const double ph[8]      =     {   8.500,   8.800,   8.800,   8.900,   8.900,   8.900,   8.900,   8.900};
   static const double fbiom[8]   =     {   0.025,   0.020,   0.020,   0.015,   0.015,   0.010,   0.010,   0.010};
   static const double finert[8]  =     {   0.400,   0.600,   0.600,   0.800,   0.800,   0.900,   0.900,   0.900};
   static const double no3ppm[8]  =     {   5.000,   4.000,   2.000,   1.000,   0.500,   0.500,   0.500,   0.500};
   static const double nh4ppm[8]  =     {   1.240,   0.180,   0.180,   0.180,   0.180,   0.130,   0.130,   0.110};

   static const double ll[8]      =     {   0.084,   0.149,   0.149,   0.144,   0.144,   0.164,   0.260,   0.260};

   static const double kl[8]      =     {   0.060,   0.060,   0.040,   0.040,   0.020,   0.020,   0.010,   0.010};
   static const double xf[8]      =     {   1.000,   1.000,   1.000,   1.000,   1.000,   0.777,   0.777,   0.645};

   static const double kl0[8]     =     {   0.080,   0.080,   0.060,   0.060,   0.060,   0.040,   0.030,   0.020};

   soil.setThickness(vector<unsigned>(thickness, thickness+8));
   soil.setAirdry(vector<double>(airdry, airdry+8));
   soil.setLl15(vector<double>(ll15, ll15+8));
   soil.setDul(vector<double>(dul, dul+8));
   soil.setSat(vector<double>(sat, sat+8));
   soil.setSw(vector<double>(sw, sw+8));
   soil.setSwcon(vector<double>(swcon, swcon+8));
   soil.setBd(vector<double>(bd, bd+8));
   soil.setOc(vector<double>(oc, oc+8));
   soil.setEc(vector<double>(ec, ec+8));
   soil.setEsp(vector<double>(esp, esp+8));
   soil.setPh(vector<double>(ph, ph+8));
   soil.setFbiom(vector<double>(fbiom, fbiom+8));
   soil.setFinert(vector<double>(finert, finert+8));
   soil.setNo3(vector<double>(no3ppm, no3ppm+8));
   soil.setNh4(vector<double>(nh4ppm, nh4ppm+8));
   soil.setll("wheat", vector<double>(ll, ll+8));
   soil.setkl("wheat", vector<double>(kl, kl+8));
   soil.setxf("wheat", vector<double>(xf, xf+8));
   soil.setkl0("wheat", vector<double>(kl0, kl0+8));

   ostringstream out;
   soil.write(out);
   BOOST_CHECK(out.str() == SOIL_DATA);
   }
//---------------------------------------------------------------------------
// Test the kl modification
//---------------------------------------------------------------------------
void testKlModification(void)
   {
   setUp();
   vector<double> kl = soil->kl("wheat", true);
   BOOST_CHECK(kl.size() == 8);
   BOOST_CHECK(kl[0] == 0.08);
   BOOST_CHECK(kl[1] == 0.08);
   BOOST_CHECK(kl[2] == 0.06);
   BOOST_CHECK(kl[3] == 0.06);
   BOOST_CHECK(kl[4] == 0.06);
   BOOST_CHECK(doublesAreEqual(kl[5], 0.04 * (2.06 / (1+pow(2.0, 0.7)) - 0.351)));
   BOOST_CHECK(doublesAreEqual(kl[6], 0.03 * (2.06 / (1+pow(2.0, 0.8)) - 0.351)));
   BOOST_CHECK(doublesAreEqual(kl[7], 0.02 * (2.06 / (1+pow(2.0, 0.9)) - 0.351)));
   tearDown();
   }
//---------------------------------------------------------------------------
// Test the xf modification
//---------------------------------------------------------------------------
void testXfModification(void)
   {
   setUp();
   vector<double> xf = soil->xf("wheat", true);
   BOOST_CHECK(xf.size() == 8);
   BOOST_CHECK(xf[0] == 1.0);
   BOOST_CHECK(xf[1] == 1.0);
   BOOST_CHECK(doublesAreEqual(xf[2], 1.0 - (25.0-15.0)/35.0));
   BOOST_CHECK(doublesAreEqual(xf[3], 1.0 - (35.0-15.0)/35.0));
   BOOST_CHECK(doublesAreEqual(xf[4], 1.0 - (45.0-15.0)/35.0));
   BOOST_CHECK(xf[5] == 0.0);
   BOOST_CHECK(xf[6] == 0.0);
   BOOST_CHECK(xf[7] == 0.0);
   tearDown();
   }
//---------------------------------------------------------------------------
// Test autocorrect.
//---------------------------------------------------------------------------
void testAutoCorrect(void)
   {
   setUp();

   // new structure.
   static const double airdry[8] =   {0.03,    0.08,    0.13,   0.13,   0.144,   0.15,   0.26,   0.26};
   static const double ll15[8] =     {0.06,    0.088,   0.13,   0.13,   0.144,   0.15,   0.26,   0.26};
   static const double dul[8] =      {0.296,   0.297,   0.297,  0.285,  0.285,   0.261,  0.27,   0.28};
   static const double sat[8] =      {0.353,   0.361,   0.361,  0.377,  0.377,   0.378,  0.375,  0.367};
   static const double ll[8] =       {0.084,   0.149,   0.149,  0.144,  0.144,   0.164,  0.26,   0.26};
   static const double sw[8] =       {0.03,    0.08,    0.13,   0.13,   0.144,   0.15,   0.27,   0.28};

   soil->setSw(vector<double>(sw, sw+8));
   soil->autoCorrect();
   BOOST_CHECK(vectorsAreEqual(soil->sw(), sw));
   BOOST_CHECK(vectorsAreEqual(soil->airdry(), airdry));
   BOOST_CHECK(vectorsAreEqual(soil->ll15(), ll15));
   BOOST_CHECK(vectorsAreEqual(soil->dul(), dul));
   BOOST_CHECK(vectorsAreEqual(soil->sat(), sat));
   BOOST_CHECK(vectorsAreEqual(soil->ll("wheat"), ll));

   tearDown();
   }

//---------------------------------------------------------------------------
// register all tests
//---------------------------------------------------------------------------
test_suite* testSoil(void)
   {
   test_suite* test= BOOST_TEST_SUITE("TestSoil");
   test->add(BOOST_TEST_CASE(&testWriteApsimFile));
   test->add(BOOST_TEST_CASE(&testGetThickness));
   test->add(BOOST_TEST_CASE(&testGetCrops));
   test->add(BOOST_TEST_CASE(&testGetCropData));
   test->add(BOOST_TEST_CASE(&testCreateSoil));
   test->add(BOOST_TEST_CASE(&testKlModification));
   test->add(BOOST_TEST_CASE(&testXfModification));
   test->add(BOOST_TEST_CASE(&testAutoCorrect));
   return test;
   }





