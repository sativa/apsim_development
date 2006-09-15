#include <stdio.h>
#include <math.h>
#include <vector>
#include <string>
#include <stdexcept>
#include <ComponentInterface/Component.h>
#include <ComponentInterface/datatypes.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ComponentInterface/MessageDataExt.h>

#include "PlantComponent.h"
#include "PlantLibrary.h"
#include "PlantInterface.h"
#include "Phase.h"
#include "LeafAppPhase.h"
#include "PlantPhenology.h"
#include "OutputVariable.h"

#include <iostream.h>

void LeafAppPhase::reset()
//=======================================================================================
   {
   pPhase::reset();
   final_leaf_no = 0.0;
   }

void LeafAppPhase::GetOutputs(std::vector <Output*> &Outputs)
//=======================================================================================
   {
   pPhase::GetOutputs(Outputs);
   OutputVariable *FLNVariable = new OutputVariable("final_leaf_no","","Final Leaf Number",final_leaf_no);
   Outputs.push_back(FLNVariable);


   }
void LeafAppPhase::readCultivarParameters(protocol::Component *s, const string & cultivar)
//=======================================================================================
   {
   pPhase::readCultivarParameters(s, cultivar);

   }

void LeafAppPhase::readSpeciesParameters (protocol::Component *s, vector<string> &sections)
//=======================================================================================
   {
   pPhase::readSpeciesParameters (s, sections);

   s->readParameter (sections
                    , "leaf_init_rate"
                    , leaf_init_rate
                    , 0.0, 100.0);
   s->readParameter (sections
                    , "leaf_no_min"
                    , leaf_no_min
                    , 0.0, 100.0);
   s->readParameter (sections
                    , "leaf_no_max"
                    , leaf_no_max
                    , 0.0, 100.0);
   s->readParameter (sections
                    , "leaf_no_seed"
                    , leaf_no_seed
                    , 0.0, 10.0);
   s->readParameter (sections
                    , "leaf_no_at_emerg"
                    , leaf_no_at_emerg
                    , 0.0, 10.0);

   node_app.search(s, sections,
                  "x_node_no_app", "", 0.0, 100.0,
                  "y_node_app_rate", "dd", 0.0, 1e3);

   }



void LeafAppPhase::updateTTTargets(PlantPhenology &parent, const environment_t &e)
//=======================================================================================
   {
   if (parent.inPhase("leaf_initiation"))
      {
      float tt_leaf_initiation = parent.TTTargetInPhase("leaf_initiation");
      final_leaf_no = leaf_no_seed + tt_leaf_initiation/leaf_init_rate;
      final_leaf_no = max(min(final_leaf_no, leaf_no_max),leaf_no_min);

      // Note the following currently gives a close estimate only
      float tt_node_formation = parent.ttInPhase("node_formation");
      float tt_required = node_app.integral(leaf_no_at_emerg,final_leaf_no);
      target = tt_required - tt_node_formation;
      }
   }

string LeafAppPhase::description() const
//=======================================================================================
   {
   return "   Stage duration determined by leaf appearance rates";
   } 

