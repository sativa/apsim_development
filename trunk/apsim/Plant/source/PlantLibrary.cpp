//---------------------------------------------------------------------------

#include <general/pch.h>
#include <vcl.h>

static const char *Blank = " ";
extern const char *Mes_delimiter = ",";
static     const char *All_active_modules ="act_mods";
static     const char *First_active_module = "unk_mod";
static     const char *New_line = "/n";
static     const char *no_section = " ";
static     const char *ACTION_Create       = "create";
static     const char *ACTION_Init         = "init2";
static     const char *ACTION_Get_variable = "get";
static     const char *ACTION_Set_variable = "set";
static     const char *ACTION_Prepare      = "prepare";
static     const char *ACTION_Process      = "process";
static     const char *ACTION_Post         = "post";
static     const char *ACTION_Start        = "start";
static     const char *ACTION_Pause        = "pause";
static     const char *ACTION_Continue     = "continue";
static     const char *ACTION_Finish       = "finish";
static     const char *ACTION_End_Run      = "end_run";
static     const char *ACTION_Report       = "report";
static     const char *ACTION_Idle         = "idle";
static     const char *ACTION_Reset        = "reset";
static     const char *ACTION_Sum_Report   = "sum_report";
static     const char *ACTION_Till         = "tillage";
static     const char *ACTION_Sow          = "sow";
static     const char *ACTION_Harvest      = "harvest";
static     const char *ACTION_End_Crop     = "end_crop";
static     const char *ACTION_Kill_Crop    = "kill_crop";
static     const char *ACTION_Incorp_FOM   = "incorp_fom";
static     const char *ACTION_Add_Residue   = "add_residue";
static     const char *ACTION_Do_Decompose  = "do_decompose";
static     const char *ACTION_Decomposed    = "decomposed";
static     const char *ACTION_Initiate_Crop = "initiate_crop";
static     const char *ACTION_Add_Residue_P = "add_residue_p";
static     const char *ACTION_User_Init     = "init";
static     const char *ACTION_Incorp_FOM_P  = "incorp_fom_p";
static       const char *DATA_sender="sender";      // keyname used to identify the sender
                                             // of an event.  This data is included
                                             // with every event.


static        const char *DATA_sender_ID="sender_id";// ID used to identify the sender
                                             // of an event.  This data is included
                                             // with every event.


static        const char *EVENT_tick="tick";

static        const char *DATA_day = "day";             // data name for day of year
static        const char *DATA_year = "year";           // data name for year
static        const char *DATA_jday = "jday";           // data name for julian day number
static        const char *DATA_time = "time";           // data name for time of day
static        const char *DATA_timestep = "timestep";   // data name for timestep


static        const char *EVENT_new_solute="new_solute";

static        const char *DATA_new_solute_names = "solute_names";  // name of character array of data containing
                                                            // a list of solutes owned by the sender



static        const char *EVENT_newmet="newmet";
static        const char *DATA_radn = "radn";
static        const char *DATA_maxt = "maxt";
static        const char *DATA_mint = "mint";
static        const char *DATA_rain = "rain";
static        const char *DATA_vp = "vp";
static        const char *EVENT_irrigated= "irrigated";
static        const char *DATA_irrigate_amount = "amount";
static        const char *DATA_irrigate_time = "time";
static        const char *DATA_irrigate_duration = "duration";
static        const char *EVENT_Pot_Res_decomp="pot_decomp";
static        const char *DATA_Pot_C_decomp = "pot_c_decomp";
static        const char *DATA_Pot_N_decomp = "pot_n_decomp";
static        const char *DATA_Pot_P_decomp = "pot_p_decomp";
static        const char *EVENT_N_Balance="n_balance";
static        const char *DATA_NH4_transform_net = "nh4_transform_net";
static        const char *DATA_NO3_transform_net = "no3_transform_net";
static        const char *DATA_dlt_NH4_net = "dlt_nh4_net";
static        const char *DATA_dlt_NO3_net = "dlt_no3_net";
static        const char *EVENT_C_Balance="c_balance";
static        const char *DATA_dlt_OC = "dlt_oc";
static        const char *DATA_dlt_OM = "dlt_om";
static        const char *EVENT_Residue_added="residue_added";
static        const char *DATA_residue_type = "dlt_residue_type";
static        const char *DATA_dlt_residue_wt = "dlt_residue_wt";
static        const char *EVENT_Residue_removed="residue_removed";
static        const char *DATA_residue_removed_action = "residue_removed_action";
static        const char *DATA_dlt_residue_fraction = "dlt_residue_fraction";
static        const char *DATA_residue_incorp_fraction = "residue_incorp_fract";
static        const char *EVENT_Crop_chopped="crop_chopped";
static        const char *DATA_crop_type = "crop_type";
static        const char *DATA_dm_type = "dm_type";
static        const char *DATA_dlt_crop_dm = "dlt_crop_dm";
static        const char *DATA_fraction_to_Residue = "fraction_to_Residue";
static        const char *DATA_dlt_dm_n = "dlt_dm_n";
static        const char *DATA_dlt_dm_cnr = "dlt_dm_cnr";
static        const char *DATA_dlt_dm_p = "dlt_dm_p";
static        const char *DATA_dlt_dm_cpr = "dlt_dm_cpr";
static        const char *EVENT_New_Profile="new_profile";
static        const char *DATA_dlayer = "dlayer";
static        const char *DATA_air_dry_dep = "air_dry_dep";
static        const char *DATA_ll15_dep = "ll15_dep";
static        const char *DATA_dul_dep = "dul_dep";
static        const char *DATA_sat_dep = "sat_dep";
static        const char *DATA_sw_dep = "sw_dep";
static        const char *DATA_bd = "bd";

//---------------------------------------------------------------------------
