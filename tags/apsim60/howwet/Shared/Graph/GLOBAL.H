#if !defined(__GLOBALS_H)
#define __GLOBALS_H

#if defined (BUILDING_GRAPH)
   #define GRAPH_CLASS_EXPORT _export
   #define GRAPH_FUNCTION_EXPORT _export
#else
   #define GRAPH_CLASS_EXPORT _import
   #define GRAPH_FUNCTION_EXPORT
#endif


#define MAX_MARKERS    8
enum Marker_type_enum {M_square,        M_circle,         M_up_triangle,
                       M_down_triangle, M_left_triangle,  M_right_triangle,
                       M_diamond,       M_none};

#define MAX_PEN_STYLES    6
enum Pen_style_enum {PS_Null,   PS_Solid,     PS_Dash,
                     PS_Dot,    PS_Dash_dot,  PS_Dash_dot_dot};

#define MAX_BRUSH_STYLES   8
enum Brush_style_enum  {BS_Null,        BS_Solid,      BS_BDiagonal,
                        BS_Cross,       BS_Diagcross,  BS_FDiagonal,
                        BS_Horizontal,  BS_Vertical};
#define MAX_COLOURS  10

enum Axis_type_enum  {x1_axis, y1_axis, x2_axis, y2_axis};

#include <consts.h>

#include <classlib\objstrm.h>
#include <graph\gpoint.h>
#ifdef INTERACTIVE
   #include <owl\owlpch.h>
   #include <graph\interact.h>
   #define HIT_TEST_MARGIN    5
#endif

#define DLL_name "graph11.dll"

#include <graph\ggdi.h>
#include <graph\dbase.h>

extern Marker_type_enum All_markers_list[MAX_MARKERS];
extern Pen_style_enum All_pen_styles_list[MAX_PEN_STYLES];
extern Brush_style_enum All_brush_styles_list[MAX_BRUSH_STYLES];
extern GColour All_colours_list[MAX_COLOURS];


#endif

